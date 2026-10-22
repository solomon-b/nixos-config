// Package hasscfg reads the facts this tool needs out of Home Assistant's
// configuration.yaml: the zone-to-valve mapping and the recorder credentials.
//
// The file carries Home Assistant tags such as !include, which a strict YAML
// parser rejects. Rather than resolve those tags, this package extracts the
// one top-level block it wants as text and parses that block alone. The
// irrigation_unlimited and recorder blocks contain no custom tags.
package hasscfg

import (
	"fmt"
	"net/url"
	"strconv"
	"strings"
	"time"

	"gopkg.in/yaml.v3"
)

type Zone struct {
	Name         string
	SwitchEntity string
	Index        int
	// Controller is the zero-based index of the controller owning this zone.
	Controller int
	// Scheduled is false when no schedule can ever fire, which is how a zone
	// is taken out of service for the season. It is read from the config, not
	// inferred from missing runs: a zone that is scheduled but has not run is
	// a different fact, and a more worrying one.
	Scheduled bool
	Schedules []Schedule
}

// Schedule is one recurring watering: a time of day, a run length, and the
// days it fires on. An empty Weekdays list means it never fires; an absent
// weekday key in the config means every day, and is stored here as all seven.
type Schedule struct {
	Hour     int
	Minute   int
	Duration time.Duration
	Weekdays []time.Weekday
}

// SensorEntity names the irrigation_unlimited binary sensor for this zone.
// Both controller and zone numbering are one-based in entity IDs, so the first
// zone of the first controller is c1_z1.
func (z Zone) SensorEntity() string {
	return fmt.Sprintf("binary_sensor.irrigation_unlimited_c%d_z%d", z.Controller+1, z.Index+1)
}

// Controller is one irrigation_unlimited controller and the zones it drives.
type Controller struct {
	Name  string
	Index int
	Zones []Zone
}

type Recorder struct {
	User     string
	Password string
	Database string
}

type Config struct {
	Controllers []Controller
	Recorder    Recorder
}

// AllZones flattens every controller's zones, for callers that do not care
// which controller a zone belongs to.
func (c Config) AllZones() []Zone {
	var out []Zone
	for _, ctrl := range c.Controllers {
		out = append(out, ctrl.Zones...)
	}
	return out
}

// ExtractBlock returns the top-level YAML block named key, including its
// header line. It ends at the next line that starts in column zero and is
// neither blank nor a comment.
func ExtractBlock(src, key string) (string, bool) {
	lines := strings.Split(src, "\n")
	start := -1
	for i, ln := range lines {
		if strings.HasPrefix(ln, key+":") {
			start = i
			break
		}
	}
	if start < 0 {
		return "", false
	}
	end := len(lines)
	for i := start + 1; i < len(lines); i++ {
		ln := lines[i]
		if strings.TrimSpace(ln) == "" || strings.HasPrefix(ln, "#") {
			continue
		}
		if !strings.HasPrefix(ln, " ") && !strings.HasPrefix(ln, "\t") {
			end = i
			break
		}
	}
	return strings.Join(lines[start:end], "\n"), true
}

func Parse(src string) (Config, error) {
	var cfg Config

	iuBlock, ok := ExtractBlock(src, "irrigation_unlimited")
	if !ok {
		return cfg, fmt.Errorf("no irrigation_unlimited block in configuration.yaml")
	}
	controllers, err := parseControllers(iuBlock)
	if err != nil {
		return cfg, fmt.Errorf("irrigation_unlimited: %w", err)
	}
	cfg.Controllers = controllers

	recBlock, ok := ExtractBlock(src, "recorder")
	if !ok {
		return cfg, fmt.Errorf("no recorder block in configuration.yaml")
	}
	rec, err := parseRecorder(recBlock)
	if err != nil {
		return cfg, fmt.Errorf("recorder: %w", err)
	}
	cfg.Recorder = rec

	return cfg, nil
}

func parseControllers(block string) ([]Controller, error) {
	var doc struct {
		IU struct {
			Controllers []struct {
				Name  string `yaml:"name"`
				Zones []struct {
					Name      string `yaml:"name"`
					EntityID  string `yaml:"entity_id"`
					Schedules []struct {
						Time     string `yaml:"time"`
						Duration string `yaml:"duration"`
						// A pointer distinguishes an absent weekday key, which
						// irrigation_unlimited treats as every day, from an
						// explicit empty list, which never fires.
						Weekday *[]string `yaml:"weekday"`
					} `yaml:"schedules"`
				} `yaml:"zones"`
			} `yaml:"controllers"`
		} `yaml:"irrigation_unlimited"`
	}
	if err := yaml.Unmarshal([]byte(block), &doc); err != nil {
		return nil, err
	}
	if len(doc.IU.Controllers) == 0 {
		return nil, fmt.Errorf("no controllers defined")
	}
	var out []Controller
	for ci, c := range doc.IU.Controllers {
		ctrl := Controller{Name: c.Name, Index: ci}
		for zi, z := range c.Zones {
			scheduled := false
			var scheds []Schedule
			for _, sc := range z.Schedules {
				days, _ := parseWeekdays(sc.Weekday)
				if len(days) > 0 {
					scheduled = true
				}
				h, m, err := parseClock(sc.Time)
				if err != nil {
					continue
				}
				dur, err := parseSpan(sc.Duration)
				if err != nil {
					continue
				}
				scheds = append(scheds, Schedule{Hour: h, Minute: m, Duration: dur, Weekdays: days})
			}
			ctrl.Zones = append(ctrl.Zones, Zone{
				Name:         z.Name,
				SwitchEntity: z.EntityID,
				Index:        zi,
				Controller:   ci,
				Scheduled:    scheduled,
				Schedules:    scheds,
			})
		}
		out = append(out, ctrl)
	}
	return out, nil
}

func parseRecorder(block string) (Recorder, error) {
	var doc struct {
		Recorder struct {
			DBURL string `yaml:"db_url"`
		} `yaml:"recorder"`
	}
	if err := yaml.Unmarshal([]byte(block), &doc); err != nil {
		return Recorder{}, err
	}
	if doc.Recorder.DBURL == "" {
		return Recorder{}, fmt.Errorf("db_url is empty; recorder may still be on SQLite")
	}
	u, err := url.Parse(doc.Recorder.DBURL)
	if err != nil {
		return Recorder{}, fmt.Errorf("parse db_url: %w", err)
	}
	pw, _ := u.User.Password()
	return Recorder{
		User:     u.User.Username(),
		Password: pw,
		Database: strings.TrimPrefix(u.Path, "/"),
	}, nil
}

// DSN builds a connection string for host, using credentials from the file.
// The host in db_url is Home Assistant's view from sower, so it is not reused.
//
// host may carry its own port, which is how an SSH tunnel endpoint such as
// "127.0.0.1:15432" is expressed. Only a bare host gets the default port.
func (c Config) DSN(host string) string {
	if !strings.Contains(host, ":") {
		host += ":5432"
	}
	return fmt.Sprintf("postgres://%s:%s@%s/%s?sslmode=disable",
		url.QueryEscape(c.Recorder.User),
		url.QueryEscape(c.Recorder.Password),
		host,
		c.Recorder.Database)
}

var weekdayNames = map[string]time.Weekday{
	"sun": time.Sunday, "mon": time.Monday, "tue": time.Tuesday, "wed": time.Wednesday,
	"thu": time.Thursday, "fri": time.Friday, "sat": time.Saturday,
}

// parseWeekdays turns the config's day list into weekdays. A nil list is an
// absent key, which irrigation_unlimited treats as every day.
func parseWeekdays(list *[]string) (days []time.Weekday, everyDay bool) {
	if list == nil {
		return []time.Weekday{time.Sunday, time.Monday, time.Tuesday, time.Wednesday,
			time.Thursday, time.Friday, time.Saturday}, true
	}
	for _, d := range *list {
		if wd, ok := weekdayNames[strings.ToLower(strings.TrimSpace(d))]; ok {
			days = append(days, wd)
		}
	}
	return days, false
}

// parseClock reads "06:00" into hour and minute.
func parseClock(s string) (int, int, error) {
	parts := strings.Split(strings.TrimSpace(s), ":")
	if len(parts) < 2 {
		return 0, 0, fmt.Errorf("want HH:MM, got %q", s)
	}
	h, err := strconv.Atoi(parts[0])
	if err != nil {
		return 0, 0, err
	}
	m, err := strconv.Atoi(parts[1])
	if err != nil {
		return 0, 0, err
	}
	return h, m, nil
}

// parseSpan reads a duration written as "02:00", meaning two hours.
func parseSpan(s string) (time.Duration, error) {
	parts := strings.Split(strings.TrimSpace(s), ":")
	if len(parts) < 2 {
		return 0, fmt.Errorf("want HH:MM, got %q", s)
	}
	h, err := strconv.Atoi(parts[0])
	if err != nil {
		return 0, err
	}
	m, err := strconv.Atoi(parts[1])
	if err != nil {
		return 0, err
	}
	var sec int
	if len(parts) > 2 {
		sec, _ = strconv.Atoi(parts[2])
	}
	return time.Duration(h)*time.Hour + time.Duration(m)*time.Minute + time.Duration(sec)*time.Second, nil
}
