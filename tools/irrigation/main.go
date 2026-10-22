package main

import (
	"context"
	"flag"
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/solomon-b/nixos-config/tools/irrigation/internal/hass"
	"github.com/solomon-b/nixos-config/tools/irrigation/internal/hasscfg"
	"github.com/solomon-b/nixos-config/tools/irrigation/internal/model"
	"github.com/solomon-b/nixos-config/tools/irrigation/internal/store"
	"github.com/solomon-b/nixos-config/tools/irrigation/internal/tui"
)

func main() {
	if err := run(); err != nil {
		fmt.Fprintln(os.Stderr, "irrigation:", err)
		os.Exit(1)
	}
}

func run() error {
	configHost := flag.String("config-host", "sower", "SSH host holding configuration.yaml")
	configPath := flag.String("config-path", "/mnt/home-assistant/configuration.yaml", "Path to configuration.yaml")
	zigbeeHost := flag.String("zigbee-host", "sower", "SSH host holding zigbee2mqtt devices.yaml")
	zigbeePath := flag.String("zigbee-path", "/mnt/zigbee2mqtt/devices.yaml", "Path to zigbee2mqtt devices.yaml")
	dbHost := flag.String("db-host", "accompaniment-of-shadows", "PostgreSQL host, optionally host:port")
	dsnFlag := flag.String("dsn", "", "Full DSN. Overrides credentials from configuration.yaml.")
	days := flag.Int("days", 90, "How much run history to load")
	haURL := flag.String("ha-url", "http://home-assistant.service.home.arpa", "Home Assistant base URL")
	secrets := flag.String("secrets", "", "Path to secrets.yaml. Defaults to searching upward from the working directory.")
	readOnly := flag.Bool("read-only", false, "Disable the run and stop keys even if a token is available")
	weather := flag.String("weather", "weather.forecast_home", "Weather entity for the forecast strip. Empty disables it.")
	flag.Parse()

	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	src := hasscfg.Source{Host: *configHost, Path: *configPath}
	devs := hasscfg.DeviceSource{Host: *zigbeeHost, Path: *zigbeePath}

	// Resolution order: --dsn, then IRRIGATION_DSN, then credentials from
	// configuration.yaml paired with a host this client can actually reach.
	dsn := *dsnFlag
	if dsn == "" {
		dsn = os.Getenv("IRRIGATION_DSN")
	}

	controllers, valves, warning, err := loadAll(ctx, src, devs, dsn, *dbHost, *days)
	if err != nil {
		return err
	}
	reload := func(ctx context.Context) ([]model.Controller, []model.Valve, string, error) {
		return loadAll(ctx, src, devs, dsn, *dbHost, *days)
	}

	// Actuation is opt-out rather than opt-in, but a missing token is not an
	// error: the tool stays useful read-only, which is most of what it does.
	var client *hass.Client
	if !*readOnly {
		path := *secrets
		if path == "" {
			if cwd, err := os.Getwd(); err == nil {
				path, _ = hass.FindSecrets(cwd)
			}
		}
		if path != "" {
			if tok, err := hass.TokenFromSops(ctx, path); err == nil {
				client = hass.New(*haURL, tok)
			} else {
				warning = strings.TrimSpace(warning + "  Read-only: " + err.Error())
			}
		} else {
			warning = strings.TrimSpace(warning + "  Read-only: no secrets.yaml found for the Home Assistant token.")
		}
	}

	since := time.Now().AddDate(0, 0, -*days)
	if oldest, err := oldestState(ctx, src, dsn, *dbHost); err == nil && oldest.After(since) {
		since = oldest
	}
	// A missing or broken forecast hides the strip rather than failing: this
	// is an irrigation tool, and the weather is context, not a dependency.
	var outlook []hass.Day
	if client != nil && *weather != "" {
		if f, ferr := client.Forecast(ctx, *weather); ferr == nil {
			outlook = f
		}
	}

	return tui.New(controllers, valves, warning, reload).
		WithSince(since).WithHass(client).WithForecast(outlook).
		WithUpcoming(tui.BuildUpcoming(controllers, time.Now(), 3, 20)).Run()
}

// oldestState finds the retention edge so the calendar can stop there.
func oldestState(ctx context.Context, src hasscfg.Source, dsn, dbHost string) (time.Time, error) {
	if dsn == "" {
		res := src.Load(ctx)
		if res.Err != nil && !res.FromCache {
			return time.Time{}, res.Err
		}
		dsn = res.Config.DSN(dbHost)
	}
	st, err := store.Open(ctx, dsn)
	if err != nil {
		return time.Time{}, err
	}
	defer st.Close()
	return st.OldestState(ctx)
}

func loadAll(ctx context.Context, src hasscfg.Source, devs hasscfg.DeviceSource, dsn, dbHost string, days int) ([]model.Controller, []model.Valve, string, error) {
	res := src.Load(ctx)
	if res.Err != nil && !res.FromCache {
		return nil, nil, "", res.Err
	}
	var warnings []string
	if res.FromCache {
		warnings = append(warnings, fmt.Sprintf("Using cached config from %s; %s is unreachable.",
			res.CachedAt.Format("01-02 15:04"), src.Host))
	}

	if dsn == "" {
		dsn = res.Config.DSN(dbHost)
	}

	st, err := store.Open(ctx, dsn)
	if err != nil {
		return nil, nil, "", err
	}
	defer st.Close()

	// Home Assistant may hold one valve's attributes under two entity id
	// prefixes: the friendly-name slug and the radio address. Fold the address
	// form onto the slug before any query uses device names. Every device, not
	// just the zone-assigned ones: a stray valve is split the same way.
	canonical := map[string]string{}
	if aliases, aerr := devs.LoadAliases(ctx); aerr == nil {
		for slug, addr := range aliases {
			canonical[addr] = slug
		}
	} else {
		warnings = append(warnings, "Zigbee device map unavailable: "+aerr.Error())
	}

	since := time.Now().AddDate(0, 0, -days)
	runs, err := st.Runs(ctx, since)
	if err != nil {
		return nil, nil, "", err
	}
	states, err := st.ZoneState(ctx)
	if err != nil {
		return nil, nil, "", err
	}

	volumes, verr := st.VolumeSeries(ctx, since, canonical)
	if verr != nil {
		return nil, nil, "", verr
	}

	var controllers []model.Controller
	var devices []string
	var ghosts, unmapped []string
	for _, c := range res.Config.Controllers {
		ctrl := model.Controller{Name: c.Name}
		live := false
		for _, z := range c.Zones {
			if _, ok := states[z.SensorEntity()]; ok {
				live = true
			}
			if z.SwitchEntity == "" {
				unmapped = append(unmapped, c.Name+"/"+z.Name)
			}
			sensor := z.SensorEntity()
			device := strings.TrimPrefix(z.SwitchEntity, "switch.")
			devices = append(devices, device)
			ctrl.Zones = append(ctrl.Zones, model.Zone{
				Name:         z.Name,
				SwitchEntity: z.SwitchEntity,
				SensorEntity: sensor,
				Index:        z.Index,
				Scheduled:    z.Scheduled,
				Specs:        specsOf(z),
				TodayMinutes: states[sensor].TodayMinutes,
				Status:       states[sensor].Status,
				Active:       states[sensor].ActiveRun(),
				// Volume comes from this zone's own valve, never from whatever
				// happened to be reporting during the same window.
				Runs: model.AttachVolume(runs[sensor], volumes[device]),
			})
		}
		// A controller declared in configuration.yaml with no entities in Home
		// Assistant was rejected at load time. irrigation_unlimited drops such
		// a controller silently, so the config and the running system disagree
		// and nothing says so.
		if !live {
			ghosts = append(ghosts, c.Name)
		}
		controllers = append(controllers, ctrl)
	}
	if len(unmapped) > 0 {
		warnings = append(warnings,
			"Zones with no entity_id: "+strings.Join(unmapped, ", ")+".")
	}
	if len(ghosts) > 0 {
		warnings = append(warnings,
			"In configuration.yaml but not running in Home Assistant: "+strings.Join(ghosts, ", ")+".")
	}

	// nil keys asks for every valve, not just the ones a zone drives. An
	// unassigned valve is invisible in the zone view, which is exactly where a
	// flat battery goes unnoticed.
	valves, err := st.Valves(ctx, nil, canonical)
	if err != nil {
		return nil, nil, "", err
	}

	zoneOf := map[string]string{}
	for _, c := range res.Config.Controllers {
		for _, z := range c.Zones {
			if d := strings.TrimPrefix(z.SwitchEntity, "switch."); d != "" {
				zoneOf[d] = z.Name
			}
		}
	}
	batteries, berr := st.BatteryHistory(ctx, canonical)
	for i := range valves {
		valves[i].Zone = zoneOf[valves[i].Device]
		if berr == nil {
			if tr, ok := model.FitBattery(batteries[valves[i].Device]); ok {
				valves[i].Trend, valves[i].HasTrend = tr, true
			}
		}
	}

	// A valve that used to report and has gone quiet is the real drift signal.
	// A valve that never exposed health entities is not, so it is not raised.
	if stale := tui.StaleValves(valves, time.Now()); len(stale) > 0 {
		// Deliberately not "valves silent": the recorder only proves Home
		// Assistant stopped writing state. The valve may be fine and the
		// ingestion path broken, which is a different problem in a different
		// place.
		warnings = append(warnings, "Link quality stale: "+strings.Join(stale, ", "))
	}

	return controllers, valves, strings.Join(warnings, "  "), nil
}

// specsOf converts the config's schedules into the shape the model projects
// forward from.
func specsOf(z hasscfg.Zone) []model.ScheduleSpec {
	out := make([]model.ScheduleSpec, 0, len(z.Schedules))
	for _, s := range z.Schedules {
		out = append(out, model.ScheduleSpec{
			Hour: s.Hour, Minute: s.Minute, Duration: s.Duration, Weekdays: s.Weekdays,
		})
	}
	return out
}
