// Package hass calls Home Assistant services to actuate irrigation zones.
//
// Everything here opens real valves on real hardware, so the API is
// deliberately narrow: start a zone for a bounded time, or stop one.
package hass

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

// MaxDuration caps a manual run. The longest scheduled run on this system is
// two hours, so anything beyond that is far more likely to be a typo than an
// intention, and the cost of the typo is a flooded garden.
const MaxDuration = 2 * time.Hour

type Client struct {
	BaseURL string
	Token   string
	HTTP    *http.Client
}

func New(baseURL, token string) *Client {
	return &Client{
		BaseURL: strings.TrimSuffix(baseURL, "/"),
		Token:   token,
		HTTP:    &http.Client{Timeout: 15 * time.Second},
	}
}

// ParseDuration accepts the shapes a person actually types: Go's own "10m" or
// "1h30m", a clock form "00:10:00" or "10:00", and a bare number meaning
// minutes.
func ParseDuration(s string) (time.Duration, error) {
	s = strings.TrimSpace(s)
	if s == "" {
		return 0, fmt.Errorf("empty duration")
	}

	if strings.Contains(s, ":") {
		parts := strings.Split(s, ":")
		if len(parts) > 3 {
			return 0, fmt.Errorf("too many parts in %q", s)
		}
		// Two parts read as MM:SS, three as HH:MM:SS.
		var h, m, sec int
		var err error
		switch len(parts) {
		case 2:
			if m, err = strconv.Atoi(parts[0]); err != nil {
				return 0, fmt.Errorf("bad minutes in %q", s)
			}
			if sec, err = strconv.Atoi(parts[1]); err != nil {
				return 0, fmt.Errorf("bad seconds in %q", s)
			}
		case 3:
			if h, err = strconv.Atoi(parts[0]); err != nil {
				return 0, fmt.Errorf("bad hours in %q", s)
			}
			if m, err = strconv.Atoi(parts[1]); err != nil {
				return 0, fmt.Errorf("bad minutes in %q", s)
			}
			if sec, err = strconv.Atoi(parts[2]); err != nil {
				return 0, fmt.Errorf("bad seconds in %q", s)
			}
		}
		return time.Duration(h)*time.Hour + time.Duration(m)*time.Minute + time.Duration(sec)*time.Second, nil
	}

	if n, err := strconv.Atoi(s); err == nil {
		return time.Duration(n) * time.Minute, nil
	}

	d, err := time.ParseDuration(s)
	if err != nil {
		return 0, fmt.Errorf("cannot read %q as a duration", s)
	}
	return d, nil
}

// FormatHMS renders a duration the way the manual_run service expects it.
func FormatHMS(d time.Duration) string {
	total := int(d.Seconds())
	return fmt.Sprintf("%02d:%02d:%02d", total/3600, (total%3600)/60, total%60)
}

// Validate rejects durations that are not worth sending.
func Validate(d time.Duration) error {
	if d <= 0 {
		return fmt.Errorf("duration must be positive")
	}
	if d > MaxDuration {
		return fmt.Errorf("duration %s exceeds the %s cap", d, MaxDuration)
	}
	return nil
}

// ManualRun opens a zone for the given time. Home Assistant closes it again
// when the time is up, so a lost connection here cannot leave water running.
func (c *Client) ManualRun(ctx context.Context, entityID string, d time.Duration) error {
	if err := Validate(d); err != nil {
		return err
	}
	return c.callService(ctx, "irrigation_unlimited", "manual_run", map[string]any{
		"entity_id": entityID,
		"time":      FormatHMS(d),
	})
}

// Cancel stops a zone that is currently running.
func (c *Client) Cancel(ctx context.Context, entityID string) error {
	return c.callService(ctx, "irrigation_unlimited", "cancel", map[string]any{
		"entity_id": entityID,
	})
}

func (c *Client) callService(ctx context.Context, domain, service string, payload map[string]any) error {
	body, err := json.Marshal(payload)
	if err != nil {
		return err
	}
	url := fmt.Sprintf("%s/api/services/%s/%s", c.BaseURL, domain, service)
	req, err := http.NewRequestWithContext(ctx, http.MethodPost, url, bytes.NewReader(body))
	if err != nil {
		return err
	}
	req.Header.Set("Authorization", "Bearer "+c.Token)
	req.Header.Set("Content-Type", "application/json")

	resp, err := c.HTTP.Do(req)
	if err != nil {
		return fmt.Errorf("%s.%s: %w", domain, service, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode < 200 || resp.StatusCode > 299 {
		buf := new(bytes.Buffer)
		buf.ReadFrom(resp.Body)
		return fmt.Errorf("%s.%s: %s: %s", domain, service, resp.Status,
			strings.TrimSpace(buf.String()))
	}
	return nil
}

// FindSecrets walks up from dir looking for the repository's secrets.yaml.
func FindSecrets(dir string) (string, error) {
	for {
		p := filepath.Join(dir, "secrets.yaml")
		if fi, err := statFile(p); err == nil && !fi {
			return p, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			return "", fmt.Errorf("no secrets.yaml found above the working directory")
		}
		dir = parent
	}
}

// TokenFromSops decrypts the Home Assistant token out of secrets.yaml.
func TokenFromSops(ctx context.Context, secretsPath string) (string, error) {
	cmd := exec.CommandContext(ctx, "sops", "-d",
		"--extract", `["home-assistant-access-token"]`, secretsPath)
	out, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("sops decrypt %s: %w", secretsPath, err)
	}
	tok := strings.TrimSpace(string(out))
	if tok == "" {
		return "", fmt.Errorf("home-assistant-access-token is empty in %s", secretsPath)
	}
	return tok, nil
}

// State reads one entity's current value. Used to confirm a valve actually
// acknowledged a command, rather than trusting that the service call returning
// 200 means the radio reached it.
func (c *Client) State(ctx context.Context, entityID string) (string, error) {
	url := fmt.Sprintf("%s/api/states/%s", c.BaseURL, entityID)
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return "", err
	}
	req.Header.Set("Authorization", "Bearer "+c.Token)

	resp, err := c.HTTP.Do(req)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()
	if resp.StatusCode == http.StatusNotFound {
		return "", fmt.Errorf("%s: no such entity", entityID)
	}
	if resp.StatusCode < 200 || resp.StatusCode > 299 {
		return "", fmt.Errorf("%s: %s", entityID, resp.Status)
	}
	var body struct {
		State string `json:"state"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
		return "", err
	}
	return body.State, nil
}

// Number reads an entity's state as a number.
func (c *Client) Number(ctx context.Context, entityID string) (float64, bool) {
	s, err := c.State(ctx, entityID)
	if err != nil {
		return 0, false
	}
	f, err := strconv.ParseFloat(s, 64)
	if err != nil {
		return 0, false
	}
	return f, true
}

// SetSwitch turns a switch entity on or off.
func (c *Client) SetSwitch(ctx context.Context, entityID string, on bool) error {
	service := "turn_off"
	if on {
		service = "turn_on"
	}
	return c.callService(ctx, "switch", service, map[string]any{"entity_id": entityID})
}

// AdjustPercent scales a zone's scheduled run time. Reversible, and it does not
// touch configuration.yaml: the adjustment lives in irrigation_unlimited's
// runtime state.
func (c *Client) AdjustPercent(ctx context.Context, entityID string, percent int) error {
	if percent < 1 || percent > 500 {
		return fmt.Errorf("percentage %d is outside 1-500", percent)
	}
	return c.callService(ctx, "irrigation_unlimited", "adjust_time", map[string]any{
		"entity_id":  entityID,
		"percentage": percent,
	})
}

// ResetAdjust reverts a zone to its configured run time.
func (c *Client) ResetAdjust(ctx context.Context, entityID string) error {
	return c.callService(ctx, "irrigation_unlimited", "adjust_time", map[string]any{
		"entity_id": entityID,
		"reset":     nil,
	})
}
