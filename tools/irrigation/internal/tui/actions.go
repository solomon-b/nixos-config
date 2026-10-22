package tui

import (
	"context"
	"fmt"
	"strconv"
	"strings"
	"sync"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/solomon-b/nixos-config/tools/irrigation/internal/hass"
	"github.com/solomon-b/nixos-config/tools/irrigation/internal/model"
)

// mode is what the keyboard currently means.
type mode int

const (
	modeNormal mode = iota
	modeDuration
	modeConfirm
	modeAdjust
)

// settleDelay is how long to wait before re-reading after actuating. Home
// Assistant has to act and the recorder has to persist the new state, so an
// immediate refresh would show the old value and look like a failed command.
const settleDelay = 4 * time.Second

type actionDoneMsg struct {
	verb string
	zone string
	err  error
}

type settleMsg struct{}

func (m *Model) toggleSelected(entity string) {
	if m.selected == nil {
		m.selected = map[string]bool{}
	}
	if m.selected[entity] {
		delete(m.selected, entity)
		return
	}
	m.selected[entity] = true
}

// targets is what the run and stop keys act on: every marked zone, or the one
// under the cursor when nothing is marked.
func (m Model) targets() []model.Zone {
	var out []model.Zone
	for _, z := range m.zones() {
		if m.selected[z.SensorEntity] {
			out = append(out, z)
		}
	}
	if len(out) > 0 {
		return out
	}
	if len(m.zones()) == 0 {
		return nil
	}
	return []model.Zone{m.zones()[m.cursor]}
}

func zoneNames(zs []model.Zone) string {
	names := make([]string, len(zs))
	for i, z := range zs {
		names[i] = z.Name
	}
	return strings.Join(names, ", ")
}

// runZones opens every zone at once. The operator chose concurrent over queued
// deliberately; supply pressure is their call, not this tool's.
func (m Model) runZones(zones []model.Zone, d time.Duration) tea.Cmd {
	client := m.hass
	return func() tea.Msg {
		return fanOut(zones, func(ctx context.Context, z model.Zone) error {
			return client.ManualRun(ctx, z.SensorEntity, d)
		}, fmt.Sprintf("started for %s", shortDur(d)))
	}
}

func (m Model) cancelZones(zones []model.Zone) tea.Cmd {
	client := m.hass
	return func() tea.Msg {
		return fanOut(zones, func(ctx context.Context, z model.Zone) error {
			return client.Cancel(ctx, z.SensorEntity)
		}, "stopped")
	}
}

// fanOut applies act to every zone concurrently and reports one combined
// result. A partial failure names the zones that failed rather than collapsing
// to a single error, because "2 of 3 valves opened" is the thing you need to
// know.
func fanOut(zones []model.Zone, act func(context.Context, model.Zone) error, verb string) actionDoneMsg {
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	errs := make([]error, len(zones))
	var wg sync.WaitGroup
	for i := range zones {
		wg.Add(1)
		go func(i int) {
			defer wg.Done()
			errs[i] = act(ctx, zones[i])
		}(i)
	}
	wg.Wait()

	var failed []string
	for i, err := range errs {
		if err != nil {
			failed = append(failed, zones[i].Name)
		}
	}
	switch {
	case len(failed) == len(zones):
		return actionDoneMsg{err: fmt.Errorf("%s failed for %s", verb, strings.Join(failed, ", "))}
	case len(failed) > 0:
		return actionDoneMsg{err: fmt.Errorf("%d of %d %s; failed: %s",
			len(zones)-len(failed), len(zones), verb, strings.Join(failed, ", "))}
	case len(zones) == 1:
		return actionDoneMsg{zone: zones[0].Name, verb: verb}
	default:
		return actionDoneMsg{zone: fmt.Sprintf("%d zones", len(zones)), verb: verb}
	}
}

func settle() tea.Cmd {
	return tea.Tick(settleDelay, func(time.Time) tea.Msg { return settleMsg{} })
}

// shortDur renders a duration the way a person says it: "10m", "1h", "1h30m".
// time.Duration's own String gives "1h0m0s", which is noise.
func shortDur(d time.Duration) string {
	if d < time.Minute {
		return fmt.Sprintf("%ds", int(d.Seconds()))
	}
	h, m, sec := int(d.Hours()), int(d.Minutes())%60, int(d.Seconds())%60
	switch {
	case h == 0 && sec == 0:
		return fmt.Sprintf("%dm", m)
	case h == 0:
		return fmt.Sprintf("%dm%ds", m, sec)
	case m == 0:
		return fmt.Sprintf("%dh", h)
	default:
		return fmt.Sprintf("%dh%dm", h, m)
	}
}

// canActuate reports whether the run and stop keys should do anything. Without
// a token the tool stays strictly read-only.
func (m Model) canActuate() bool { return m.hass != nil }

// isRunning reports whether irrigation_unlimited currently has the zone on.
func isRunning(status string) bool { return status == "on" }

var _ = hass.MaxDuration

// defaultDuration pre-fills the prompt. Ten minutes is a spot-watering
// amount: useful on its own, harmless if fired by accident.
const defaultDuration = "10m"

// defaultSweepDuration only has to prove a valve moves. Long enough for the
// mesh to answer, short enough to walk the whole fleet quickly.
const defaultSweepDuration = "30s"

var errNoToken = fmt.Errorf("read-only: no Home Assistant token, so zones cannot be actuated")

// updatePrompt handles keys while a prompt owns the keyboard.
func (m Model) updatePrompt(msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	switch m.mode {
	case modeDuration:
		switch msg.Type {
		case tea.KeyEsc:
			m.mode = modeNormal
			m.sweepPending = false
			return m, nil
		case tea.KeyBackspace:
			if len(m.input) > 0 {
				m.input = m.input[:len(m.input)-1]
			}
			return m, nil
		case tea.KeyEnter:
			d, err := hass.ParseDuration(m.input)
			if err == nil {
				err = hass.Validate(d)
			}
			if err != nil {
				m.actionErr = err
				return m, nil
			}
			m.pending = d
			m.actionErr = nil
			m.mode = modeConfirm
			return m, nil
		case tea.KeyRunes, tea.KeySpace:
			m.input += string(msg.Runes)
			return m, nil
		}
		return m, nil

	case modeAdjust:
		switch msg.Type {
		case tea.KeyEsc:
			m.mode = modeNormal
			return m, nil
		case tea.KeyBackspace:
			if len(m.input) > 0 {
				m.input = m.input[:len(m.input)-1]
			}
			return m, nil
		case tea.KeyEnter:
			targets := m.targets()
			m.mode = modeNormal
			m.selected = nil
			m.busy = true
			if strings.EqualFold(strings.TrimSpace(m.input), "reset") {
				return m, m.adjustZones(targets, 0, true)
			}
			pct, err := strconv.Atoi(strings.TrimSpace(m.input))
			if err != nil || pct < 1 || pct > 500 {
				m.busy = false
				m.mode = modeAdjust
				m.actionErr = fmt.Errorf("enter a percentage between 1 and 500, or \"reset\"")
				return m, nil
			}
			return m, m.adjustZones(targets, pct, false)
		case tea.KeyRunes, tea.KeySpace:
			m.input += string(msg.Runes)
			return m, nil
		}
		return m, nil

	case modeConfirm:
		switch msg.String() {
		case "y", "Y", "enter":
			m.mode = modeNormal
			if m.sweepPending {
				m.sweepPending = false
				m.sweeping = true
				m.sweepZones = append([]model.Zone(nil), m.zones()...)
				m.sweepResults = nil
				m.sweepAt = 0
				m.sweepDur = m.pending
				if len(m.sweepZones) == 0 {
					m.sweeping = false
					return m, nil
				}
				return m, m.sweepStep(0, m.sweepZones[0], m.sweepDur)
			}
			targets := m.targets()
			m.busy = true
			m.selected = nil
			return m, m.runZones(targets, m.pending)
		default:
			m.mode = modeNormal
			return m, nil
		}
	}
	return m, nil
}

// promptView renders whichever prompt is open.
func (m Model) promptView() string {
	if len(m.zones()) == 0 {
		return ""
	}
	switch m.mode {
	case modeDuration:
		head := "Run " + zoneNames(m.targets())
		if m.sweepPending {
			head = fmt.Sprintf("Sweep all %d zones", len(m.zones()))
		}
		return stylePane.Render(fmt.Sprintf("%s\n\nDuration  %s█\n\n%s",
			styleHeader.Render(head),
			m.input,
			styleDim.Render("enter continue · esc cancel · accepts 10m, 90s, 1h30m, 00:10:00")))
	case modeAdjust:
		return stylePane.Render(fmt.Sprintf("%s\n\nRun time  %s%%█\n\n%s",
			styleHeader.Render("Adjust "+zoneNames(m.targets())),
			m.input,
			styleDim.Render("enter apply · esc cancel · 150 means half again as long · \"reset\" reverts")))
	case modeConfirm:
		if m.sweepPending {
			return stylePane.Render(fmt.Sprintf("%s\n\n%s\n\n%s",
				styleWarn.Render("Test every valve?"),
				fmt.Sprintf("%d zones, %s each, one at a time.\nEach is shut again immediately.",
					len(m.zones()), shortDur(m.pending)),
				styleDim.Render("y confirm · any other key cancel")))
		}
		targets := m.targets()
		head := "Open the valve?"
		if len(targets) > 1 {
			head = fmt.Sprintf("Open %d valves at once?", len(targets))
		}
		return stylePane.Render(fmt.Sprintf("%s\n\n%s\n\n%s",
			styleWarn.Render(head),
			fmt.Sprintf("%s\nfor %s each", styleHeader.Render(zoneNames(targets)), shortDur(m.pending)),
			styleDim.Render("y confirm · any other key cancel")))
	}
	return ""
}

type tickMsg time.Time

func tickCmd() tea.Cmd {
	return tea.Tick(time.Second, func(t time.Time) tea.Msg { return tickMsg(t) })
}


// defaultAdjust pre-fills the adjustment prompt. 150% is the shape of a
// heatwave response; 100 is the same as reverting.
const defaultAdjust = "150"

func (m Model) toggleAutoClose(v model.Valve) tea.Cmd {
	client := m.hass
	want := !v.AutoClose
	return func() tea.Msg {
		ctx, cancel := context.WithTimeout(context.Background(), 20*time.Second)
		defer cancel()
		err := client.SetSwitch(ctx, v.AutoCloseEntity, want)
		verb := "auto-shutoff off"
		if want {
			verb = "auto-shutoff on"
		}
		return actionDoneMsg{zone: v.Device, verb: verb, err: err}
	}
}

// adjustZones scales scheduled run time. This is runtime state in
// irrigation_unlimited, not an edit to configuration.yaml, so it is reversible
// and cannot leave the schedule file in a broken state.
func (m Model) adjustZones(zones []model.Zone, percent int, reset bool) tea.Cmd {
	client := m.hass
	verb := fmt.Sprintf("adjusted to %d%%", percent)
	if reset {
		verb = "reset to schedule"
	}
	return func() tea.Msg {
		return fanOut(zones, func(ctx context.Context, z model.Zone) error {
			if reset {
				return client.ResetAdjust(ctx, z.SensorEntity)
			}
			return client.AdjustPercent(ctx, z.SensorEntity, percent)
		}, verb)
	}
}
