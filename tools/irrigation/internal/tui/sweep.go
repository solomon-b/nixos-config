package tui

import (
	"context"
	"fmt"
	"strings"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/solomon-b/nixos-config/tools/irrigation/internal/model"
)

// defaultAckWait is how long to give the mesh to acknowledge before reading
// back. Zigbee round trips through a coordinator and a repeater, so an
// immediate read would report the old state and call a working valve dead.
const defaultAckWait = 6 * time.Second

// SweepResult is one valve's verdict.
type SweepResult struct {
	Zone      string
	Acked     bool
	Volume    float64
	HasVolume bool
	Err       error
}

// Verdict describes what was actually established, in the narrowest terms the
// evidence supports.
func (r SweepResult) Verdict() (string, bool) {
	switch {
	case r.Err != nil:
		return "error", false
	case r.Acked && r.HasVolume && r.Volume > 0:
		return "opened, water seen", true
	case r.Acked:
		// The valve took the command and reported itself open. Whether water
		// moved is unknown: these flow meters do not register drip rates.
		return "opened, no flow reading", true
	default:
		return "no acknowledgement", false
	}
}

type sweepStepMsg struct {
	index  int
	result SweepResult
}

type sweepDoneMsg struct{}

// sweepStep exercises one zone: note the volume, open it, wait for the mesh to
// answer, read back, then shut it again.
func (m Model) sweepStep(i int, z model.Zone, d time.Duration) tea.Cmd {
	client := m.hass
	wait := m.ackWait
	if wait <= 0 {
		wait = defaultAckWait
	}
	device := strings.TrimPrefix(z.SwitchEntity, "switch.")
	volEntity := "sensor." + device + "_real_time_irrigation_volume"

	return func() tea.Msg {
		ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
		defer cancel()

		res := SweepResult{Zone: z.Name}
		before, hadBefore := client.Number(ctx, volEntity)

		if err := client.ManualRun(ctx, z.SensorEntity, d); err != nil {
			res.Err = err
			return sweepStepMsg{index: i, result: res}
		}

		select {
		case <-time.After(wait):
		case <-ctx.Done():
		}

		if state, err := client.State(ctx, z.SwitchEntity); err == nil {
			res.Acked = state == "on"
		} else {
			res.Err = err
		}

		if after, ok := client.Number(ctx, volEntity); ok && hadBefore {
			res.Volume = after - before
			res.HasVolume = true
		}

		// Always shut it again. A sweep must not leave water running.
		_ = client.Cancel(ctx, z.SensorEntity)
		return sweepStepMsg{index: i, result: res}
	}
}

// sweepView renders progress and the accumulated verdicts.
func (m Model) sweepView() string {
	var b strings.Builder
	b.WriteString(styleHeader.Render("VALVE SWEEP"))
	if m.sweepAt < len(m.sweepZones) {
		b.WriteString(styleDim.Render(fmt.Sprintf("  testing %d of %d: %s",
			m.sweepAt+1, len(m.sweepZones), m.sweepZones[m.sweepAt].Name)))
	} else {
		b.WriteString(styleDim.Render("  complete"))
	}
	b.WriteString("\n\n")

	for i, z := range m.sweepZones {
		switch {
		case i < len(m.sweepResults):
			r := m.sweepResults[i]
			verdict, ok := r.Verdict()
			style := styleBad
			if ok {
				style = styleRan
			}
			line := fmt.Sprintf("%-24s %s", z.Name, verdict)
			if r.Err != nil {
				line = fmt.Sprintf("%-24s %s", z.Name, r.Err.Error())
			} else if r.HasVolume && r.Volume > 0 {
				line = fmt.Sprintf("%-24s %s (+%.0f)", z.Name, verdict, r.Volume)
			}
			fmt.Fprintf(&b, "%s\n", style.Render(line))
		case i == m.sweepAt:
			fmt.Fprintf(&b, "%s\n", styleWarn.Render(fmt.Sprintf("%-24s testing…", z.Name)))
		default:
			fmt.Fprintf(&b, "%s\n", styleDim.Render(fmt.Sprintf("%-24s waiting", z.Name)))
		}
	}

	if m.sweepAt >= len(m.sweepZones) {
		b.WriteString("\n")
		b.WriteString(styleDim.Render("Every valve was shut again after testing. Press esc to return."))
	}
	return b.String()
}
