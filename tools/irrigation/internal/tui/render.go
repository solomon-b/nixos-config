package tui

import (
	"fmt"
	"strings"
	"time"

	"github.com/charmbracelet/lipgloss"
	"github.com/solomon-b/nixos-config/tools/irrigation/internal/model"
)

// StaleAfter is how long a valve may stay quiet before it counts as a problem.
// Only valves that have reported before are judged. Several valves in this
// fleet expose no health entities at all, which is not a fault.
const StaleAfter = 48 * time.Hour

var (
	styleHeader   = lipgloss.NewStyle().Bold(true)
	styleSelected = lipgloss.NewStyle().Bold(true).Foreground(lipgloss.Color("42"))
	styleDim      = lipgloss.NewStyle().Foreground(lipgloss.Color("240"))
	styleWarn     = lipgloss.NewStyle().Foreground(lipgloss.Color("214"))
	styleBad      = lipgloss.NewStyle().Foreground(lipgloss.Color("196"))
	styleBox      = lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).Padding(0, 1)
)

const sparkRunes = "▁▂▃▄▅▆▇█"

// sparkline renders values as block characters, scaled to the largest value.
func sparkline(vals []float64) string {
	if len(vals) == 0 {
		return ""
	}
	max := vals[0]
	for _, v := range vals {
		if v > max {
			max = v
		}
	}
	if max <= 0 {
		return strings.Repeat("─", len(vals))
	}
	runes := []rune(sparkRunes)
	var b strings.Builder
	for _, v := range vals {
		idx := int(v / max * float64(len(runes)-1))
		if idx < 0 {
			idx = 0
		}
		b.WriteRune(runes[idx])
	}
	return b.String()
}

// zoneStatus classifies a zone for display.
//
// "disabled" and "idle" are deliberately separate. A disabled zone has no
// schedule that can fire, so silence is expected. An idle zone is scheduled
// but has no runs on record, which is either a young install or a problem.
func zoneStatus(z model.Zone, now time.Time) (string, lipgloss.Style) {
	if !z.Scheduled {
		return "disabled", styleDim
	}
	// Lateness comes from the configured schedule, not from the gaps between
	// runs. Ad-hoc runs make those gaps meaningless.
	if model.MissedSchedule(z.Specs, z.Runs, now) {
		return "LATE", styleBad
	}
	if len(z.Runs) == 0 {
		return "idle", styleWarn
	}
	return "ok", styleHeader
}

func humanSince(t time.Time, now time.Time) string {
	if t.IsZero() {
		return "never"
	}
	d := now.Sub(t)
	switch {
	case d < time.Hour:
		return fmt.Sprintf("%dm", int(d.Minutes()))
	case d < 48*time.Hour:
		return fmt.Sprintf("%dh", int(d.Hours()))
	default:
		return fmt.Sprintf("%dd", int(d.Hours()/24))
	}
}

// StaleValves names irrigation valves whose recorded state has not moved for a
// while.
//
// This measures what Home Assistant has recorded, which is not the same as
// whether the valve is alive. A battery resting at 100% writes no new rows, and
// a device can be publishing happily to zigbee2mqtt while Home Assistant fails
// to ingest it. Both look identical from here, so the wording upstream says
// what is actually known rather than pronouncing the valve dead.
func StaleValves(valves []model.Valve, now time.Time) []string {
	var out []string
	for _, v := range valves {
		// Non-irrigation devices share the mesh but not the job. A dimmer
		// switch has no business in an irrigation warning.
		if !v.IsValve() {
			continue
		}
		// Only judge valves that have a continuously changing metric.
		//
		// These valves transmit on change. An idle one with a full battery has
		// nothing to say, so the recorder writes no rows for days and the valve
		// looks dead while being perfectly healthy. Link quality is the only
		// metric here that moves on its own, so it is the only one whose
		// absence means anything. Valves without it are unjudgeable, and
		// staying quiet beats crying wolf.
		if !v.HasLink {
			continue
		}
		if now.Sub(v.LastHeard) > StaleAfter {
			out = append(out, fmt.Sprintf("%s (%s)", v.Device, humanSince(v.LastHeard, now)))
		}
	}
	return out
}

// Calendar palette. Kept beside the other styles so a theme change is one edit.
var (
	styleRan      = lipgloss.NewStyle().Foreground(lipgloss.Color("42"))
	styleRanNoVol = lipgloss.NewStyle().Foreground(lipgloss.Color("108"))
	styleNoRun    = lipgloss.NewStyle().Foreground(lipgloss.Color("237"))
	stylePane     = lipgloss.NewStyle().Border(lipgloss.RoundedBorder()).Padding(0, 1)
	// Reverse video marks today. A colour alone competes with the glyph
	// colours already carrying meaning in the grid.
	styleToday = lipgloss.NewStyle().Reverse(true)
)

// progressBar draws a filled proportion in width columns.
func progressBar(percent, width int) string {
	if width < 1 {
		return ""
	}
	filled := percent * width / 100
	if filled < 0 {
		filled = 0
	}
	if filled > width {
		filled = width
	}
	return styleRan.Render(strings.Repeat("█", filled)) +
		styleNoRun.Render(strings.Repeat("░", width-filled))
}

// countdown renders remaining time at a resolution a person can act on.
func countdown(d time.Duration) string {
	if d <= 0 {
		return "finishing"
	}
	h := int(d.Hours())
	m := int(d.Minutes()) % 60
	sec := int(d.Seconds()) % 60
	if h > 0 {
		return fmt.Sprintf("%dh %02dm", h, m)
	}
	if m > 0 {
		return fmt.Sprintf("%dm %02ds", m, sec)
	}
	return fmt.Sprintf("%ds", sec)
}

// countdownShort is the column form: never wider than 5 columns, so it cannot
// wrap the calendar row. Seconds are dropped above a minute, where they are
// not worth the width.
func countdownShort(d time.Duration) string {
	if d <= 0 {
		return "done"
	}
	switch {
	case d >= 10*time.Hour:
		// Unreachable under the 2h cap, but a wider string would wrap the row.
		return fmt.Sprintf("%dh", int(d.Hours()))
	case d >= time.Hour:
		return fmt.Sprintf("%dh%02dm", int(d.Hours()), int(d.Minutes())%60)
	case d >= time.Minute:
		return fmt.Sprintf("%dm", int(d.Minutes()))
	default:
		return fmt.Sprintf("%ds", int(d.Seconds()))
	}
}

// clip truncates to n lines, marking the cut so a reader knows something was
// dropped rather than assuming the pane ended naturally.
func clip(s string, n int) string {
	lines := strings.Split(s, "\n")
	if n < 1 || len(lines) <= n {
		return s
	}
	out := append([]string(nil), lines[:n-1]...)
	return strings.Join(append(out, styleDim.Render("…")), "\n")
}
