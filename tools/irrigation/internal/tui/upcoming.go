package tui

import (
	"fmt"
	"strings"
	"time"

	"github.com/solomon-b/nixos-config/tools/irrigation/internal/model"
)

// upcomingView lists the next scheduled waterings across every controller,
// soonest first.
func (m Model) upcomingView2() string {
	var b strings.Builder
	b.WriteString(styleHeader.Render("UPCOMING"))
	if len(m.upcoming) == 0 {
		b.WriteString(styleDim.Render("  nothing scheduled"))
		b.WriteString("\n\n")
		b.WriteString(styleDim.Render("Every zone has an empty weekday list, so none will fire."))
		return b.String()
	}
	b.WriteString(styleDim.Render(fmt.Sprintf("  next %d", len(m.upcoming))))
	b.WriteString("\n\n")

	fmt.Fprintf(&b, "%s\n", styleDim.Render(fmt.Sprintf("%-13s  %-13s  %-6s  %-22s %s",
		"WHEN", "WINDOW", "FOR", "ZONE", "IN")))

	var lastDay string
	for _, u := range m.upcoming {
		day := u.At.Format("Mon 01-02")
		when := day
		if day == lastDay {
			// Repeating the date on every row of the same morning is noise.
			when = ""
		}
		lastDay = day

		until := u.At.Sub(m.now)
		style := styleDim
		if until < 24*time.Hour {
			style = styleRan
		}

		fmt.Fprintf(&b, "%-13s  %s → %s  %-6s  %-22s %s\n",
			when,
			u.At.Format("15:04"), u.Ends().Format("15:04"),
			shortDur(u.Duration),
			trunc(u.Zone, 22),
			style.Render(humanUntil(until)))
	}
	return b.String()
}

// humanUntil renders a wait at a resolution that matches how far off it is.
func humanUntil(d time.Duration) string {
	switch {
	case d < 0:
		return "now"
	case d < time.Hour:
		return fmt.Sprintf("in %dm", int(d.Minutes()))
	case d < 48*time.Hour:
		return fmt.Sprintf("in %dh", int(d.Hours()))
	default:
		return fmt.Sprintf("in %dd", int(d.Hours()/24))
	}
}

// BuildUpcoming computes the next runs for every zone and merges them.
func BuildUpcoming(controllers []model.Controller, from time.Time, perZone, total int) []model.Upcoming {
	var lists [][]model.Upcoming
	for _, c := range controllers {
		for _, z := range c.Zones {
			lists = append(lists, model.NextRuns(z.Name, c.Name, z.Specs, from, perZone))
		}
	}
	all := model.MergeUpcoming(lists...)
	if total > 0 && len(all) > total {
		all = all[:total]
	}
	return all
}
