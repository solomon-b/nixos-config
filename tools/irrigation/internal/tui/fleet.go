package tui

import (
	"fmt"
	"sort"
	"strings"
	"time"

	"github.com/solomon-b/nixos-config/tools/irrigation/internal/model"
)

// lowBattery is where a valve stops being something to note and starts being
// something to act on before the season needs it.
const lowBattery = 30

// fleet lists every valve reporting telemetry, including ones no zone drives.
// The zone view is built from configuration.yaml, so an unassigned valve is
// invisible there by construction. That is exactly where a flat battery hides.
func (m Model) fleet() string {
	valves := fleetOrder(m.valves)

	var b strings.Builder
	b.WriteString(styleHeader.Render("VALVES"))
	b.WriteString(styleDim.Render(fmt.Sprintf("  %d reporting", len(valves))))
	if n := len(unassigned(valves)); n > 0 {
		b.WriteString(styleWarn.Render(fmt.Sprintf("  ·  %d driving no zone", n)))
	}
	b.WriteString("\n\n")

	fmt.Fprintf(&b, "%s\n", styleDim.Render(fmt.Sprintf("  %-34s %-22s %-7s %-14s %-7s %-8s %s",
		"VALVE", "ZONE", "BATTERY", "PROJECTED", "HEARD", "AUTOSHUT", "STATUS")))

	for i, v := range valves {
		cursor := "  "
		if i == m.fleetCursor {
			cursor = styleSelected.Render("> ")
		}
		zone := v.Zone
		zoneCell := zone
		if zone == "" {
			zoneCell = styleWarn.Render(pad("unassigned", 22))
		} else {
			zoneCell = pad(zone, 22)
		}

		battery := styleDim.Render(pad("-", 7))
		if v.HasBattery {
			cell := pad(fmt.Sprintf("%d%%", v.Battery), 7)
			switch {
			case v.Battery == 0:
				battery = styleBad.Render(cell)
			case v.Battery <= lowBattery:
				battery = styleWarn.Render(cell)
			default:
				battery = cell
			}
		}

		projected := styleDim.Render(pad("-", 14))
		if v.HasTrend {
			days := int(time.Until(v.Trend.Empty).Hours() / 24)
			cell := pad(fmt.Sprintf("empty in %dd", days), 14)
			if days < 30 {
				projected = styleWarn.Render(cell)
			} else {
				projected = cell
			}
		}

		auto := styleDim.Render(pad("-", 8))
		if v.HasAutoClose {
			if v.AutoClose {
				auto = styleWarn.Render(pad("on", 8))
			} else {
				auto = pad("off", 8)
			}
		}

		status := v.DeviceStatus
		if status != "" && status != "normal_state" {
			status = styleWarn.Render(status)
		}

		// Pad before styling. Escape codes count toward %-34s width, so a
		// styled name inside the format verb would shorten the column.
		name := pad(trunc(v.Device, 34), 34)
		if i == m.fleetCursor {
			name = styleSelected.Render(name)
		}
		fmt.Fprintf(&b, "%s%s %s %s %s %-7s %s %s\n",
			cursor, name, zoneCell, battery, projected,
			humanSince(v.LastHeard, m.now), auto, status)
	}

	if len(valves) == 0 {
		b.WriteString(styleDim.Render("No valve telemetry found."))
	}
	return b.String()
}

// unassigned reports valves that report telemetry but drive no zone.
func unassigned(valves []model.Valve) []string {
	var out []string
	for _, v := range valves {
		if v.Zone == "" {
			out = append(out, v.Device)
		}
	}
	return out
}

// fleetOrder keeps assigned valves first, in zone order, then the strays. It
// also drops non-irrigation devices, which share the mesh but not the job.
func fleetOrder(valves []model.Valve) []model.Valve {
	out := make([]model.Valve, 0, len(valves))
	for _, v := range valves {
		if v.IsValve() {
			out = append(out, v)
		}
	}
	sort.SliceStable(out, func(i, j int) bool {
		a, b := out[i], out[j]
		if (a.Zone == "") != (b.Zone == "") {
			return a.Zone != ""
		}
		if a.Zone != b.Zone {
			return a.Zone < b.Zone
		}
		return a.Device < b.Device
	})
	return out
}

// fleetValves is the ordered list the fleet cursor indexes into.
func (m Model) fleetValves() []model.Valve { return fleetOrder(m.valves) }
