package tui

import (
	"strings"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/solomon-b/nixos-config/tools/irrigation/internal/model"
)

func valve(device, zone string, battery int, isValve bool) model.Valve {
	v := model.Valve{Device: device, Zone: zone, Battery: battery, HasBattery: true,
		LastHeard: time.Now().Add(-time.Hour)}
	if isValve {
		v.HasAutoClose = true
		v.DeviceStatus = "normal_state"
	}
	return v
}

func TestFleetDropsNonIrrigationDevices(t *testing.T) {
	in := []model.Valve{
		valve("manifold_5", "Food Forest", 91, true),
		valve("hue_dimmer_switch_1", "", 100, false),
	}
	got := fleetOrder(in)
	if len(got) != 1 || got[0].Device != "manifold_5" {
		t.Errorf("fleetOrder kept a non-valve: %+v", got)
	}
}

func TestFleetPutsAssignedValvesFirst(t *testing.T) {
	in := []model.Valve{
		valve("manifold_2", "", 0, true),
		valve("manifold_5", "Food Forest", 91, true),
		valve("manifold_3", "", 56, true),
		valve("manifold_1", "Green House 1", 56, true),
	}
	got := fleetOrder(in)
	if got[0].Zone != "Food Forest" || got[1].Zone != "Green House 1" {
		t.Errorf("assigned valves not first and in zone order: %+v", got)
	}
	if got[2].Device != "manifold_2" || got[3].Device != "manifold_3" {
		t.Errorf("unassigned valves not last and ordered: %+v", got)
	}
}

func TestUnassignedNamesStrays(t *testing.T) {
	got := unassigned([]model.Valve{
		valve("manifold_5", "Food Forest", 91, true),
		valve("manifold_2", "", 0, true),
	})
	if len(got) != 1 || got[0] != "manifold_2" {
		t.Errorf("unassigned = %v", got)
	}
}

func TestFleetViewSurfacesAStrayFlatBattery(t *testing.T) {
	// The zone view is built from configuration.yaml, so this valve appears
	// nowhere else. That is the whole point of the view.
	valves := []model.Valve{
		valve("manifold_5", "Food Forest", 91, true),
		valve("manifold_2", "", 0, true),
	}
	m := New([]model.Controller{{Name: "C"}}, valves, "", nil)
	n, _ := m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'v'}})
	v := n.(Model).View()
	if !strings.Contains(v, "manifold_2") {
		t.Errorf("stray valve missing from fleet view:\n%s", v)
	}
	if !strings.Contains(v, "unassigned") {
		t.Errorf("stray valve not marked unassigned:\n%s", v)
	}
	if !strings.Contains(v, "driving no zone") {
		t.Errorf("no summary of strays:\n%s", v)
	}
}

func TestViewKeyTogglesBackToZones(t *testing.T) {
	m := New(one(sampleZone()), nil, "", nil)
	n, _ := m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'v'}})
	if !strings.Contains(n.(Model).View(), "VALVES") {
		t.Error("v did not open the fleet view")
	}
	n, _ = n.(Model).Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'v'}})
	if !strings.Contains(n.(Model).View(), "ZONE") {
		t.Error("v did not return to the zone view")
	}
}

func fleetLine(v string, device string) string {
	for _, ln := range strings.Split(v, "\n") {
		if strings.Contains(ln, device) {
			return ln
		}
	}
	return ""
}

func TestFleetHighlightsTheSelectedValveName(t *testing.T) {
	valves := []model.Valve{
		valve("manifold_5", "Food Forest", 91, true),
		valve("manifold_7", "Potted San Pedros", 90, true),
	}
	m := New([]model.Controller{{Name: "C"}}, valves, "", nil)
	n, _ := m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'v'}})
	v := n.(Model).View()

	// Assert the name itself carries the style, not merely that the row
	// contains an escape somewhere: the cursor arrow is styled too, so a
	// looser check passes even with no highlight at all.
	styled := styleSelected.Render(pad("manifold_5", 34))
	if !strings.Contains(fleetLine(v, "manifold_5"), styled) {
		t.Errorf("cursor row name is not highlighted:\n%q", fleetLine(v, "manifold_5"))
	}
	if strings.Contains(fleetLine(v, "manifold_7"), styleSelected.Render(pad("manifold_7", 34))) {
		t.Errorf("non-cursor row name is highlighted:\n%q", fleetLine(v, "manifold_7"))
	}
	if a, b := lipgloss.Width(fleetLine(v, "manifold_5")), lipgloss.Width(fleetLine(v, "manifold_7")); a != b {
		t.Errorf("row widths differ once styled: %d vs %d", a, b)
	}
}

func TestFleetHighlightFollowsTheCursor(t *testing.T) {
	valves := []model.Valve{
		valve("manifold_5", "Food Forest", 91, true),
		valve("manifold_7", "Potted San Pedros", 90, true),
	}
	m := New([]model.Controller{{Name: "C"}}, valves, "", nil)
	n, _ := m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'v'}})
	n, _ = n.(Model).Update(tea.KeyMsg{Type: tea.KeyDown})
	v := n.(Model).View()

	if !strings.Contains(fleetLine(v, "manifold_7"), styleSelected.Render(pad("manifold_7", 34))) {
		t.Errorf("highlight did not follow the cursor:\n%q", fleetLine(v, "manifold_7"))
	}
	if strings.Contains(fleetLine(v, "manifold_5"), styleSelected.Render(pad("manifold_5", 34))) {
		t.Errorf("previous row is still highlighted:\n%q", fleetLine(v, "manifold_5"))
	}
}

func TestStaleValvesIgnoresNonIrrigationDevices(t *testing.T) {
	// Regression: a Hue dimmer on the same mesh appeared in an irrigation
	// warning, because the filter existed only in the fleet view.
	now := time.Now()
	old := now.Add(-72 * time.Hour)
	valves := []model.Valve{
		{Device: "hue_dimmer_switch_1", LastHeard: old, HasBattery: true,
			LinkQuality: 100, HasLink: true},
		{Device: "manifold_11", LastHeard: old, HasAutoClose: true,
			DeviceStatus: "normal_state", LinkQuality: 180, HasLink: true},
	}
	got := StaleValves(valves, now)
	for _, s := range got {
		if strings.Contains(s, "hue_dimmer") {
			t.Errorf("non-valve device in irrigation warning: %v", got)
		}
	}
	if len(got) != 1 {
		t.Errorf("stale = %v, want just the valve", got)
	}
}

func TestStaleValvesOnlyJudgesValvesWithAHeartbeat(t *testing.T) {
	// Regression: valves report on change, so an idle one writes no rows for
	// days and looked dead. Six healthy valves were flagged silent while
	// zigbee2mqtt was receiving from all of them. Only link quality moves on
	// its own, so only valves reporting it can be judged.
	now := time.Now()
	old := now.Add(-72 * time.Hour)
	valves := []model.Valve{
		{Device: "manifold_11", LastHeard: old, DeviceStatus: "normal_state"},
		{Device: "manifold_2", LastHeard: old, DeviceStatus: "normal_state",
			LinkQuality: 200, HasLink: true},
	}
	got := StaleValves(valves, now)
	if len(got) != 1 {
		t.Fatalf("stale = %v, want only the valve with link quality", got)
	}
	if !strings.HasPrefix(got[0], "manifold_2") {
		t.Errorf("stale = %v, want manifold_2", got)
	}
}

func TestStaleValvesQuietForAHealthyLinkQuality(t *testing.T) {
	now := time.Now()
	valves := []model.Valve{
		{Device: "manifold_3", LastHeard: now.Add(-time.Hour),
			DeviceStatus: "normal_state", LinkQuality: 184, HasLink: true},
	}
	if got := StaleValves(valves, now); len(got) != 0 {
		t.Errorf("stale = %v, want none", got)
	}
}
