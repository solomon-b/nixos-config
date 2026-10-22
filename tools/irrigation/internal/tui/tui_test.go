package tui

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/solomon-b/nixos-config/tools/irrigation/internal/model"
)

// one wraps zones in a single unnamed controller, which is the shape most of
// these tests care about.
func one(zs ...model.Zone) []model.Controller {
	return []model.Controller{{Name: "Test Controller", Zones: zs}}
}

func sampleZone() model.Zone {
	base := time.Date(2026, 8, 13, 6, 0, 0, 0, time.UTC)
	return model.Zone{
		Name:         "Food Forest",
		SwitchEntity: "switch.manifold_5",
		Runs: []model.Run{
			{Started: base, Ended: base.Add(2 * time.Hour), Minutes: 120},
			{Started: base.AddDate(0, 0, 7), Ended: base.AddDate(0, 0, 7).Add(2 * time.Hour),
				Minutes: 120, VolumeDelta: 219, HasVolume: true},
		},
	}
}

func press(m Model, t tea.KeyType) Model {
	next, _ := m.Update(tea.KeyMsg{Type: t})
	return next.(Model)
}

func TestHistoryListsRunsNewestFirst(t *testing.T) {
	m := New(one(sampleZone()), nil, "", nil)
	out := m.history(m.zones()[0])
	if !strings.Contains(out, "219") {
		t.Errorf("history missing volume:\n%s", out)
	}
	if !strings.Contains(out, "no reading") {
		t.Errorf("history should mark the run without volume:\n%s", out)
	}
	first := strings.Index(out, "08-20")
	second := strings.Index(out, "08-13")
	if first < 0 || second < 0 || first > second {
		t.Errorf("runs not newest-first:\n%s", out)
	}
}

func TestHistoryHandlesZoneWithNoRuns(t *testing.T) {
	m := New(one(model.Zone{Name: "Green House 2"}), nil, "", nil)
	if out := m.history(m.zones()[0]); !strings.Contains(out, "No runs on record") {
		t.Errorf("history = %q", out)
	}
}

func TestEscQuits(t *testing.T) {
	m := New(one(sampleZone()), nil, "", nil)
	if _, cmd := m.Update(tea.KeyMsg{Type: tea.KeyEsc}); cmd == nil {
		t.Error("esc did not quit")
	}
}

func TestHistoryReportsDroppedRuns(t *testing.T) {
	z := sampleZone()
	base := time.Date(2026, 6, 1, 6, 0, 0, 0, time.UTC)
	for i := 0; i < 10; i++ {
		d := base.AddDate(0, 0, i*7)
		z.Runs = append([]model.Run{{Started: d, Ended: d.Add(time.Hour), Minutes: 60}}, z.Runs...)
	}
	m := New(one(z), nil, "", nil)
	out := m.historyN(z, 3)
	if !strings.Contains(out, "earlier runs") {
		t.Errorf("truncated history did not say how many were dropped:\n%s", out)
	}
	if strings.Count(out, "→") != 3 {
		t.Errorf("expected 3 run rows, got:\n%s", out)
	}
}

func TestReloadReplacesData(t *testing.T) {
	m := New(nil, nil, "old", func(context.Context) ([]model.Controller, []model.Valve, string, error) {
		return one(sampleZone()), nil, "fresh", nil
	})
	next, _ := m.Update(reloadedMsg{controllers: one(sampleZone()), warning: "fresh"})
	m = next.(Model)
	if len(m.zones()) != 1 || m.warning != "fresh" {
		t.Errorf("state after reload: zones=%d warning=%q", len(m.zones()), m.warning)
	}
	if m.loading {
		t.Error("loading still true after reload landed")
	}
}

func TestReloadFailureKeepsOldDataAndRecordsError(t *testing.T) {
	m := New(one(sampleZone()), nil, "", nil)
	next, _ := m.Update(reloadedMsg{err: errors.New("boom")})
	m = next.(Model)
	if len(m.zones()) != 1 {
		t.Error("failed reload discarded existing zones")
	}
	if m.loadErr == nil {
		t.Error("loadErr not recorded")
	}
}

func TestCursorResetsIfZoneListShrinks(t *testing.T) {
	m := New(one(sampleZone(), sampleZone(), sampleZone()), nil, "", nil)
	m.cursor = 2
	next, _ := m.Update(reloadedMsg{controllers: one(sampleZone())})
	if got := next.(Model).cursor; got != 0 {
		t.Errorf("cursor = %d after list shrank, want 0", got)
	}
}

func two() []model.Controller {
	return []model.Controller{
		{Name: "Middle Garden", Zones: []model.Zone{sampleZone(), sampleZone()}},
		{Name: "Back Orchard", Zones: []model.Zone{{Name: "Apple Row", Scheduled: true}}},
	}
}

func TestTabCyclesControllers(t *testing.T) {
	m := New(two(), nil, "", nil)
	if m.active != 0 {
		t.Fatalf("active = %d, want 0", m.active)
	}
	next, _ := m.Update(tea.KeyMsg{Type: tea.KeyTab})
	m = next.(Model)
	if m.active != 1 {
		t.Errorf("active = %d after tab, want 1", m.active)
	}
	next, _ = m.Update(tea.KeyMsg{Type: tea.KeyTab})
	if got := next.(Model).active; got != 0 {
		t.Errorf("active = %d after wrapping, want 0", got)
	}
}

func TestTabResetsCursorSoItCannotPointPastTheNewZoneList(t *testing.T) {
	m := New(two(), nil, "", nil)
	m.cursor = 1 // valid for Middle Garden's two zones
	next, _ := m.Update(tea.KeyMsg{Type: tea.KeyTab})
	m = next.(Model)
	// Back Orchard has one zone; a stale cursor of 1 would panic on render.
	if m.cursor != 0 {
		t.Errorf("cursor = %d after switching controller, want 0", m.cursor)
	}
	if len(m.zones()) != 1 {
		t.Errorf("zones = %d, want Back Orchard's 1", len(m.zones()))
	}
}

func TestTabIsInertWithOneController(t *testing.T) {
	m := New(one(sampleZone()), nil, "", nil)
	next, _ := m.Update(tea.KeyMsg{Type: tea.KeyTab})
	if got := next.(Model).active; got != 0 {
		t.Errorf("active = %d, want 0", got)
	}
}

func TestViewShowsControllerNameFromConfig(t *testing.T) {
	m := New(two(), nil, "", nil)
	if v := m.View(); !strings.Contains(v, "Middle Garden") || !strings.Contains(v, "(1/2)") {
		t.Errorf("view missing controller name or indicator:\n%s", v)
	}
	next, _ := m.Update(tea.KeyMsg{Type: tea.KeyTab})
	if v := next.(Model).View(); !strings.Contains(v, "Back Orchard") {
		t.Errorf("view did not follow the active controller:\n%s", v)
	}
}
