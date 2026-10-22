package tui

import (
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/solomon-b/nixos-config/tools/irrigation/internal/hass"
	"github.com/solomon-b/nixos-config/tools/irrigation/internal/model"
)

func recorder(t *testing.T, bodies *[]string, paths *[]string) *hass.Client {
	t.Helper()
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		buf := make([]byte, r.ContentLength)
		r.Body.Read(buf)
		*bodies = append(*bodies, string(buf))
		*paths = append(*paths, r.URL.Path)
		w.WriteHeader(http.StatusOK)
	}))
	t.Cleanup(srv.Close)
	return hass.New(srv.URL, "t")
}

func autoCloseValve(on bool) model.Valve {
	return model.Valve{
		Device: "manifold_11", Zone: "Green House 2",
		AutoClose: on, HasAutoClose: true,
		AutoCloseEntity: "switch.0x842712fffe80d231_auto_close_when_water_shortage",
		DeviceStatus:    "normal_state", LastHeard: time.Now(),
	}
}

func TestToggleAutoCloseUsesTheRealEntityId(t *testing.T) {
	var bodies, paths []string
	v := autoCloseValve(true)
	m := New([]model.Controller{{Name: "C"}}, []model.Valve{v}, "", nil).WithHass(recorder(t, &bodies, &paths))
	n, _ := m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'v'}})
	n, cmd := n.(Model).Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'x'}})
	if cmd == nil {
		t.Fatal("x produced no command")
	}
	if _, ok := cmd().(actionDoneMsg); !ok {
		t.Fatal("toggle did not report a result")
	}
	// It was on, so it must turn off, and via the address-named entity.
	if len(paths) != 1 || !strings.HasSuffix(paths[0], "/switch/turn_off") {
		t.Errorf("paths = %v, want switch/turn_off", paths)
	}
	if !strings.Contains(bodies[0], "0x842712fffe80d231") {
		t.Errorf("body did not use the real entity id: %s", bodies[0])
	}
}

func TestToggleAutoCloseTurnsOnWhenOff(t *testing.T) {
	var bodies, paths []string
	m := New([]model.Controller{{Name: "C"}}, []model.Valve{autoCloseValve(false)}, "", nil).
		WithHass(recorder(t, &bodies, &paths))
	n, _ := m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'v'}})
	_, cmd := n.(Model).Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'x'}})
	cmd()
	if len(paths) != 1 || !strings.HasSuffix(paths[0], "/switch/turn_on") {
		t.Errorf("paths = %v, want switch/turn_on", paths)
	}
}

func TestToggleRefusesWhenNoSwitchExists(t *testing.T) {
	var bodies, paths []string
	v := model.Valve{Device: "manifold_5", Zone: "Food Forest", DeviceStatus: "normal_state"}
	m := New([]model.Controller{{Name: "C"}}, []model.Valve{v}, "", nil).WithHass(recorder(t, &bodies, &paths))
	n, _ := m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'v'}})
	n, cmd := n.(Model).Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'x'}})
	if cmd != nil {
		t.Error("toggled a valve with no auto-shutoff switch")
	}
	if n.(Model).actionErr == nil {
		t.Error("no explanation for the refusal")
	}
	if len(paths) != 0 {
		t.Errorf("contacted Home Assistant anyway: %v", paths)
	}
}

func TestAdjustSendsPercentage(t *testing.T) {
	var bodies, paths []string
	m := New(one(sampleZone()), nil, "", nil).WithHass(recorder(t, &bodies, &paths))
	m = typeKeys(m, "p")
	if m.mode != modeAdjust {
		t.Fatalf("mode = %v, want modeAdjust", m.mode)
	}
	if m.input != defaultAdjust {
		t.Errorf("input = %q, want %q", m.input, defaultAdjust)
	}
	n, cmd := m.Update(tea.KeyMsg{Type: tea.KeyEnter})
	if cmd == nil {
		t.Fatal("enter produced no command")
	}
	cmd()
	if len(paths) != 1 || !strings.HasSuffix(paths[0], "/irrigation_unlimited/adjust_time") {
		t.Errorf("paths = %v", paths)
	}
	if !strings.Contains(bodies[0], `"percentage":150`) {
		t.Errorf("body = %s, want percentage 150", bodies[0])
	}
	_ = n
}

func TestAdjustAcceptsReset(t *testing.T) {
	var bodies, paths []string
	m := New(one(sampleZone()), nil, "", nil).WithHass(recorder(t, &bodies, &paths))
	m = typeKeys(m, "p")
	for range defaultAdjust {
		n, _ := m.Update(tea.KeyMsg{Type: tea.KeyBackspace})
		m = n.(Model)
	}
	m = typeKeys(m, "reset")
	_, cmd := m.Update(tea.KeyMsg{Type: tea.KeyEnter})
	cmd()
	if !strings.Contains(bodies[0], "reset") {
		t.Errorf("body = %s, want a reset", bodies[0])
	}
}

func TestAdjustRejectsOutOfRange(t *testing.T) {
	var bodies, paths []string
	m := New(one(sampleZone()), nil, "", nil).WithHass(recorder(t, &bodies, &paths))
	m = typeKeys(m, "p")
	for range defaultAdjust {
		n, _ := m.Update(tea.KeyMsg{Type: tea.KeyBackspace})
		m = n.(Model)
	}
	m = typeKeys(m, "9000")
	n, cmd := m.Update(tea.KeyMsg{Type: tea.KeyEnter})
	m = n.(Model)
	if cmd != nil {
		t.Error("sent an out-of-range adjustment")
	}
	if m.mode != modeAdjust {
		t.Error("prompt closed on invalid input")
	}
	if m.actionErr == nil {
		t.Error("no explanation for the rejection")
	}
	if len(paths) != 0 {
		t.Errorf("contacted Home Assistant anyway: %v", paths)
	}
}

func TestFleetCursorMovesIndependentlyOfZoneCursor(t *testing.T) {
	valves := []model.Valve{autoCloseValve(true), autoCloseValve(false)}
	valves[1].Device = "manifold_12"
	m := New(one(sampleZone(), sampleZone()), valves, "", nil)
	n, _ := m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'v'}})
	n, _ = n.(Model).Update(tea.KeyMsg{Type: tea.KeyDown})
	got := n.(Model)
	if got.fleetCursor != 1 {
		t.Errorf("fleetCursor = %d, want 1", got.fleetCursor)
	}
	if got.cursor != 0 {
		t.Errorf("zone cursor moved to %d while in the fleet view", got.cursor)
	}
}
