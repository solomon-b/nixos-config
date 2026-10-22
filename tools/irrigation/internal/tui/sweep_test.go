package tui

import (
	"net/http"
	"net/http/httptest"
	"strings"
	"sync/atomic"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/solomon-b/nixos-config/tools/irrigation/internal/hass"
	"github.com/solomon-b/nixos-config/tools/irrigation/internal/model"
)

func TestVerdictNarrowsToWhatTheEvidenceSupports(t *testing.T) {
	cases := []struct {
		name string
		r    SweepResult
		want string
		ok   bool
	}{
		{"no ack", SweepResult{}, "no acknowledgement", false},
		{"ack only", SweepResult{Acked: true}, "opened, no flow reading", true},
		{"ack, meter read zero", SweepResult{Acked: true, HasVolume: true, Volume: 0},
			"opened, no flow reading", true},
		{"ack and water", SweepResult{Acked: true, HasVolume: true, Volume: 5},
			"opened, water seen", true},
	}
	for _, c := range cases {
		got, ok := c.r.Verdict()
		if got != c.want || ok != c.ok {
			t.Errorf("%s: Verdict = (%q, %v), want (%q, %v)", c.name, got, ok, c.want, c.ok)
		}
	}
}

func TestSweepRefusesWhileAZoneIsWatering(t *testing.T) {
	z := sampleZone()
	z.Status = "on"
	var hits int32
	var paths []string
	m := New([]model.Controller{{Name: "C", Zones: []model.Zone{z}}}, nil, "", nil).
		WithHass(stubHA(t, &hits, &paths))

	m = typeKeys(m, "t")
	if m.mode != modeNormal {
		t.Error("sweep prompt opened while a zone was running")
	}
	if m.actionErr == nil || !strings.Contains(m.actionErr.Error(), "running") {
		t.Errorf("actionErr = %v, want an explanation", m.actionErr)
	}
	if hits != 0 {
		t.Error("sweep contacted Home Assistant despite refusing")
	}
}

func TestSweepShutsEachValveAgain(t *testing.T) {
	var manualRuns, cancels int32
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch {
		case strings.HasSuffix(r.URL.Path, "/manual_run"):
			atomic.AddInt32(&manualRuns, 1)
		case strings.HasSuffix(r.URL.Path, "/cancel"):
			atomic.AddInt32(&cancels, 1)
		case strings.Contains(r.URL.Path, "/api/states/"):
			w.Write([]byte(`{"state":"on"}`))
			return
		}
		w.WriteHeader(http.StatusOK)
	}))
	defer srv.Close()

	z := sampleZone()
	m := New([]model.Controller{{Name: "C", Zones: []model.Zone{z}}}, nil, "", nil).
		WithHass(hass.New(srv.URL, "t"))
	m.sweepDur = time.Second
	m.ackWait = time.Millisecond

	msg := m.sweepStep(0, z, time.Second)()
	step, ok := msg.(sweepStepMsg)
	if !ok {
		t.Fatalf("got %T, want sweepStepMsg", msg)
	}
	if !step.result.Acked {
		t.Error("switch reported on but result was not acked")
	}
	if manualRuns != 1 {
		t.Errorf("manual_run calls = %d, want 1", manualRuns)
	}
	// The important one: a sweep must never leave water running.
	if cancels != 1 {
		t.Errorf("cancel calls = %d, want 1", cancels)
	}
}

func TestSweepMarksAValveThatNeverAnswers(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if strings.Contains(r.URL.Path, "/api/states/") {
			w.Write([]byte(`{"state":"off"}`))
			return
		}
		w.WriteHeader(http.StatusOK)
	}))
	defer srv.Close()

	z := sampleZone()
	m := New([]model.Controller{{Name: "C", Zones: []model.Zone{z}}}, nil, "", nil).
		WithHass(hass.New(srv.URL, "t"))
	m.ackWait = time.Millisecond
	step := m.sweepStep(0, z, time.Second)().(sweepStepMsg)
	if step.result.Acked {
		t.Error("a valve reporting off was counted as acknowledged")
	}
	if v, ok := step.result.Verdict(); ok || v != "no acknowledgement" {
		t.Errorf("Verdict = (%q, %v)", v, ok)
	}
}

func TestSweepAdvancesThroughEveryZone(t *testing.T) {
	zones := []model.Zone{sampleZone(), sampleZone(), sampleZone()}
	m := New([]model.Controller{{Name: "C", Zones: zones}}, nil, "", nil)
	m.sweeping = true
	m.sweepZones = zones
	m.sweepDur = time.Second

	for i := 0; i < 2; i++ {
		next, cmd := m.Update(sweepStepMsg{index: i, result: SweepResult{Acked: true}})
		m = next.(Model)
		if cmd == nil {
			t.Fatalf("step %d did not schedule the next zone", i)
		}
	}
	next, cmd := m.Update(sweepStepMsg{index: 2, result: SweepResult{Acked: true}})
	m = next.(Model)
	if cmd != nil {
		t.Error("sweep scheduled work past the last zone")
	}
	if m.sweeping {
		t.Error("sweep still marked running after the last zone")
	}
	if len(m.sweepResults) != 3 {
		t.Errorf("results = %d, want 3", len(m.sweepResults))
	}
}

func TestEscClearsAFinishedSweep(t *testing.T) {
	m := New(one(sampleZone()), nil, "", nil)
	m.sweepResults = []SweepResult{{Zone: "Z", Acked: true}}
	next, cmd := m.Update(tea.KeyMsg{Type: tea.KeyEsc})
	if cmd != nil {
		t.Error("esc quit instead of clearing the sweep report")
	}
	if len(next.(Model).sweepResults) != 0 {
		t.Error("esc did not clear the report")
	}
}
