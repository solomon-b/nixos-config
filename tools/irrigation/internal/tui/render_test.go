package tui

import (
	"strings"
	"testing"
	"time"

	"github.com/charmbracelet/lipgloss"

	"github.com/solomon-b/nixos-config/tools/irrigation/internal/model"
)

func TestSparklineScalesToMax(t *testing.T) {
	got := sparkline([]float64{0, 100})
	if []rune(got)[0] != '▁' {
		t.Errorf("low value = %q, want ▁", string([]rune(got)[0]))
	}
	if []rune(got)[1] != '█' {
		t.Errorf("high value = %q, want █", string([]rune(got)[1]))
	}
}

func TestSparklineAllZeroDrawsFlatLine(t *testing.T) {
	if got := sparkline([]float64{0, 0, 0}); got != "───" {
		t.Errorf("sparkline = %q, want ───", got)
	}
}

func TestSparklineEmpty(t *testing.T) {
	if got := sparkline(nil); got != "" {
		t.Errorf("sparkline = %q, want empty", got)
	}
}

func TestZoneStatusIdleForScheduledZoneWithNoRuns(t *testing.T) {
	label, _ := zoneStatus(model.Zone{Scheduled: true}, time.Now())
	if label != "idle" {
		t.Errorf("label = %q, want idle", label)
	}
}

func TestStaleValvesIgnoresDevicesThatNeverReported(t *testing.T) {
	now := time.Now()
	// A valve with no health entities is simply absent from the slice.
	// Only a device present and quiet counts.
	valves := []model.Valve{
		{Device: "manifold_2", LastHeard: now.Add(-210 * time.Hour),
			DeviceStatus: "normal_state", LinkQuality: 200, HasLink: true},
		{Device: "cactus_fence_irrigation_controller", LastHeard: now.Add(-1 * time.Hour),
			DeviceStatus: "normal_state", LinkQuality: 184, HasLink: true},
	}
	got := StaleValves(valves, now)
	if len(got) != 1 {
		t.Fatalf("stale count = %d, want 1: %v", len(got), got)
	}
	if !strings.HasPrefix(got[0], "manifold_2") {
		t.Errorf("stale = %v, want manifold_2", got)
	}
}

func TestZoneStatusDistinguishesDisabledFromIdle(t *testing.T) {
	now := time.Now()
	if label, _ := zoneStatus(model.Zone{Scheduled: false}, now); label != "disabled" {
		t.Errorf("unscheduled zone label = %q, want disabled", label)
	}
	// Scheduled but never run is not the same thing and must not read as normal.
	if label, _ := zoneStatus(model.Zone{Scheduled: true}, now); label != "idle" {
		t.Errorf("scheduled zone with no runs = %q, want idle", label)
	}
}

func TestCountdownResolution(t *testing.T) {
	cases := map[time.Duration]string{
		0:                              "finishing",
		-time.Second:                   "finishing",
		45 * time.Second:               "45s",
		90 * time.Second:               "1m 30s",
		(2*time.Hour + 5*time.Minute):  "2h 05m",
	}
	for d, want := range cases {
		if got := countdown(d); got != want {
			t.Errorf("countdown(%v) = %q, want %q", d, got, want)
		}
	}
}

func TestProgressBarFillsProportionally(t *testing.T) {
	if got := lipgloss.Width(progressBar(50, 10)); got != 10 {
		t.Errorf("bar width = %d, want 10", got)
	}
	if strings.Count(progressBar(0, 10), "█") != 0 {
		t.Error("0%% bar has filled cells")
	}
	if strings.Count(progressBar(100, 10), "█") != 10 {
		t.Error("100%% bar is not full")
	}
	if progressBar(50, 0) != "" {
		t.Error("zero-width bar produced output")
	}
}

func TestCountdownShortNeverExceedsTheColumn(t *testing.T) {
	for _, d := range []time.Duration{
		0, time.Second, 59 * time.Second, time.Minute, 59 * time.Minute,
		time.Hour, 90 * time.Minute, 2*time.Hour + 5*time.Minute, 23 * time.Hour,
	} {
		if got := countdownShort(d); len(got) > 5 {
			t.Errorf("countdownShort(%v) = %q, %d chars, want at most 5", d, got, len(got))
		}
	}
}

func TestCountdownShortForms(t *testing.T) {
	cases := map[time.Duration]string{
		0:                             "done",
		45 * time.Second:              "45s",
		90 * time.Second:              "1m",
		37 * time.Minute:              "37m",
		2*time.Hour + 5*time.Minute:   "2h05m",
	}
	for d, want := range cases {
		if got := countdownShort(d); got != want {
			t.Errorf("countdownShort(%v) = %q, want %q", d, got, want)
		}
	}
}

func TestShortDurReadsLikeSpeech(t *testing.T) {
	cases := map[time.Duration]string{
		45 * time.Second:            "45s",
		10 * time.Minute:            "10m",
		90 * time.Second:            "1m30s",
		time.Hour:                   "1h",
		2 * time.Hour:               "2h",
		90 * time.Minute:            "1h30m",
	}
	for d, want := range cases {
		if got := shortDur(d); got != want {
			t.Errorf("shortDur(%v) = %q, want %q", d, got, want)
		}
	}
}

func TestClipMarksTheCut(t *testing.T) {
	in := "a\nb\nc\nd\ne"
	got := clip(in, 3)
	lines := strings.Split(got, "\n")
	if len(lines) != 3 {
		t.Fatalf("clip produced %d lines, want 3", len(lines))
	}
	if !strings.Contains(lines[2], "…") {
		t.Errorf("clip did not mark the cut: %q", got)
	}
}

func TestClipLeavesShortContentAlone(t *testing.T) {
	in := "a\nb"
	if got := clip(in, 5); got != in {
		t.Errorf("clip = %q, want it unchanged", got)
	}
}
