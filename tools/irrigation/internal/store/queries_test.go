package store

import (
	"testing"
	"time"
)

func TestParseHMS(t *testing.T) {
	cases := map[string]time.Duration{
		"2:00:00": 2 * time.Hour,
		"0:10:00": 10 * time.Minute,
		"1:59:30": time.Hour + 59*time.Minute + 30*time.Second,
	}
	for in, want := range cases {
		got, err := parseHMS(in)
		if err != nil {
			t.Errorf("parseHMS(%q): %v", in, err)
			continue
		}
		if got != want {
			t.Errorf("parseHMS(%q) = %v, want %v", in, got, want)
		}
	}
	for _, bad := range []string{"", "2:00", "a:b:c", "1:2:3:4"} {
		if _, err := parseHMS(bad); err == nil {
			t.Errorf("parseHMS(%q) accepted junk", bad)
		}
	}
}

// The strings here are copied verbatim from the recorder during Food Forest's
// run on 2026-08-20, so this covers the real attribute shapes.
func TestActiveRunFromRealAttributes(t *testing.T) {
	zs := ZoneState{
		Status:          "on",
		CurrentStart:    "2026-08-20T06:00:00-07:00",
		CurrentDuration: "2:00:00",
		CurrentName:     "Schedule 1",
	}
	a := zs.ActiveRun()
	if a == nil {
		t.Fatal("ActiveRun = nil for a running zone")
	}
	if a.Duration != 2*time.Hour {
		t.Errorf("Duration = %v", a.Duration)
	}
	if a.ScheduleName != "Schedule 1" {
		t.Errorf("ScheduleName = %q", a.ScheduleName)
	}
	if got, want := a.Ends().UTC(), a.Start.Add(2*time.Hour).UTC(); !got.Equal(want) {
		t.Errorf("Ends = %v, want %v", got, want)
	}
}

func TestActiveRunNilWhenNotRunning(t *testing.T) {
	if (ZoneState{Status: "off", CurrentStart: "2026-08-20T06:00:00-07:00", CurrentDuration: "2:00:00"}).ActiveRun() != nil {
		t.Error("ActiveRun returned a run for a zone that is off")
	}
	if (ZoneState{Status: "on"}).ActiveRun() != nil {
		t.Error("ActiveRun returned a run with no start time")
	}
	if (ZoneState{Status: "on", CurrentStart: "not-a-time", CurrentDuration: "2:00:00"}).ActiveRun() != nil {
		t.Error("ActiveRun accepted an unparseable start")
	}
}
