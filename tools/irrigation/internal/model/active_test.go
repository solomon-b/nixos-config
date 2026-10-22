package model

import (
	"testing"
	"time"
)

func active() ActiveRun {
	return ActiveRun{
		Start:    time.Date(2026, 8, 20, 6, 0, 0, 0, time.UTC),
		Duration: 2 * time.Hour,
	}
}

func TestRemainingCountsDown(t *testing.T) {
	a := active()
	if got := a.Remaining(a.Start); got != 2*time.Hour {
		t.Errorf("at start Remaining = %v, want 2h", got)
	}
	if got := a.Remaining(a.Start.Add(30 * time.Minute)); got != 90*time.Minute {
		t.Errorf("Remaining = %v, want 90m", got)
	}
}

func TestRemainingNeverGoesNegative(t *testing.T) {
	a := active()
	if got := a.Remaining(a.Start.Add(5 * time.Hour)); got != 0 {
		t.Errorf("Remaining = %v, want 0 after the run ended", got)
	}
}

func TestPercentIsClamped(t *testing.T) {
	a := active()
	cases := []struct {
		at   time.Duration
		want int
	}{
		{0, 0},
		{time.Hour, 50},
		{2 * time.Hour, 100},
		{5 * time.Hour, 100},
	}
	for _, c := range cases {
		if got := a.Percent(a.Start.Add(c.at)); got != c.want {
			t.Errorf("Percent at +%v = %d, want %d", c.at, got, c.want)
		}
	}
	if got := a.Percent(a.Start.Add(-time.Hour)); got != 0 {
		t.Errorf("Percent before start = %d, want 0", got)
	}
}

func TestPercentSurvivesZeroDuration(t *testing.T) {
	a := ActiveRun{Start: time.Now()}
	if got := a.Percent(time.Now()); got != 0 {
		t.Errorf("Percent = %d, want 0 for a zero-length run", got)
	}
}
