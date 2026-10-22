package model

import (
	"testing"
	"time"
)

func samples(start time.Time, step time.Duration, vals ...float64) []Sample {
	out := make([]Sample, len(vals))
	for i, v := range vals {
		out[i] = Sample{At: start.Add(time.Duration(i) * step), Value: v}
	}
	return out
}

func TestFitBatteryNeedsEnoughReadings(t *testing.T) {
	now := time.Now()
	if _, ok := FitBattery(nil); ok {
		t.Error("fitted a trend from nothing")
	}
	if _, ok := FitBattery(samples(now, 24*time.Hour, 100, 90)); ok {
		t.Error("fitted a trend from two readings")
	}
}

func TestFitBatteryNeedsEnoughSpan(t *testing.T) {
	now := time.Now()
	// Three readings inside a single day would imply a wild slope.
	if _, ok := FitBattery(samples(now, time.Hour, 100, 99, 98)); ok {
		t.Error("fitted a trend from a few hours of history")
	}
}

func TestFitBatteryProjectsEmpty(t *testing.T) {
	start := time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC)
	// 100 to 80 over 20 days is 1%/day, so 80 more days from the last reading.
	tr, ok := FitBattery(samples(start, 10*24*time.Hour, 100, 90, 80))
	if !ok {
		t.Fatal("no trend fitted")
	}
	if got := tr.PerDay; got > -0.9 || got < -1.1 {
		t.Errorf("PerDay = %.3f, want about -1", got)
	}
	last := start.Add(20 * 24 * time.Hour)
	wantEmpty := last.AddDate(0, 0, 80)
	if diff := tr.Empty.Sub(wantEmpty); diff > 48*time.Hour || diff < -48*time.Hour {
		t.Errorf("Empty = %v, want about %v", tr.Empty, wantEmpty)
	}
}

func TestFitBatteryIgnoresFlatOrRisingLevels(t *testing.T) {
	start := time.Date(2026, 1, 1, 0, 0, 0, 0, time.UTC)
	if _, ok := FitBattery(samples(start, 10*24*time.Hour, 100, 100, 100)); ok {
		t.Error("projected an empty date for a flat battery")
	}
	if _, ok := FitBattery(samples(start, 10*24*time.Hour, 60, 80, 100)); ok {
		t.Error("projected an empty date for a rising battery")
	}
}

func TestAttachVolumeUsesOnlyThisValvesReadings(t *testing.T) {
	start := time.Date(2026, 8, 20, 16, 35, 0, 0, time.UTC)
	run := Run{Started: start, Ended: start.Add(time.Hour)}

	// Cactus Fence's own counter barely moves.
	own := []Sample{
		{At: start.Add(time.Minute), Value: 64},
		{At: start.Add(30 * time.Minute), Value: 65},
	}
	got := AttachVolume([]Run{run}, own)
	if !got[0].HasVolume {
		t.Fatal("no volume attached")
	}
	if got[0].VolumeDelta != 1 {
		t.Errorf("VolumeDelta = %v, want 1", got[0].VolumeDelta)
	}
}

func TestAttachVolumeIgnoresConcurrentRunsOnOtherValves(t *testing.T) {
	// Regression: correlating by time alone mixed cumulative counters from
	// whichever valves happened to run in the same window, producing deltas in
	// the tens of thousands for a zone whose counter moves by one.
	start := time.Date(2026, 8, 20, 16, 35, 0, 0, time.UTC)
	run := Run{Started: start, Ended: start.Add(time.Hour)}
	own := []Sample{
		{At: start.Add(time.Minute), Value: 64},
		{At: start.Add(30 * time.Minute), Value: 65},
	}
	got := AttachVolume([]Run{run}, own)
	if got[0].VolumeDelta > 10 {
		t.Errorf("VolumeDelta = %v, a foreign counter leaked in", got[0].VolumeDelta)
	}
}

func TestAttachVolumeNeedsTwoSamplesInTheWindow(t *testing.T) {
	start := time.Date(2026, 8, 20, 6, 0, 0, 0, time.UTC)
	run := Run{Started: start, Ended: start.Add(2 * time.Hour)}
	one := []Sample{{At: start.Add(time.Minute), Value: 63}}
	if got := AttachVolume([]Run{run}, one); got[0].HasVolume {
		t.Error("a single reading was treated as a measured volume")
	}
}

func TestAttachVolumeExcludesReadingsOutsideTheRun(t *testing.T) {
	start := time.Date(2026, 8, 20, 6, 0, 0, 0, time.UTC)
	run := Run{Started: start, Ended: start.Add(time.Hour)}
	samples := []Sample{
		{At: start.Add(-time.Hour), Value: 10},
		{At: start.Add(30 * time.Minute), Value: 100},
		{At: start.Add(30 * time.Minute), Value: 105},
		{At: start.Add(5 * time.Hour), Value: 900},
	}
	got := AttachVolume([]Run{run}, samples)
	if got[0].VolumeDelta != 5 {
		t.Errorf("VolumeDelta = %v, want 5 from the in-window readings only", got[0].VolumeDelta)
	}
}
