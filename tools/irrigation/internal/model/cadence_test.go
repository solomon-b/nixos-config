package model

import (
	"testing"
	"time"
)

func runsAt(days ...int) []Run {
	base := time.Date(2026, 8, 1, 6, 0, 0, 0, time.UTC)
	var out []Run
	for _, d := range days {
		s := base.AddDate(0, 0, d)
		out = append(out, Run{Started: s, Ended: s.Add(2 * time.Hour), Minutes: 120})
	}
	return out
}

func TestMedianGapWeekly(t *testing.T) {
	got, ok := MedianGap(runsAt(0, 7, 14, 21))
	if !ok {
		t.Fatal("ok = false, want true")
	}
	if want := 7 * 24 * time.Hour; got != want {
		t.Errorf("MedianGap = %v, want %v", got, want)
	}
}

func TestMedianGapIgnoresSingleOutlier(t *testing.T) {
	got, _ := MedianGap(runsAt(0, 7, 14, 44))
	if want := 7 * 24 * time.Hour; got != want {
		t.Errorf("MedianGap = %v, want %v", got, want)
	}
}

func TestMedianGapEvenCountAverages(t *testing.T) {
	got, _ := MedianGap(runsAt(0, 2, 6))
	if want := 3 * 24 * time.Hour; got != want {
		t.Errorf("MedianGap = %v, want %v", got, want)
	}
}

func TestMedianGapNeedsTwoRuns(t *testing.T) {
	if _, ok := MedianGap(runsAt(0)); ok {
		t.Error("ok = true for a single run, want false")
	}
	if _, ok := MedianGap(nil); ok {
		t.Error("ok = true for no runs, want false")
	}
}

func TestMedianGapHandlesUnsortedInput(t *testing.T) {
	got, ok := MedianGap(runsAt(14, 0, 21, 7))
	if !ok {
		t.Fatal("ok = false")
	}
	if want := 7 * 24 * time.Hour; got != want {
		t.Errorf("MedianGap = %v, want %v", got, want)
	}
}

func TestIsLateBeyondThreshold(t *testing.T) {
	runs := runsAt(0, 7, 14)
	last := runs[len(runs)-1].Started
	if IsLate(runs, last.Add(10*24*time.Hour)) {
		t.Error("late at 10d, want not late")
	}
	if !IsLate(runs, last.Add(11*24*time.Hour)) {
		t.Error("not late at 11d, want late")
	}
}

func TestIsLateNeverForZonesWithoutBaseline(t *testing.T) {
	if IsLate(nil, time.Now()) {
		t.Error("no-history zone reported late")
	}
	if IsLate(runsAt(0), time.Now().AddDate(1, 0, 0)) {
		t.Error("single-run zone reported late a year later")
	}
}

func TestVolumeBaselineUsesOnlyRunsWithVolume(t *testing.T) {
	runs := runsAt(0, 7, 14)
	runs[0].VolumeDelta, runs[0].HasVolume = 200, true
	runs[1].VolumeDelta, runs[1].HasVolume = 220, true
	runs[2].HasVolume = false
	got, ok := VolumeBaseline(runs)
	if !ok {
		t.Fatal("ok = false, want true")
	}
	if want := 210.0; got != want {
		t.Errorf("VolumeBaseline = %v, want %v", got, want)
	}
}

func TestVolumeBaselineAbsentWhenNoReadings(t *testing.T) {
	if _, ok := VolumeBaseline(runsAt(0, 7)); ok {
		t.Error("ok = true with no volume readings, want false")
	}
}

func TestMedianGapDoesNotMutateCallerSlice(t *testing.T) {
	runs := runsAt(14, 0, 7)
	first := runs[0].Started
	MedianGap(runs)
	if !runs[0].Started.Equal(first) {
		t.Error("MedianGap reordered the caller's slice")
	}
}
