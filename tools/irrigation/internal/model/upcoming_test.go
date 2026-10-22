package model

import (
	"testing"
	"time"
)

func mondayAt6() []ScheduleSpec {
	return []ScheduleSpec{{Hour: 6, Duration: 2 * time.Hour, Weekdays: []time.Weekday{time.Monday}}}
}

func TestNextRunsPicksTheComingMonday(t *testing.T) {
	// Friday 2026-08-21.
	from := time.Date(2026, 8, 21, 12, 0, 0, 0, time.UTC)
	got := NextRuns("Cactus Fence", "Middle Garden", mondayAt6(), from, 1)
	if len(got) != 1 {
		t.Fatalf("got %d runs", len(got))
	}
	want := time.Date(2026, 8, 24, 6, 0, 0, 0, time.UTC)
	if !got[0].At.Equal(want) {
		t.Errorf("At = %v, want %v", got[0].At, want)
	}
	if got[0].Ends() != want.Add(2*time.Hour) {
		t.Errorf("Ends = %v", got[0].Ends())
	}
}

func TestNextRunsSkipsTodayOnceTheTimeHasPassed(t *testing.T) {
	// Monday 07:00, after the 06:00 slot.
	from := time.Date(2026, 8, 24, 7, 0, 0, 0, time.UTC)
	got := NextRuns("Z", "C", mondayAt6(), from, 1)
	want := time.Date(2026, 8, 31, 6, 0, 0, 0, time.UTC)
	if !got[0].At.Equal(want) {
		t.Errorf("At = %v, want next week %v", got[0].At, want)
	}
}

func TestNextRunsIncludesTodayWhenStillAhead(t *testing.T) {
	// Monday 05:00, before the 06:00 slot.
	from := time.Date(2026, 8, 24, 5, 0, 0, 0, time.UTC)
	got := NextRuns("Z", "C", mondayAt6(), from, 1)
	want := time.Date(2026, 8, 24, 6, 0, 0, 0, time.UTC)
	if !got[0].At.Equal(want) {
		t.Errorf("At = %v, want today %v", got[0].At, want)
	}
}

func TestNextRunsHandlesMultipleWeekdays(t *testing.T) {
	// Green House 1: Mon, Wed, Sat at 05:00.
	specs := []ScheduleSpec{{Hour: 5, Duration: time.Hour,
		Weekdays: []time.Weekday{time.Monday, time.Wednesday, time.Saturday}}}
	from := time.Date(2026, 8, 21, 12, 0, 0, 0, time.UTC) // Friday
	got := NextRuns("Green House 1", "C", specs, from, 3)
	if len(got) != 3 {
		t.Fatalf("got %d runs", len(got))
	}
	// Saturday first, then Monday, then Wednesday.
	wants := []time.Time{
		time.Date(2026, 8, 22, 5, 0, 0, 0, time.UTC),
		time.Date(2026, 8, 24, 5, 0, 0, 0, time.UTC),
		time.Date(2026, 8, 26, 5, 0, 0, 0, time.UTC),
	}
	for i, w := range wants {
		if !got[i].At.Equal(w) {
			t.Errorf("run %d at %v, want %v", i, got[i].At, w)
		}
	}
}

func TestNextRunsIgnoresEmptyWeekdayLists(t *testing.T) {
	specs := []ScheduleSpec{{Hour: 6, Duration: time.Hour}}
	if got := NextRuns("Off Season", "C", specs, time.Now(), 5); len(got) != 0 {
		t.Errorf("got %d runs for a zone with no days", len(got))
	}
}

func TestMergeUpcomingSortsChronologically(t *testing.T) {
	base := time.Date(2026, 8, 22, 5, 0, 0, 0, time.UTC)
	a := []Upcoming{{Zone: "A", At: base.Add(48 * time.Hour)}}
	b := []Upcoming{{Zone: "B", At: base}, {Zone: "B", At: base.Add(72 * time.Hour)}}
	got := MergeUpcoming(a, b)
	if len(got) != 3 {
		t.Fatalf("got %d", len(got))
	}
	if got[0].Zone != "B" || got[1].Zone != "A" || got[2].Zone != "B" {
		t.Errorf("order = %v", []string{got[0].Zone, got[1].Zone, got[2].Zone})
	}
}

func TestMissedScheduleIgnoresManualRuns(t *testing.T) {
	// Regression: Food Forest ran on schedule Thursday 06:00 and was then run
	// manually twice that afternoon. Deriving cadence from run gaps collapsed
	// the expected interval from a week to hours and flagged it LATE the next
	// day. The schedule says Thursday, so nothing is due until next Thursday.
	specs := []ScheduleSpec{{Hour: 6, Duration: 2 * time.Hour,
		Weekdays: []time.Weekday{time.Thursday}}}
	thu := time.Date(2026, 8, 20, 6, 0, 0, 0, time.UTC)
	runs := []Run{
		{Started: thu.AddDate(0, 0, -7)},
		{Started: thu},
		{Started: thu.Add(9 * time.Hour)},  // manual
		{Started: thu.Add(10 * time.Hour)}, // manual
	}
	friday := time.Date(2026, 8, 21, 15, 0, 0, 0, time.UTC)
	if MissedSchedule(specs, runs, friday) {
		t.Error("zone flagged late the day after running on schedule")
	}
}

func TestMissedScheduleCatchesARealMiss(t *testing.T) {
	specs := []ScheduleSpec{{Hour: 6, Duration: 2 * time.Hour,
		Weekdays: []time.Weekday{time.Thursday}}}
	// Last run was two Thursdays ago; this Thursday came and went.
	runs := []Run{{Started: time.Date(2026, 8, 13, 6, 0, 0, 0, time.UTC)}}
	friday := time.Date(2026, 8, 21, 15, 0, 0, 0, time.UTC)
	if !MissedSchedule(specs, runs, friday) {
		t.Error("a genuinely missed watering was not flagged")
	}
}

func TestMissedScheduleAllowsGraceAfterTheSlot(t *testing.T) {
	specs := []ScheduleSpec{{Hour: 6, Duration: time.Hour,
		Weekdays: []time.Weekday{time.Thursday}}}
	thu := time.Date(2026, 8, 20, 6, 0, 0, 0, time.UTC)
	// Ten minutes past the slot with no run yet is not yet a miss.
	if MissedSchedule(specs, nil, thu.Add(10*time.Minute)) {
		t.Error("flagged late inside the grace window")
	}
	if !MissedSchedule(specs, nil, thu.Add(2*time.Hour)) {
		t.Error("not flagged well past the grace window")
	}
}

func TestMissedScheduleQuietForUnscheduledZones(t *testing.T) {
	if MissedSchedule(nil, nil, time.Now()) {
		t.Error("a zone with no schedule was flagged late")
	}
	empty := []ScheduleSpec{{Hour: 6, Duration: time.Hour}}
	if MissedSchedule(empty, nil, time.Now()) {
		t.Error("a zone with an empty weekday list was flagged late")
	}
}

func TestPrevRunFindsTheMostRecentSlot(t *testing.T) {
	specs := []ScheduleSpec{{Hour: 5, Weekdays: []time.Weekday{
		time.Monday, time.Wednesday, time.Saturday}}}
	friday := time.Date(2026, 8, 21, 12, 0, 0, 0, time.UTC)
	got, ok := PrevRun(specs, friday)
	if !ok {
		t.Fatal("no previous slot found")
	}
	want := time.Date(2026, 8, 19, 5, 0, 0, 0, time.UTC) // Wednesday
	if !got.Equal(want) {
		t.Errorf("PrevRun = %v, want %v", got, want)
	}
}
