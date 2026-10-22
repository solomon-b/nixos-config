package tui

import (
	"strings"
	"testing"
	"time"

	"github.com/solomon-b/nixos-config/tools/irrigation/internal/model"
)

func day(y int, m time.Month, d int) time.Time {
	return time.Date(y, m, d, 6, 0, 0, 0, time.Local)
}

func mondaySpec() []model.ScheduleSpec {
	return []model.ScheduleSpec{{Hour: 6, Duration: 2 * time.Hour,
		Weekdays: []time.Weekday{time.Monday}}}
}

func TestDayIndexIgnoresClockTime(t *testing.T) {
	from := time.Date(2026, 8, 10, 23, 59, 0, 0, time.Local)
	to := time.Date(2026, 8, 11, 0, 1, 0, 0, time.Local)
	if got := dayIndex(from, to); got != 1 {
		t.Errorf("dayIndex = %d, want 1", got)
	}
}

func TestMondayOfAlignsToWeekStart(t *testing.T) {
	// 2026-08-24 is a Monday; 08-26 a Wednesday.
	if got := mondayOf(day(2026, 8, 26)); got.Day() != 24 {
		t.Errorf("mondayOf(Wed 08-26) = %v, want the 24th", got)
	}
	if got := mondayOf(day(2026, 8, 24)); got.Day() != 24 {
		t.Errorf("mondayOf(Mon) moved off its own week: %v", got)
	}
	// Sunday belongs to the week that started six days earlier.
	if got := mondayOf(day(2026, 8, 23)); got.Day() != 17 {
		t.Errorf("mondayOf(Sun 08-23) = %v, want the 17th", got)
	}
}

func TestZoneDaysMarksScheduledDays(t *testing.T) {
	from := mondayOf(day(2026, 8, 24))
	z := model.Zone{Specs: mondaySpec()}
	cells := zoneDays(z, from, 7, day(2026, 8, 26))
	if !cells[0].scheduled {
		t.Error("Monday not marked scheduled")
	}
	for i := 1; i < 7; i++ {
		if cells[i].scheduled {
			t.Errorf("day %d marked scheduled for a Monday-only zone", i)
		}
	}
}

func TestZoneDaysDistinguishesMissedFromNotDue(t *testing.T) {
	// Monday came and went with no run; Tuesday was never due.
	from := mondayOf(day(2026, 8, 24))
	z := model.Zone{Specs: mondaySpec()}
	cells := zoneDays(z, from, 7, day(2026, 8, 26))
	out := renderCells(cells)
	if !strings.Contains(out, glyphMissed) {
		t.Errorf("a missed watering did not render as %q:\n%s", glyphMissed, out)
	}
	if strings.Count(out, glyphMissed) != 1 {
		t.Errorf("expected exactly one miss, got %d", strings.Count(out, glyphMissed))
	}
}

func TestZoneDaysDoesNotCallFutureDaysMissed(t *testing.T) {
	// Sitting on Monday morning before the run: Monday is due, not missed.
	from := mondayOf(day(2026, 8, 24))
	z := model.Zone{Specs: mondaySpec()}
	cells := zoneDays(z, from, 7, day(2026, 8, 24))
	next := zoneDays(z, from, 14, day(2026, 8, 24))
	if strings.Contains(renderCells(cells[1:]), glyphMissed) {
		t.Error("a future day was marked missed")
	}
	if !strings.Contains(renderCells(next[7:]), glyphDue) {
		t.Error("next week's scheduled day not marked as upcoming")
	}
}

func TestZoneDaysBucketsRunsByDay(t *testing.T) {
	from := mondayOf(day(2026, 8, 24))
	z := model.Zone{Specs: mondaySpec(), Runs: []model.Run{
		{Started: day(2026, 8, 24), VolumeDelta: 219, HasVolume: true},
	}}
	cells := zoneDays(z, from, 7, day(2026, 8, 26))
	if !cells[0].ran || !cells[0].hasVolume {
		t.Errorf("Monday cell = %+v, want a run with volume", cells[0])
	}
	if strings.Contains(renderCells(cells), glyphMissed) {
		t.Error("a day that ran was marked missed")
	}
}

func TestRenderCellsSeparatesWeeks(t *testing.T) {
	from := mondayOf(day(2026, 8, 24))
	cells := zoneDays(model.Zone{}, from, 14, day(2026, 8, 26))
	out := stripANSI(renderCells(cells))
	if len([]rune(out)) != 15 {
		t.Errorf("two weeks rendered %d columns, want 14 plus one separator", len([]rune(out)))
	}
	if []rune(out)[7] != ' ' {
		t.Errorf("no separator between weeks: %q", out)
	}
}

func TestWeekdayHeaderRepeatsPerWeek(t *testing.T) {
	got := stripANSI(weekdayHeader(3, -1))
	if got != "MTWTFSS MTWTFSS MTWTFSS" {
		t.Errorf("weekdayHeader = %q", got)
	}
}

func TestWeekdayHeaderMarksTodayOnce(t *testing.T) {
	// Day 9 of a three week grid: second week, Wednesday.
	got := weekdayHeader(3, 9)
	marked := styleToday.Render("W")
	if !strings.Contains(got, marked) {
		t.Errorf("today not marked:\n%q", got)
	}
	if strings.Count(got, marked) != 1 {
		t.Errorf("today marked %d times, want 1", strings.Count(got, marked))
	}
	// Other Wednesdays stay plain.
	if strings.Count(stripANSI(got), "W") != 3 {
		t.Errorf("weekday letters changed: %q", stripANSI(got))
	}
}

func TestWeekdayHeaderWithTodayOutsideTheGrid(t *testing.T) {
	got := weekdayHeader(2, -1)
	if strings.Contains(got, styleToday.Render("M")) {
		t.Errorf("marked a day when today is outside the grid: %q", got)
	}
}

func TestWeekdayHeaderKeepsAlignmentWhenMarked(t *testing.T) {
	plain := stripANSI(weekdayHeader(3, -1))
	marked := stripANSI(weekdayHeader(3, 9))
	if plain != marked {
		t.Errorf("marking today changed the row: %q vs %q", plain, marked)
	}
}

func TestDateHeaderAlignsWithWeeks(t *testing.T) {
	from := mondayOf(day(2026, 8, 10))
	got := stripANSI(dateHeader(from, 2, day(2026, 8, 26)))
	if !strings.HasPrefix(got, "Aug10") {
		t.Errorf("dateHeader = %q, want it to start at the first week", got)
	}
	// The second label sits at the start of the second week.
	if idx := strings.Index(got, "Aug17"); idx != weekCols {
		t.Errorf("second label at %d, want %d", idx, weekCols)
	}
}

func TestCalendarWeeksCapsAtAvailableHistory(t *testing.T) {
	if got := calendarWeeks(300, 14); got != 2 {
		t.Errorf("calendarWeeks = %d, want 2 weeks of history", got)
	}
}

func TestCalendarWeeksCapsAtTerminalWidth(t *testing.T) {
	if got := calendarWeeks(80, 3650); got > 4 {
		t.Errorf("calendarWeeks = %d, too many for an 80 column terminal", got)
	}
}

func TestTruncMarksTheCut(t *testing.T) {
	if got := trunc("manifold_5", 20); got != "manifold_5" {
		t.Errorf("trunc shortened a string that fit: %q", got)
	}
	got := trunc("cactus_fence_irrigation_controller", 20)
	if len([]rune(got)) != 20 || !strings.HasSuffix(got, "…") {
		t.Errorf("trunc = %q, want 20 runes ending in ellipsis", got)
	}
}

func TestLegendCoversEveryGlyph(t *testing.T) {
	l := stripANSI(legend())
	for _, g := range []string{glyphRan, glyphNoVol, glyphMissed, glyphDue, glyphNoRun} {
		if !strings.Contains(l, g) {
			t.Errorf("legend missing glyph %q: %s", g, l)
		}
	}
	if !strings.Contains(l, "MISSED") {
		t.Errorf("legend does not name the missed case: %s", l)
	}
}
