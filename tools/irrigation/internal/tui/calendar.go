package tui

import (
	"strings"
	"time"

	"github.com/solomon-b/nixos-config/tools/irrigation/internal/model"
)

// dayCell is one zone on one day.
type dayCell struct {
	ran       bool
	hasVolume bool
	volume    float64
	scheduled bool
	future    bool
}

// Glyphs. The important distinction is the last two: a day the zone was due
// and did not run looks nothing like a day it was never due. Before this they
// were the same dot, which made a missed watering invisible in the one view
// meant to reveal it.
const (
	glyphRan     = "█" // ran, volume confirms water moved
	glyphNoVol   = "▪" // ran, meter said nothing
	glyphMissed  = "✗" // was due, did not run
	glyphDue     = "◦" // due later today or in future
	glyphNoRun   = "·" // not due
	daysPerWeek  = 7
	weekGap      = 1 // blank column between weeks, so the eye can count
	weekCols     = daysPerWeek + weekGap
	nameCol      = 22
	statusCol    = 15 // wide enough for "RUNNING 1h59m"
	nextCol      = 9 // "Mon 06:00"
	weekdayLine  = "MTWTFSS"
)

// dayIndex counts calendar days from one date to another, ignoring clock time.
func dayIndex(from, to time.Time) int {
	fy, fm, fd := from.Date()
	ty, tm, td := to.Date()
	a := time.Date(fy, fm, fd, 0, 0, 0, 0, time.Local)
	b := time.Date(ty, tm, td, 0, 0, 0, 0, time.Local)
	return int(b.Sub(a).Hours() / 24)
}

// mondayOf is the start of the week containing t.
func mondayOf(t time.Time) time.Time {
	back := (int(t.Weekday()) + 6) % 7 // Monday = 0
	y, m, d := t.Date()
	return time.Date(y, m, d, 0, 0, 0, 0, t.Location()).AddDate(0, 0, -back)
}

// zoneDays buckets a zone's runs into n daily cells starting at from, and marks
// which of those days the schedule called for.
func zoneDays(z model.Zone, from time.Time, n int, now time.Time) []dayCell {
	cells := make([]dayCell, n)
	today := dayIndex(from, now)

	for i := range cells {
		day := from.AddDate(0, 0, i)
		cells[i].scheduled = scheduledOn(z.Specs, day.Weekday())
		cells[i].future = i > today
	}
	for _, r := range z.Runs {
		i := dayIndex(from, r.Started)
		if i < 0 || i >= n {
			continue
		}
		cells[i].ran = true
		if r.HasVolume {
			cells[i].hasVolume = true
			cells[i].volume += r.VolumeDelta
		}
	}
	return cells
}

func scheduledOn(specs []model.ScheduleSpec, wd time.Weekday) bool {
	for _, sp := range specs {
		for _, d := range sp.Weekdays {
			if d == wd {
				return true
			}
		}
	}
	return false
}

// renderCells draws one zone's row, with a blank column between weeks.
func renderCells(cells []dayCell) string {
	var b strings.Builder
	for i, c := range cells {
		if i > 0 && i%daysPerWeek == 0 {
			b.WriteString(" ")
		}
		switch {
		case c.ran && c.hasVolume:
			b.WriteString(styleRan.Render(glyphRan))
		case c.ran:
			b.WriteString(styleRanNoVol.Render(glyphNoVol))
		case c.future && c.scheduled:
			b.WriteString(styleDim.Render(glyphDue))
		case c.scheduled:
			// Due and never ran. This is the whole point of the view.
			b.WriteString(styleBad.Render(glyphMissed))
		default:
			b.WriteString(styleNoRun.Render(glyphNoRun))
		}
	}
	return b.String()
}

// weekdayHeader repeats MTWTFSS across the grid so a column can be read off
// without counting from a distant date label. today is the day index of the
// current day within the grid, or -1 when it falls outside.
func weekdayHeader(weeks, today int) string {
	var b strings.Builder
	for i := 0; i < weeks*daysPerWeek; i++ {
		if i > 0 && i%daysPerWeek == 0 {
			b.WriteString(" ")
		}
		letter := string(weekdayLine[i%daysPerWeek])
		if i == today {
			b.WriteString(styleToday.Render(letter))
			continue
		}
		b.WriteString(styleDim.Render(letter))
	}
	return b.String()
}

// dateHeader puts one date above each week.
// dateHeader puts one date above each week, accenting the current one.
func dateHeader(from time.Time, weeks int, now time.Time) string {
	thisWeek := mondayOf(now)
	var b strings.Builder
	for w := 0; w < weeks; w++ {
		start := from.AddDate(0, 0, w*daysPerWeek)
		label := start.Format("Jan02")
		// Each week occupies 7 columns plus a separator; the label is 5 wide.
		pad := strings.Repeat(" ", weekCols-len([]rune(label)))
		if start.Equal(thisWeek) {
			b.WriteString(styleHeader.Render(label))
		} else {
			b.WriteString(styleDim.Render(label))
		}
		b.WriteString(pad)
	}
	return strings.TrimRight(b.String(), " ")
}

// calendarWeeks is how many whole weeks fit, bounded by the history held.
func calendarWeeks(width, maxDays int) int {
	avail := width - nameCol - statusCol - nextCol - 6
	weeks := avail / weekCols
	if weeks < 1 {
		weeks = 1
	}
	if maxWeeks := maxDays / daysPerWeek; maxWeeks < weeks {
		weeks = maxWeeks
	}
	if weeks < 1 {
		weeks = 1
	}
	return weeks
}

// CalendarWidth is the rendered width of a grid of the given week count.
func CalendarWidth(weeks int) int {
	return nameCol + weeks*weekCols - weekGap + 2 + statusCol + nextCol
}

// trunc shortens s to n columns, marking the cut.
func trunc(s string, n int) string {
	if len(s) <= n {
		return s
	}
	if n <= 1 {
		return s[:max(n, 0)]
	}
	return s[:n-1] + "…"
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

// legend explains the glyphs. Without it the grid is a cipher: the difference
// between "was due and did not run" and "was never due" is the whole point,
// and neither is guessable from a symbol.
func legend() string {
	parts := []string{
		styleRan.Render(glyphRan) + styleDim.Render(" ran, water seen"),
		styleRanNoVol.Render(glyphNoVol) + styleDim.Render(" ran, no reading"),
		styleBad.Render(glyphMissed) + styleDim.Render(" MISSED"),
		styleDim.Render(glyphDue + " due"),
		styleNoRun.Render(glyphNoRun) + styleDim.Render(" not due"),
	}
	return strings.Join(parts, styleDim.Render("   "))
}
