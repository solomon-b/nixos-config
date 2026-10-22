package tui

import (
	"strings"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/solomon-b/nixos-config/tools/irrigation/internal/model"
)

func scheduledControllers() []model.Controller {
	return []model.Controller{{Name: "Middle Garden", Zones: []model.Zone{
		{Name: "Cactus Fence", Scheduled: true, Specs: []model.ScheduleSpec{
			{Hour: 6, Duration: 2 * time.Hour, Weekdays: []time.Weekday{time.Monday}}}},
		{Name: "Green House 1", Scheduled: true, Specs: []model.ScheduleSpec{
			{Hour: 5, Duration: time.Hour, Weekdays: []time.Weekday{
				time.Monday, time.Wednesday, time.Saturday}}}},
		{Name: "Green House 2", Scheduled: false, Specs: []model.ScheduleSpec{
			{Hour: 6, Duration: time.Hour}}},
	}}}
}

func TestUpcomingListsSoonestFirst(t *testing.T) {
	from := time.Date(2026, 8, 21, 12, 0, 0, 0, time.UTC) // Friday
	up := BuildUpcoming(scheduledControllers(), from, 3, 20)
	if len(up) == 0 {
		t.Fatal("no upcoming runs computed")
	}
	for i := 1; i < len(up); i++ {
		if up[i].At.Before(up[i-1].At) {
			t.Fatalf("run %d is earlier than %d", i, i-1)
		}
	}
	// Saturday's Green House 1 comes before Monday's Cactus Fence.
	if up[0].Zone != "Green House 1" {
		t.Errorf("first upcoming = %s, want Green House 1", up[0].Zone)
	}
}

func TestUpcomingOmitsUnscheduledZones(t *testing.T) {
	from := time.Date(2026, 8, 21, 12, 0, 0, 0, time.UTC)
	for _, u := range BuildUpcoming(scheduledControllers(), from, 3, 20) {
		if u.Zone == "Green House 2" {
			t.Error("a zone with an empty weekday list appeared in the schedule")
		}
	}
}

func TestUpcomingViewRendersRows(t *testing.T) {
	from := time.Date(2026, 8, 21, 12, 0, 0, 0, time.UTC)
	ctrls := scheduledControllers()
	m := New(ctrls, nil, "", nil).WithUpcoming(BuildUpcoming(ctrls, from, 3, 20))
	m.now = from
	n, _ := m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'u'}})
	v := n.(Model).View()

	for _, want := range []string{"UPCOMING", "WHEN", "WINDOW", "FOR", "ZONE", "Cactus Fence"} {
		if !strings.Contains(v, want) {
			t.Errorf("upcoming view missing %q:\n%s", want, v)
		}
	}
}

func TestUpcomingViewRepeatsNoDateWithinADay(t *testing.T) {
	// Monday has two runs; the date should appear once.
	from := time.Date(2026, 8, 21, 12, 0, 0, 0, time.UTC)
	ctrls := scheduledControllers()
	m := New(ctrls, nil, "", nil).WithUpcoming(BuildUpcoming(ctrls, from, 3, 20))
	m.now = from
	n, _ := m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'u'}})
	if got := strings.Count(n.(Model).View(), "Mon 08-24"); got != 1 {
		t.Errorf("date shown %d times for one day, want 1", got)
	}
}

func TestUpcomingViewExplainsAnEmptySchedule(t *testing.T) {
	idle := []model.Controller{{Name: "C", Zones: []model.Zone{
		{Name: "Off", Specs: []model.ScheduleSpec{{Hour: 6}}}}}}
	m := New(idle, nil, "", nil).WithUpcoming(BuildUpcoming(idle, time.Now(), 3, 20))
	n, _ := m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'u'}})
	v := n.(Model).View()
	if !strings.Contains(v, "nothing scheduled") {
		t.Errorf("empty schedule not explained:\n%s", v)
	}
}

func TestZoneTableShowsNextRunPerZone(t *testing.T) {
	from := time.Date(2026, 8, 27, 12, 0, 0, 0, time.UTC) // Thursday
	ctrls := scheduledControllers()
	m := New(ctrls, nil, "", nil)
	m.now = from
	v := m.View()

	if !strings.Contains(v, "NEXT") {
		t.Errorf("NEXT column heading missing:\n%s", v)
	}
	// Cactus Fence is Mondays at 06:00.
	if !strings.Contains(v, "Mon 06:00") {
		t.Errorf("next run time not shown for a scheduled zone:\n%s", v)
	}
	// A zone with no schedule has no next run to show.
	lines := strings.Split(v, "\n")
	for _, ln := range lines {
		if strings.Contains(ln, "Green House 2") && strings.Contains(ln, ":") {
			t.Errorf("unscheduled zone showed a next run:\n%s", ln)
		}
	}
}
