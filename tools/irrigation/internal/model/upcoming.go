package model

import (
	"sort"
	"time"
)

// Upcoming is one future watering.
type Upcoming struct {
	Zone       string
	Controller string
	At         time.Time
	Duration   time.Duration
}

func (u Upcoming) Ends() time.Time { return u.At.Add(u.Duration) }

// ScheduleSpec is the shape NextRuns needs from a zone's configuration. It is
// declared here so the model does not depend on the config reader.
type ScheduleSpec struct {
	Hour     int
	Minute   int
	Duration time.Duration
	Weekdays []time.Weekday
}

// NextRuns lists the next occurrences of one zone's schedules, soonest first.
//
// It is computed from the configured schedule rather than read from Home
// Assistant, because irrigation_unlimited's calendar entity reports only a
// single event and disagrees with the configured weekday.
func NextRuns(zone, controller string, specs []ScheduleSpec, from time.Time, limit int) []Upcoming {
	var out []Upcoming
	for _, sp := range specs {
		if len(sp.Weekdays) == 0 {
			continue // a schedule with no days never fires
		}
		for _, wd := range sp.Weekdays {
			at := nextWeekdayAt(from, wd, sp.Hour, sp.Minute)
			out = append(out, Upcoming{
				Zone: zone, Controller: controller, At: at, Duration: sp.Duration,
			})
		}
	}
	sort.Slice(out, func(i, j int) bool { return out[i].At.Before(out[j].At) })
	if limit > 0 && len(out) > limit {
		out = out[:limit]
	}
	return out
}

// nextWeekdayAt is the next occurrence of a weekday at a time of day, strictly
// after from. Today counts only if the time has not already passed.
func nextWeekdayAt(from time.Time, wd time.Weekday, hour, minute int) time.Time {
	days := (int(wd) - int(from.Weekday()) + 7) % 7
	candidate := time.Date(from.Year(), from.Month(), from.Day(), hour, minute, 0, 0, from.Location()).
		AddDate(0, 0, days)
	if !candidate.After(from) {
		candidate = candidate.AddDate(0, 0, 7)
	}
	return candidate
}

// MergeUpcoming flattens per-zone schedules into one chronological list.
func MergeUpcoming(lists ...[]Upcoming) []Upcoming {
	var all []Upcoming
	for _, l := range lists {
		all = append(all, l...)
	}
	sort.Slice(all, func(i, j int) bool {
		if all[i].At.Equal(all[j].At) {
			return all[i].Zone < all[j].Zone
		}
		return all[i].At.Before(all[j].At)
	})
	return all
}

// lateGrace is how long after a scheduled start a run may still appear before
// the zone counts as having missed it. Home Assistant fires on the minute, but
// a restart or a busy mesh can delay the state landing in the recorder.
const lateGrace = 30 * time.Minute

// PrevRun is the most recent scheduled occurrence at or before from.
func PrevRun(specs []ScheduleSpec, from time.Time) (time.Time, bool) {
	var best time.Time
	var found bool
	for _, sp := range specs {
		for _, wd := range sp.Weekdays {
			back := (int(from.Weekday()) - int(wd) + 7) % 7
			cand := time.Date(from.Year(), from.Month(), from.Day(), sp.Hour, sp.Minute, 0, 0, from.Location()).
				AddDate(0, 0, -back)
			if cand.After(from) {
				cand = cand.AddDate(0, 0, -7)
			}
			if !found || cand.After(best) {
				best, found = cand, true
			}
		}
	}
	return best, found
}

// MissedSchedule reports whether the most recent scheduled watering came and
// went without a run.
//
// This replaces inferring cadence from run history. History stopped being a
// safe proxy once ad-hoc runs existed: three manual runs in one afternoon
// collapse the apparent gap from a week to a few hours, and the zone is then
// flagged late the next morning for no reason.
func MissedSchedule(specs []ScheduleSpec, runs []Run, now time.Time) bool {
	due, ok := PrevRun(specs, now)
	if !ok {
		return false // nothing scheduled, so nothing to miss
	}
	if now.Before(due.Add(lateGrace)) {
		return false // still within the window it might start in
	}
	for _, r := range runs {
		if !r.Started.Before(due.Add(-5 * time.Minute)) {
			return false // something ran for this slot
		}
	}
	return true
}
