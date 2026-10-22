package model

import "time"

// Sample is one timestamped reading.
type Sample struct {
	At    time.Time
	Value float64
}

// minTrendSpan is how much history a drain rate needs before it means
// anything. These valves report battery only on change, so a short window can
// hold two readings a day apart and imply an absurd slope.
const minTrendSpan = 7 * 24 * time.Hour

// maxProjectionDays is the horizon past which a drain projection stops being
// actionable.
const maxProjectionDays = 365

// BatteryTrend is a fitted drain rate. Absent when history is too thin or the
// level is not actually falling.
type BatteryTrend struct {
	PerDay float64 // negative while draining
	Empty  time.Time
}

// FitBattery estimates a drain rate from readings. It reports ok=false rather
// than guessing: a wrong "empty next week" is worse than no estimate.
func FitBattery(samples []Sample) (BatteryTrend, bool) {
	if len(samples) < 3 {
		return BatteryTrend{}, false
	}
	first, last := samples[0], samples[len(samples)-1]
	span := last.At.Sub(first.At)
	if span < minTrendSpan {
		return BatteryTrend{}, false
	}
	drop := last.Value - first.Value
	if drop >= 0 {
		return BatteryTrend{}, false // flat or charging, nothing to project
	}
	perDay := drop / (span.Hours() / 24)
	daysLeft := last.Value / -perDay
	// Past a year this is noise amplified by a long lever arm, not a forecast.
	// A 1% dip over ten days projects centuries out and helps nobody.
	if daysLeft > maxProjectionDays {
		return BatteryTrend{}, false
	}
	return BatteryTrend{
		PerDay: perDay,
		Empty:  last.At.Add(time.Duration(daysLeft * 24 * float64(time.Hour))),
	}, true
}

// AttachVolume fills in each run's volume delta from its own valve's counter.
//
// Correlation has to happen here rather than in SQL, because only the caller
// knows which valve drives which zone. Matching readings to runs by time alone
// mixes counters the moment two zones run at once, and these counters are
// cumulative, so mixing them produces a delta in the tens of thousands.
func AttachVolume(runs []Run, samples []Sample) []Run {
	out := make([]Run, len(runs))
	copy(out, runs)
	for i, r := range out {
		var lo, hi float64
		var n int
		for _, s := range samples {
			if s.At.Before(r.Started) || s.At.After(r.Ended) {
				continue
			}
			if n == 0 || s.Value < lo {
				lo = s.Value
			}
			if n == 0 || s.Value > hi {
				hi = s.Value
			}
			n++
		}
		// Two samples minimum. A single reading yields a delta of zero, which is
		// indistinguishable from a zone that ran without moving water.
		if n > 1 {
			out[i].VolumeDelta = hi - lo
			out[i].HasVolume = true
		}
	}
	return out
}
