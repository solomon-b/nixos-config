package model

import (
	"sort"
	"time"
)

// lateMultiplier is how far past the normal gap a zone may drift before it
// counts as late. Irrigation schedules are weekly, so some slack is normal.
const lateMultiplier = 1.5

// MedianGap returns the typical interval between runs. It reports ok=false
// when there is not enough history to establish one, which is how off-season
// zones stay quiet: no baseline means no expectation, so nothing to violate.
func MedianGap(runs []Run) (time.Duration, bool) {
	if len(runs) < 2 {
		return 0, false
	}
	sorted := append([]Run(nil), runs...)
	sort.Slice(sorted, func(i, j int) bool { return sorted[i].Started.Before(sorted[j].Started) })

	gaps := make([]time.Duration, 0, len(sorted)-1)
	for i := 1; i < len(sorted); i++ {
		gaps = append(gaps, sorted[i].Started.Sub(sorted[i-1].Started))
	}
	sort.Slice(gaps, func(i, j int) bool { return gaps[i] < gaps[j] })

	n := len(gaps)
	if n%2 == 1 {
		return gaps[n/2], true
	}
	return (gaps[n/2-1] + gaps[n/2]) / 2, true
}

// IsLate reports whether a zone has missed its expected watering. A zone with
// no established cadence is never late.
func IsLate(runs []Run, now time.Time) bool {
	gap, ok := MedianGap(runs)
	if !ok {
		return false
	}
	var last time.Time
	for _, r := range runs {
		if r.Started.After(last) {
			last = r.Started
		}
	}
	threshold := time.Duration(float64(gap) * lateMultiplier)
	return now.Sub(last) > threshold
}

// VolumeBaseline is the mean volume delta across runs that reported one.
// The unit is meaningless in absolute terms, because these valves under-report
// at drip-emitter rates. It is useful only as each zone's own normal.
func VolumeBaseline(runs []Run) (float64, bool) {
	var sum float64
	var n int
	for _, r := range runs {
		if r.HasVolume {
			sum += r.VolumeDelta
			n++
		}
	}
	if n == 0 {
		return 0, false
	}
	return sum / float64(n), true
}
