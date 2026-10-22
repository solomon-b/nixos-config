package model

import "time"

type Run struct {
	Started     time.Time
	Ended       time.Time
	Minutes     float64
	VolumeDelta float64
	HasVolume   bool
}

// ActiveRun is a run in progress. Remaining time is derived from Start and
// Duration against the local clock rather than read from the recorder, so the
// countdown stays smooth between refreshes.
type ActiveRun struct {
	Start        time.Time
	Duration     time.Duration
	ScheduleName string
}

func (a ActiveRun) Ends() time.Time { return a.Start.Add(a.Duration) }

func (a ActiveRun) Remaining(now time.Time) time.Duration {
	left := a.Ends().Sub(now)
	if left < 0 {
		return 0
	}
	return left
}

func (a ActiveRun) Percent(now time.Time) int {
	if a.Duration <= 0 {
		return 0
	}
	p := int(now.Sub(a.Start) * 100 / a.Duration)
	if p < 0 {
		return 0
	}
	if p > 100 {
		return 100
	}
	return p
}

type Zone struct {
	Name         string
	SwitchEntity string
	SensorEntity string
	Index        int
	Scheduled    bool
	Specs        []ScheduleSpec
	TodayMinutes float64
	Status       string
	Active       *ActiveRun
	Runs         []Run
}

// Controller is one irrigation_unlimited controller and the zones it drives.
type Controller struct {
	Name  string
	Zones []Zone
}

type Valve struct {
	Device       string
	LastHeard    time.Time
	Battery      int
	HasBattery   bool
	LinkQuality  int
	HasLink      bool
	DeviceStatus string
	Flow         float64
	HasFlow      bool
	DailyVolume  float64
	HasDaily     bool
	// AutoClose is the valve shutting itself off when it believes there is no
	// water. On a valve whose flow meter under-reads, that turns a false
	// reading into a zone that silently does not water.
	AutoClose    bool
	HasAutoClose bool
	// AutoCloseEntity is the switch behind AutoClose. Held because the entity
	// may be named after the radio address rather than the friendly name.
	AutoCloseEntity string
	// Zone is the zone this valve drives, empty when it belongs to none.
	Zone     string
	Trend    BatteryTrend
	HasTrend bool
}

// IsValve distinguishes an irrigation valve from any other battery device on
// the mesh. Only the SWV valves report these metrics; a dimmer switch reports
// battery and link quality and nothing else.
func (v Valve) IsValve() bool {
	return v.HasFlow || v.HasDaily || v.HasAutoClose || v.DeviceStatus != ""
}
