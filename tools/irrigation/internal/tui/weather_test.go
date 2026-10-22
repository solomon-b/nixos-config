package tui

import (
	"strings"
	"testing"
	"time"

	"github.com/solomon-b/nixos-config/tools/irrigation/internal/hass"
	"github.com/solomon-b/nixos-config/tools/irrigation/internal/model"
)

func forecast(days ...hass.Day) []hass.Day {
	base := time.Now()
	for i := range days {
		days[i].Date = base.AddDate(0, 0, i)
	}
	return days
}

func TestWeatherLineHiddenWithoutAForecast(t *testing.T) {
	m := New(one(sampleZone()), nil, "", nil)
	if got := m.weatherLine(); got != "" {
		t.Errorf("weatherLine = %q, want empty", got)
	}
	if strings.Contains(m.View(), "no rain") {
		t.Error("weather strip rendered with no forecast")
	}
}

func TestWeatherLineSuggestsSkippingWhenRainIsComing(t *testing.T) {
	m := New(one(sampleZone()), nil, "", nil).WithForecast(forecast(
		hass.Day{Condition: "rainy", High: 70, Low: 55, Precipitation: 0.3},
		hass.Day{Condition: "rainy", High: 68, Low: 54, Precipitation: 0.2},
	))
	got := m.weatherLine()
	if !strings.Contains(got, "consider skipping") {
		t.Errorf("no skip suggestion despite rain: %q", got)
	}
	if !strings.Contains(got, "0.50in") {
		t.Errorf("rain total not shown: %q", got)
	}
}

func TestWeatherLineIgnoresTraceRain(t *testing.T) {
	m := New(one(sampleZone()), nil, "", nil).WithForecast(forecast(
		hass.Day{Condition: "cloudy", High: 80, Low: 60, Precipitation: 0.01},
		hass.Day{Condition: "cloudy", High: 80, Low: 60, Precipitation: 0.0},
	))
	if got := m.weatherLine(); strings.Contains(got, "consider skipping") {
		t.Errorf("suggested skipping for a trace of rain: %q", got)
	}
}

func TestWeatherLineFlagsHeat(t *testing.T) {
	m := New(one(sampleZone()), nil, "", nil).WithForecast(forecast(
		hass.Day{Condition: "sunny", High: 98, Low: 85},
		hass.Day{Condition: "sunny", High: 102, Low: 80},
		hass.Day{Condition: "sunny", High: 105, Low: 80},
	))
	got := m.weatherLine()
	if !strings.Contains(got, "over 95") {
		t.Errorf("heat not flagged: %q", got)
	}
	if strings.Contains(got, "consider skipping") {
		t.Errorf("suggested skipping during a dry heatwave: %q", got)
	}
}

func TestWeatherLineSurvivesAShortForecast(t *testing.T) {
	// A one-day forecast must not index past the end.
	m := New([]model.Controller{{Name: "C"}}, nil, "", nil).
		WithForecast(forecast(hass.Day{Condition: "sunny", High: 90, Low: 70}))
	if got := m.weatherLine(); got == "" {
		t.Error("one-day forecast produced no line")
	}
}
