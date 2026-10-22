package hass

import (
	"context"
	"net/http"
	"net/http/httptest"
	"testing"
)

// Trimmed from a real weather.get_forecasts response.
const forecastBody = `{"changed_states":[],"service_response":{"weather.forecast_home":{"forecast":[
{"condition":"sunny","datetime":"2026-08-20T19:00:00+00:00","temperature":98,"templow":85,"precipitation":0.0,"humidity":26},
{"condition":"rainy","datetime":"2026-08-21T19:00:00+00:00","temperature":102,"templow":80,"precipitation":0.4,"humidity":24}]}}}`

func TestForecastParsesServiceResponse(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Query().Get("return_response") != "true" {
			t.Errorf("missing return_response, got %q", r.URL.RawQuery)
		}
		w.Write([]byte(forecastBody))
	}))
	defer srv.Close()

	days, err := New(srv.URL, "t").Forecast(context.Background(), "weather.forecast_home")
	if err != nil {
		t.Fatalf("Forecast: %v", err)
	}
	if len(days) != 2 {
		t.Fatalf("days = %d, want 2", len(days))
	}
	if days[0].Condition != "sunny" || days[0].High != 98 {
		t.Errorf("day 0 = %+v", days[0])
	}
	if days[1].Precipitation != 0.4 {
		t.Errorf("precipitation = %v, want 0.4", days[1].Precipitation)
	}
}

func TestForecastErrorsOnUnknownEntity(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte(`{"service_response":{}}`))
	}))
	defer srv.Close()
	if _, err := New(srv.URL, "t").Forecast(context.Background(), "weather.nope"); err == nil {
		t.Error("expected an error for an entity with no forecast")
	}
}

func TestRainAheadRespectsWindow(t *testing.T) {
	days := []Day{{Precipitation: 0.1}, {Precipitation: 0.4}, {Precipitation: 2.0}}
	if got := RainAhead(days, 2); got != 0.5 {
		t.Errorf("RainAhead(2) = %v, want 0.5", got)
	}
	if got := RainAhead(days, 10); got != 2.5 {
		t.Errorf("RainAhead(10) = %v, want 2.5", got)
	}
	if got := RainAhead(nil, 2); got != 0 {
		t.Errorf("RainAhead(nil) = %v, want 0", got)
	}
}
