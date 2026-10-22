package hass

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"time"
)

// Day is one day of forecast.
type Day struct {
	Date          time.Time
	Condition     string
	High          float64
	Low           float64
	Precipitation float64
	Humidity      float64
}

// Forecast fetches the daily outlook. It uses the service-response form of
// weather.get_forecasts, because the weather entity's own attributes carry
// current conditions only, with no precipitation ahead.
func (c *Client) Forecast(ctx context.Context, entityID string) ([]Day, error) {
	body, _ := json.Marshal(map[string]any{"entity_id": entityID, "type": "daily"})
	url := fmt.Sprintf("%s/api/services/weather/get_forecasts?return_response=true", c.BaseURL)
	req, err := http.NewRequestWithContext(ctx, http.MethodPost, url, bytes.NewReader(body))
	if err != nil {
		return nil, err
	}
	req.Header.Set("Authorization", "Bearer "+c.Token)
	req.Header.Set("Content-Type", "application/json")

	resp, err := c.HTTP.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	if resp.StatusCode < 200 || resp.StatusCode > 299 {
		return nil, fmt.Errorf("get_forecasts: %s", resp.Status)
	}

	var payload struct {
		ServiceResponse map[string]struct {
			Forecast []struct {
				Datetime      string  `json:"datetime"`
				Condition     string  `json:"condition"`
				Temperature   float64 `json:"temperature"`
				TempLow       float64 `json:"templow"`
				Precipitation float64 `json:"precipitation"`
				Humidity      float64 `json:"humidity"`
			} `json:"forecast"`
		} `json:"service_response"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&payload); err != nil {
		return nil, err
	}
	entry, ok := payload.ServiceResponse[entityID]
	if !ok {
		return nil, fmt.Errorf("no forecast returned for %s", entityID)
	}

	out := make([]Day, 0, len(entry.Forecast))
	for _, f := range entry.Forecast {
		t, err := time.Parse(time.RFC3339, f.Datetime)
		if err != nil {
			continue
		}
		out = append(out, Day{
			Date: t.Local(), Condition: f.Condition, High: f.Temperature,
			Low: f.TempLow, Precipitation: f.Precipitation, Humidity: f.Humidity,
		})
	}
	return out, nil
}

// WetEnoughToSkip is the rainfall over the lookahead window that makes running
// the irrigation questionable. In inches, matching this system's units.
const WetEnoughToSkip = 0.1

// RainAhead totals forecast precipitation over the next days.
func RainAhead(days []Day, within int) float64 {
	var total float64
	for i, d := range days {
		if i >= within {
			break
		}
		total += d.Precipitation
	}
	return total
}
