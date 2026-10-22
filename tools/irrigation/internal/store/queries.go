package store

import (
	"context"
	"fmt"
	"sort"
	"strconv"
	"strings"
	"time"

	"github.com/solomon-b/nixos-config/tools/irrigation/internal/model"
)

// runsSQL pairs each on-edge with the following off-edge to produce run
// windows.
//
// The LAG filter is essential. irrigation_unlimited rewrites its attributes
// constantly, and the recorder writes a states row for every rewrite. Counting
// rows where state='on' reports roughly 100x the true run count. Only edges,
// where the state differs from the previous row, are real transitions.
const runsSQL = `
WITH ev AS (
  SELECT m.entity_id, s.state, s.last_updated_ts AS ts, lag(s.state) OVER w AS prev
  FROM states s
  JOIN states_meta m ON m.metadata_id = s.metadata_id
  WHERE m.entity_id ~ '^binary_sensor\.irrigation_unlimited_c[0-9]+_z[0-9]+$'
    AND s.state IN ('on','off')
    AND s.last_updated_ts > $1
  WINDOW w AS (PARTITION BY m.entity_id ORDER BY s.last_updated_ts)
),
edges AS (
  SELECT entity_id, state, ts FROM ev WHERE prev IS DISTINCT FROM state
),
runs AS (
  SELECT entity_id, ts AS started,
         lead(ts) OVER (PARTITION BY entity_id ORDER BY ts) AS ended,
         state
  FROM edges
),
zone_runs AS (
  SELECT entity_id, started, ended FROM runs WHERE state = 'on' AND ended IS NOT NULL
)
SELECT entity_id, started::float8, ended::float8
FROM zone_runs
ORDER BY 1, 2`

func (s *Store) Runs(ctx context.Context, since time.Time) (map[string][]model.Run, error) {
	rows, err := s.pool.Query(ctx, runsSQL, float64(since.Unix()))
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	out := map[string][]model.Run{}
	for rows.Next() {
		var entity string
		var started, ended float64
		if err := rows.Scan(&entity, &started, &ended); err != nil {
			return nil, err
		}
		// Volume is attached later, in Go, where the zone-to-valve mapping from
		// configuration.yaml is known. Correlating it here by time alone mixes
		// counters whenever two zones run at once.
		out[entity] = append(out[entity], model.Run{
			Started: time.Unix(int64(started), 0),
			Ended:   time.Unix(int64(ended), 0),
			Minutes: (ended - started) / 60.0,
		})
	}
	return out, rows.Err()
}

type ZoneState struct {
	FriendlyName string
	Status       string
	TodayMinutes float64
	// Set only while a run is in progress.
	CurrentStart    string
	CurrentDuration string
	CurrentName     string
}

// zoneStateSQL reads the newest attribute payload per zone sensor.
// today_total is Home Assistant's own count of minutes run today, which is
// more trustworthy than anything derived from these valves' flow sensors.
const zoneStateSQL = `
SELECT DISTINCT ON (m.entity_id)
       m.entity_id,
       COALESCE(sa.shared_attrs::jsonb->>'friendly_name', ''),
       COALESCE(sa.shared_attrs::jsonb->>'status', ''),
       COALESCE((sa.shared_attrs::jsonb->>'today_total')::float8, 0),
       COALESCE(sa.shared_attrs::jsonb->>'current_start', ''),
       COALESCE(sa.shared_attrs::jsonb->>'current_duration', ''),
       COALESCE(sa.shared_attrs::jsonb->>'current_name', '')
FROM states s
JOIN states_meta m ON m.metadata_id = s.metadata_id
JOIN state_attributes sa ON sa.attributes_id = s.attributes_id
WHERE m.entity_id ~ '^binary_sensor\.irrigation_unlimited_c[0-9]+_z[0-9]+$'
ORDER BY m.entity_id, s.last_updated_ts DESC`

func (s *Store) ZoneState(ctx context.Context) (map[string]ZoneState, error) {
	rows, err := s.pool.Query(ctx, zoneStateSQL)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	out := map[string]ZoneState{}
	for rows.Next() {
		var entity string
		var st ZoneState
		if err := rows.Scan(&entity, &st.FriendlyName, &st.Status, &st.TodayMinutes,
			&st.CurrentStart, &st.CurrentDuration, &st.CurrentName); err != nil {
			return nil, err
		}
		out[entity] = st
	}
	return out, rows.Err()
}

// valvesSQL reports the newest reading of each device metric, and when the
// device was last heard from at all.
//
// It spans sensor and switch because auto_close_when_water_shortage is a
// switch, and it is queried by a caller-supplied key list so a valve split
// across slug and address entity ids can be gathered under both.
const valveMetrics = `(battery|linkquality|current_device_status|flow|daily_irrigation_volume|auto_close_when_water_shortage)`

const valvesSQL = `
WITH s AS (
  SELECT regexp_replace(m.entity_id, '^(sensor|switch)\.(.*)_` + valveMetrics + `$', '\2') AS device,
         regexp_replace(m.entity_id, '^(sensor|switch)\.(.*)_` + valveMetrics + `$', '\3') AS metric,
         st.state, st.last_updated_ts AS ts, m.entity_id
  FROM states st
  JOIN states_meta m ON m.metadata_id = st.metadata_id
  WHERE m.entity_id ~ '^(sensor|switch)\.(.*)_` + valveMetrics + `$'
    AND st.state NOT IN ('unknown','unavailable')
),
newest AS (
  SELECT DISTINCT ON (device, metric) device, metric, state, ts, entity_id
  FROM s ORDER BY device, metric, ts DESC
)
SELECT device,
       max(ts)::float8 AS last_heard,
       max(state) FILTER (WHERE metric = 'battery') AS battery,
       max(state) FILTER (WHERE metric = 'linkquality') AS linkquality,
       COALESCE(max(state) FILTER (WHERE metric = 'current_device_status'), '') AS device_status,
       max(state) FILTER (WHERE metric = 'flow') AS flow,
       max(state) FILTER (WHERE metric = 'daily_irrigation_volume') AS daily_volume,
       max(state) FILTER (WHERE metric = 'auto_close_when_water_shortage') AS auto_close,
       max(entity_id) FILTER (WHERE metric = 'auto_close_when_water_shortage') AS auto_close_entity
FROM newest
WHERE $1::text[] IS NULL OR device = ANY($1)
GROUP BY device
ORDER BY device`

// Valves gathers device metadata for every key in keys, then folds each row
// onto the canonical device name given by canonical. That merge is what makes a
// valve whole again when Home Assistant filed half its entities under the radio
// address and half under the friendly name.
func (s *Store) Valves(ctx context.Context, keys []string, canonical map[string]string) ([]model.Valve, error) {
	rows, err := s.pool.Query(ctx, valvesSQL, keys)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	merged := map[string]*model.Valve{}
	for rows.Next() {
		var key, status string
		var lastHeard float64
		var battery, link, flow, daily, autoClose, autoCloseEntity *string
		if err := rows.Scan(&key, &lastHeard, &battery, &link, &status, &flow, &daily,
			&autoClose, &autoCloseEntity); err != nil {
			return nil, err
		}

		name := key
		if c, ok := canonical[key]; ok {
			name = c
		}
		v, ok := merged[name]
		if !ok {
			v = &model.Valve{Device: name}
			merged[name] = v
		}

		if t := time.Unix(int64(lastHeard), 0); t.After(v.LastHeard) {
			v.LastHeard = t
		}
		if battery != nil {
			if n, err := parseIntPrefix(*battery); err == nil {
				v.Battery, v.HasBattery = n, true
			}
		}
		if link != nil {
			if n, err := parseIntPrefix(*link); err == nil {
				v.LinkQuality, v.HasLink = n, true
			}
		}
		if status != "" {
			v.DeviceStatus = status
		}
		if flow != nil {
			if f, err := strconv.ParseFloat(*flow, 64); err == nil {
				v.Flow, v.HasFlow = f, true
			}
		}
		if daily != nil {
			if f, err := strconv.ParseFloat(*daily, 64); err == nil {
				v.DailyVolume, v.HasDaily = f, true
			}
		}
		if autoClose != nil {
			v.AutoClose, v.HasAutoClose = *autoClose == "on", true
		}
		if autoCloseEntity != nil {
			v.AutoCloseEntity = *autoCloseEntity
		}
	}
	if err := rows.Err(); err != nil {
		return nil, err
	}

	out := make([]model.Valve, 0, len(merged))
	for _, v := range merged {
		out = append(out, *v)
	}
	sort.Slice(out, func(i, j int) bool { return out[i].Device < out[j].Device })
	return out, nil
}

// batteryHistorySQL returns every battery reading, oldest first, so a drain
// rate can be fitted.
const batteryHistorySQL = `
SELECT regexp_replace(m.entity_id, '^sensor\.(.*)_battery$', '\1') AS device,
       s.last_updated_ts::float8 AS ts,
       s.state::float8 AS pct
FROM states s
JOIN states_meta m ON m.metadata_id = s.metadata_id
WHERE m.entity_id ~ '^sensor\..*_battery$'
  AND s.state ~ '^[0-9]+(\.[0-9]+)?$'
ORDER BY 1, 2`

// BatteryHistory returns readings per device, folded onto canonical names.
func (s *Store) BatteryHistory(ctx context.Context, canonical map[string]string) (map[string][]model.Sample, error) {
	rows, err := s.pool.Query(ctx, batteryHistorySQL)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	out := map[string][]model.Sample{}
	for rows.Next() {
		var key string
		var ts, pct float64
		if err := rows.Scan(&key, &ts, &pct); err != nil {
			return nil, err
		}
		name := key
		if c, ok := canonical[key]; ok {
			name = c
		}
		out[name] = append(out[name], model.Sample{At: time.Unix(int64(ts), 0), Value: pct})
	}
	if err := rows.Err(); err != nil {
		return nil, err
	}
	for k := range out {
		sort.Slice(out[k], func(i, j int) bool { return out[k][i].At.Before(out[k][j].At) })
	}
	return out, nil
}

// volumeSeriesSQL returns each valve's cumulative volume readings, per device,
// so a run's delta can be taken against the valve that actually drove it.
const volumeSeriesSQL = `
SELECT regexp_replace(m.entity_id, '^sensor\.(.*)_real_time_irrigation_volume$', '\1') AS device,
       s.last_updated_ts::float8 AS ts,
       s.state::float8 AS v
FROM states s
JOIN states_meta m ON m.metadata_id = s.metadata_id
WHERE m.entity_id ~ '_real_time_irrigation_volume$'
  AND s.state ~ '^[0-9]+(\.[0-9]+)?$'
  AND s.last_updated_ts > $1
ORDER BY 1, 2`

// VolumeSeries returns cumulative volume readings keyed by device.
func (s *Store) VolumeSeries(ctx context.Context, since time.Time, canonical map[string]string) (map[string][]model.Sample, error) {
	rows, err := s.pool.Query(ctx, volumeSeriesSQL, float64(since.Unix()))
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	out := map[string][]model.Sample{}
	for rows.Next() {
		var key string
		var ts, v float64
		if err := rows.Scan(&key, &ts, &v); err != nil {
			return nil, err
		}
		name := key
		if c, ok := canonical[key]; ok {
			name = c
		}
		out[name] = append(out[name], model.Sample{At: time.Unix(int64(ts), 0), Value: v})
	}
	if err := rows.Err(); err != nil {
		return nil, err
	}
	for k := range out {
		sort.Slice(out[k], func(i, j int) bool { return out[k][i].At.Before(out[k][j].At) })
	}
	return out, nil
}

// OldestState reports the earliest row the recorder still holds. The calendar
// must not draw days before this point: an empty cell there would read as "did
// not run" when the truth is "purged, unknown".
func (s *Store) OldestState(ctx context.Context) (time.Time, error) {
	var ts float64
	err := s.pool.QueryRow(ctx, `SELECT COALESCE(min(last_updated_ts), 0)::float8 FROM states`).Scan(&ts)
	if err != nil {
		return time.Time{}, err
	}
	return time.Unix(int64(ts), 0), nil
}

// ActiveRun builds an in-flight run from the attribute strings, or nil when
// the zone is not running.
func (z ZoneState) ActiveRun() *model.ActiveRun {
	if z.Status != "on" || z.CurrentStart == "" {
		return nil
	}
	start, err := time.Parse(time.RFC3339, z.CurrentStart)
	if err != nil {
		return nil
	}
	d, err := parseHMS(z.CurrentDuration)
	if err != nil {
		return nil
	}
	return &model.ActiveRun{Start: start, Duration: d, ScheduleName: z.CurrentName}
}

// parseHMS reads irrigation_unlimited's "H:MM:SS" duration strings.
func parseHMS(s string) (time.Duration, error) {
	parts := strings.Split(strings.TrimSpace(s), ":")
	if len(parts) != 3 {
		return 0, fmt.Errorf("want H:MM:SS, got %q", s)
	}
	var out time.Duration
	for i, unit := range []time.Duration{time.Hour, time.Minute, time.Second} {
		n, err := strconv.Atoi(parts[i])
		if err != nil {
			return 0, fmt.Errorf("bad field in %q", s)
		}
		out += time.Duration(n) * unit
	}
	return out, nil
}
