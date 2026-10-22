package tui

import (
	"context"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync/atomic"
	"testing"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/solomon-b/nixos-config/tools/irrigation/internal/hass"
	"github.com/solomon-b/nixos-config/tools/irrigation/internal/model"
)

// stubHA stands in for Home Assistant so no test can open a real valve.
func stubHA(t *testing.T, hits *int32, paths *[]string) *hass.Client {
	t.Helper()
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		atomic.AddInt32(hits, 1)
		*paths = append(*paths, r.URL.Path)
		w.WriteHeader(http.StatusOK)
	}))
	t.Cleanup(srv.Close)
	return hass.New(srv.URL, "test-token")
}

func typeKeys(m Model, s string) Model {
	for _, r := range s {
		next, _ := m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{r}})
		m = next.(Model)
	}
	return m
}

func TestRunKeyOpensPromptWithDefault(t *testing.T) {
	var hits int32
	var paths []string
	m := New(one(sampleZone()), nil, "", nil).WithHass(stubHA(t, &hits, &paths))

	m = typeKeys(m, "a")
	if m.mode != modeDuration {
		t.Fatalf("mode = %v, want modeDuration", m.mode)
	}
	if m.input != defaultDuration {
		t.Errorf("input = %q, want %q", m.input, defaultDuration)
	}
	if hits != 0 {
		t.Error("opening the prompt contacted Home Assistant")
	}
}

func TestRunKeyIsInertWithoutAToken(t *testing.T) {
	m := New(one(sampleZone()), nil, "", nil)
	m = typeKeys(m, "a")
	if m.mode != modeNormal {
		t.Error("prompt opened without a token")
	}
	if m.actionErr == nil {
		t.Error("no explanation given for the ignored key")
	}
}

func TestPromptCapturesQuitKey(t *testing.T) {
	var hits int32
	var paths []string
	m := New(one(sampleZone()), nil, "", nil).WithHass(stubHA(t, &hits, &paths))
	m = typeKeys(m, "a")

	next, cmd := m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'q'}})
	if cmd != nil {
		t.Error("q quit the program while the duration prompt was open")
	}
	if got := next.(Model).input; got != defaultDuration+"q" {
		t.Errorf("input = %q, want the q to be typed into the field", got)
	}
}

func TestDurationPromptRejectsJunkAndStaysOpen(t *testing.T) {
	var hits int32
	var paths []string
	m := New(one(sampleZone()), nil, "", nil).WithHass(stubHA(t, &hits, &paths))
	m = typeKeys(m, "a")
	for range defaultDuration {
		next, _ := m.Update(tea.KeyMsg{Type: tea.KeyBackspace})
		m = next.(Model)
	}
	m = typeKeys(m, "banana")

	next, _ := m.Update(tea.KeyMsg{Type: tea.KeyEnter})
	m = next.(Model)
	if m.mode != modeDuration {
		t.Error("bad input advanced past the duration prompt")
	}
	if m.actionErr == nil {
		t.Error("no error shown for unparseable duration")
	}
	if hits != 0 {
		t.Error("contacted Home Assistant with an invalid duration")
	}
}

func TestDurationOverCapNeverReachesHomeAssistant(t *testing.T) {
	var hits int32
	var paths []string
	m := New(one(sampleZone()), nil, "", nil).WithHass(stubHA(t, &hits, &paths))
	m = typeKeys(m, "a")
	for range defaultDuration {
		next, _ := m.Update(tea.KeyMsg{Type: tea.KeyBackspace})
		m = next.(Model)
	}
	m = typeKeys(m, "5h")

	next, _ := m.Update(tea.KeyMsg{Type: tea.KeyEnter})
	m = next.(Model)
	if m.mode == modeConfirm {
		t.Error("an over-cap duration reached the confirmation step")
	}
	if m.actionErr == nil || !strings.Contains(m.actionErr.Error(), "cap") {
		t.Errorf("actionErr = %v, want a cap explanation", m.actionErr)
	}
	if hits != 0 {
		t.Error("contacted Home Assistant with an over-cap duration")
	}
}

func TestConfirmationIsRequiredBeforeTheValveOpens(t *testing.T) {
	var hits int32
	var paths []string
	m := New(one(sampleZone()), nil, "", nil).WithHass(stubHA(t, &hits, &paths))
	m = typeKeys(m, "a")

	next, _ := m.Update(tea.KeyMsg{Type: tea.KeyEnter})
	m = next.(Model)
	if m.mode != modeConfirm {
		t.Fatalf("mode = %v, want modeConfirm", m.mode)
	}
	if hits != 0 {
		t.Fatal("valve opened before confirmation")
	}

	// Anything other than y backs out.
	next, cmd := m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'n'}})
	if cmd != nil {
		t.Error("declining the confirmation still issued a command")
	}
	if next.(Model).mode != modeNormal {
		t.Error("declining did not return to normal mode")
	}
	if hits != 0 {
		t.Error("valve opened despite declining")
	}
}

func TestConfirmingSendsManualRun(t *testing.T) {
	var hits int32
	var paths []string
	m := New(one(sampleZone()), nil, "", nil).WithHass(stubHA(t, &hits, &paths))
	m = typeKeys(m, "a")
	next, _ := m.Update(tea.KeyMsg{Type: tea.KeyEnter})
	m = next.(Model)

	next, cmd := m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'y'}})
	m = next.(Model)
	if cmd == nil {
		t.Fatal("confirming produced no command")
	}
	msg := cmd()
	done, ok := msg.(actionDoneMsg)
	if !ok {
		t.Fatalf("got %T, want actionDoneMsg", msg)
	}
	if done.err != nil {
		t.Fatalf("manual run errored: %v", done.err)
	}
	if hits != 1 {
		t.Errorf("hits = %d, want 1", hits)
	}
	if len(paths) != 1 || !strings.HasSuffix(paths[0], "/manual_run") {
		t.Errorf("paths = %v, want a manual_run call", paths)
	}
}

func TestStopKeyOnlyActsOnARunningZone(t *testing.T) {
	var hits int32
	var paths []string

	idle := sampleZone()
	idle.Status = "off"
	m := New(one(idle), nil, "", nil).WithHass(stubHA(t, &hits, &paths))
	if _, cmd := m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'c'}}); cmd != nil {
		t.Error("stop key issued a command for a zone that is not running")
	}

	running := sampleZone()
	running.Status = "on"
	m = New(one(running), nil, "", nil).WithHass(stubHA(t, &hits, &paths))
	_, cmd := m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'c'}})
	if cmd == nil {
		t.Fatal("stop key did nothing for a running zone")
	}
	if _, ok := cmd().(actionDoneMsg); !ok {
		t.Error("stop did not report a result")
	}
	if len(paths) == 0 || !strings.HasSuffix(paths[len(paths)-1], "/cancel") {
		t.Errorf("paths = %v, want a cancel call", paths)
	}
}

func TestRunningZoneIsLabelledInTheList(t *testing.T) {
	z := sampleZone()
	z.Status = "on"
	m := New([]model.Controller{{Name: "C", Zones: []model.Zone{z}}}, nil, "", nil)
	if v := m.View(); !strings.Contains(v, "RUNNING") {
		t.Errorf("running zone not marked in the list:\n%s", v)
	}
}

func TestSuccessSchedulesARefresh(t *testing.T) {
	m := New(one(sampleZone()), nil, "", nil)
	next, cmd := m.Update(actionDoneMsg{verb: "started for 10m", zone: "Food Forest"})
	m = next.(Model)
	if cmd == nil {
		t.Error("no settle refresh scheduled after a successful action")
	}
	if !strings.Contains(m.actionMsg, "Food Forest") {
		t.Errorf("actionMsg = %q", m.actionMsg)
	}
	if m.busy {
		t.Error("still busy after the action completed")
	}
}

func TestRunningZoneShowsLiveProgress(t *testing.T) {
	start := time.Now().Add(-30 * time.Minute)
	z := sampleZone()
	z.Status = "on"
	z.Active = &model.ActiveRun{Start: start, Duration: time.Hour, ScheduleName: "Manual"}

	m := New([]model.Controller{{Name: "C", Zones: []model.Zone{z}}}, nil, "", nil)
	m.now = time.Now()
	v := m.View()

	for _, want := range []string{"RUNNING", "Started", "Ends", "Left", "Manual", "50%"} {
		if !strings.Contains(v, want) {
			t.Errorf("view missing %q:\n%s", want, v)
		}
	}
	// The countdown lives beside RUNNING; the last column shows the next run.
	if !strings.Contains(v, "NEXT") {
		t.Errorf("NEXT column header missing:\n%s", v)
	}
	if strings.Contains(v, "LEFT") {
		t.Errorf("column header flips to LEFT:\n%s", v)
	}
}

func TestTickAdvancesTheClock(t *testing.T) {
	m := New(one(sampleZone()), nil, "", nil)
	before := m.now
	later := before.Add(5 * time.Second)
	next, cmd := m.Update(tickMsg(later))
	if !next.(Model).now.Equal(later) {
		t.Error("tick did not advance the clock")
	}
	if cmd == nil {
		t.Error("tick did not schedule the next tick")
	}
}

func threeZones() []model.Controller {
	mk := func(n string) model.Zone {
		z := sampleZone()
		z.Name = n
		z.SensorEntity = "binary_sensor.irrigation_unlimited_c1_z" + n
		return z
	}
	return []model.Controller{{Name: "C", Zones: []model.Zone{mk("1"), mk("2"), mk("3")}}}
}

func TestSpaceTogglesSelection(t *testing.T) {
	m := New(threeZones(), nil, "", nil)
	m = typeKeys(m, " ")
	if len(m.selected) != 1 {
		t.Fatalf("selected = %d, want 1", len(m.selected))
	}
	m = typeKeys(m, " ")
	if len(m.selected) != 0 {
		t.Errorf("selected = %d after toggling off, want 0", len(m.selected))
	}
}

func TestTargetsFallsBackToTheCursor(t *testing.T) {
	m := New(threeZones(), nil, "", nil)
	if got := m.targets(); len(got) != 1 || got[0].Name != "1" {
		t.Errorf("targets = %v, want the cursor zone", zoneNames(got))
	}
}

func TestTargetsUsesEverySelectedZone(t *testing.T) {
	m := New(threeZones(), nil, "", nil)
	m = typeKeys(m, " ")            // select zone 1
	next, _ := m.Update(tea.KeyMsg{Type: tea.KeyDown})
	m = next.(Model)
	next, _ = m.Update(tea.KeyMsg{Type: tea.KeyDown})
	m = next.(Model)
	m = typeKeys(m, " ") // select zone 3

	got := m.targets()
	if len(got) != 2 {
		t.Fatalf("targets = %d, want 2", len(got))
	}
	// Order follows the list, not the click order.
	if got[0].Name != "1" || got[1].Name != "3" {
		t.Errorf("targets = %q, want 1 and 3", zoneNames(got))
	}
}

func TestRunningSelectedZonesOpensEachValve(t *testing.T) {
	var hits int32
	var paths []string
	m := New(threeZones(), nil, "", nil).WithHass(stubHA(t, &hits, &paths))

	m = typeKeys(m, " ")
	next, _ := m.Update(tea.KeyMsg{Type: tea.KeyDown})
	m = next.(Model)
	m = typeKeys(m, " ")

	m = typeKeys(m, "a")
	next, _ = m.Update(tea.KeyMsg{Type: tea.KeyEnter})
	m = next.(Model)
	next, cmd := m.Update(tea.KeyMsg{Type: tea.KeyRunes, Runes: []rune{'y'}})
	m = next.(Model)
	if cmd == nil {
		t.Fatal("confirming produced no command")
	}
	done, ok := cmd().(actionDoneMsg)
	if !ok {
		t.Fatal("no actionDoneMsg")
	}
	if done.err != nil {
		t.Fatalf("run errored: %v", done.err)
	}
	if hits != 2 {
		t.Errorf("hits = %d, want 2 manual_run calls", hits)
	}
	if done.zone != "2 zones" {
		t.Errorf("zone = %q, want a count", done.zone)
	}
	if len(m.selected) != 0 {
		t.Error("selection survived the run")
	}
}

func TestSwitchingControllerClearsSelection(t *testing.T) {
	m := New(two(), nil, "", nil)
	m = typeKeys(m, " ")
	if len(m.selected) != 1 {
		t.Fatal("nothing selected")
	}
	next, _ := m.Update(tea.KeyMsg{Type: tea.KeyTab})
	if len(next.(Model).selected) != 0 {
		t.Error("selection carried across controllers, so a blind run could open unseen valves")
	}
}

func TestPartialFailureNamesTheZonesThatFailed(t *testing.T) {
	var n int32
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// Fail exactly one of the calls.
		if atomic.AddInt32(&n, 1) == 1 {
			w.WriteHeader(http.StatusBadRequest)
			w.Write([]byte("nope"))
			return
		}
		w.WriteHeader(http.StatusOK)
	}))
	defer srv.Close()

	zones := threeZones()[0].Zones
	msg := fanOut(zones, func(ctx context.Context, z model.Zone) error {
		return hass.New(srv.URL, "t").ManualRun(ctx, z.SensorEntity, time.Minute)
	}, "started")
	if msg.err == nil {
		t.Fatal("partial failure reported as success")
	}
	if !strings.Contains(msg.err.Error(), "2 of 3") {
		t.Errorf("error does not say how many succeeded: %v", msg.err)
	}
}

func TestTotalFailureIsReportedAsFailure(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusInternalServerError)
	}))
	defer srv.Close()

	zones := threeZones()[0].Zones
	msg := fanOut(zones, func(ctx context.Context, z model.Zone) error {
		return hass.New(srv.URL, "t").ManualRun(ctx, z.SensorEntity, time.Minute)
	}, "started")
	if msg.err == nil || !strings.Contains(msg.err.Error(), "failed for") {
		t.Errorf("err = %v, want a total failure", msg.err)
	}
}

func TestSelectedZoneIsMarkedInTheList(t *testing.T) {
	m := New(threeZones(), nil, "", nil)
	before := m.View()
	if strings.Contains(before, "•") {
		t.Fatal("marker present before anything was selected")
	}
	m = typeKeys(m, " ")
	after := m.View()
	if !strings.Contains(after, "•") {
		t.Errorf("selected zone is not marked in the list:\n%s", after)
	}
	if strings.Count(after, "•") != 1 {
		t.Errorf("expected exactly one marker, got %d", strings.Count(after, "•"))
	}
}

func TestRowsStayAlignedWhenMarked(t *testing.T) {
	m := New(threeZones(), nil, "", nil)
	widthOf := func(v string, row int) int {
		lines := strings.Split(v, "\n")
		return lipgloss.Width(lines[row])
	}
	plain := widthOf(m.View(), 3)
	m = typeKeys(m, " ")
	marked := widthOf(m.View(), 3)
	if plain != marked {
		t.Errorf("row width changed when marked: %d then %d", plain, marked)
	}
}

func TestDisabledZoneStillShowsEveryField(t *testing.T) {
	// A zone with no schedule and no runs must still account for each field,
	// because "we looked and found nothing" differs from "we did not look".
	z := model.Zone{Name: "Green House 2", SwitchEntity: "switch.manifold_11", Scheduled: false}
	m := New([]model.Controller{{Name: "C", Zones: []model.Zone{z}}}, nil, "", nil)
	d := m.detail(z)
	for _, want := range []string{"Last", "Ran", "Volume", "Next", "Heard", "Battery", "Link"} {
		if !strings.Contains(d, want) {
			t.Errorf("detail pane missing %q for a disabled zone:\n%s", want, d)
		}
	}
	if !strings.Contains(d, "never") {
		t.Errorf("a zone that has never run should say so:\n%s", d)
	}
	if !strings.Contains(d, "disabled") {
		t.Errorf("next-run should read disabled for an unscheduled zone:\n%s", d)
	}
}

func TestZoneWithTelemetryButNoRunsShowsBattery(t *testing.T) {
	z := model.Zone{Name: "Idle", SwitchEntity: "switch.manifold_7", Scheduled: true}
	v := model.Valve{Device: "manifold_7", LastHeard: time.Now().Add(-90 * time.Minute),
		Battery: 90, HasBattery: true}
	m := New([]model.Controller{{Name: "C", Zones: []model.Zone{z}}}, []model.Valve{v}, "", nil)
	d := m.detail(z)
	if !strings.Contains(d, "90%") {
		t.Errorf("battery not shown for a zone with no runs:\n%s", d)
	}
	if !strings.Contains(d, "1h ago") {
		t.Errorf("last-heard not shown for a zone with no runs:\n%s", d)
	}
}

func TestDetailShowsEveryValveField(t *testing.T) {
	z := model.Zone{Name: "Green House 2", SwitchEntity: "switch.manifold_11", Scheduled: false}
	v := model.Valve{
		Device: "manifold_11", LastHeard: time.Now().Add(-10 * time.Minute),
		Battery: 100, HasBattery: true,
		Flow: 0, HasFlow: true,
		DailyVolume: 0, HasDaily: true,
		AutoClose: true, HasAutoClose: true,
		DeviceStatus: "normal_state",
	}
	m := New([]model.Controller{{Name: "C", Zones: []model.Zone{z}}}, []model.Valve{v}, "", nil)
	d := m.detail(z)
	for _, want := range []string{"Battery", "100%", "Link", "Flow", "Today", "Autoshut", "Status", "normal_state"} {
		if !strings.Contains(d, want) {
			t.Errorf("detail missing %q:\n%s", want, d)
		}
	}
}

func TestAutoCloseOnIsFlagged(t *testing.T) {
	z := model.Zone{Name: "Z", SwitchEntity: "switch.manifold_11", Scheduled: true}
	on := model.Valve{Device: "manifold_11", AutoClose: true, HasAutoClose: true}
	off := model.Valve{Device: "manifold_11", AutoClose: false, HasAutoClose: true}

	withOn := New([]model.Controller{{Name: "C", Zones: []model.Zone{z}}}, []model.Valve{on}, "", nil).detail(z)
	withOff := New([]model.Controller{{Name: "C", Zones: []model.Zone{z}}}, []model.Valve{off}, "", nil).detail(z)
	if !strings.Contains(withOn, "Autoshut") || !strings.Contains(withOn, "on") {
		t.Errorf("auto-close on not shown:\n%s", withOn)
	}
	if !strings.Contains(withOff, "off") {
		t.Errorf("auto-close off not shown:\n%s", withOff)
	}
}

// stripANSI removes escape sequences so column positions can be compared.
func stripANSI(s string) string {
	var b strings.Builder
	for i := 0; i < len(s); i++ {
		if s[i] == 0x1b {
			for i < len(s) && s[i] != 'm' {
				i++
			}
			continue
		}
		b.WriteByte(s[i])
	}
	return b.String()
}

func TestHistoryHasColumnHeadings(t *testing.T) {
	m := New(one(sampleZone()), nil, "", nil)
	out := m.history(m.zones()[0])
	for _, want := range []string{"DATE", "WINDOW", "MINS", "VOLUME"} {
		if !strings.Contains(out, want) {
			t.Errorf("history missing heading %q:\n%s", want, out)
		}
	}
}

// runeIndex is the column a substring starts at, counting display cells rather
// than bytes. The arrow in a run row is three bytes wide and one column wide,
// so byte offsets drift from what the eye sees.
func runeIndex(haystack, needle string) int {
	b := strings.Index(haystack, needle)
	if b < 0 {
		return -1
	}
	return len([]rune(haystack[:b]))
}

func TestHistoryHeadingsAlignWithRows(t *testing.T) {
	m := New(one(sampleZone()), nil, "", nil)
	lines := strings.Split(strings.TrimRight(m.history(m.zones()[0]), "\n"), "\n")

	var header, row string
	for _, ln := range lines {
		plain := stripANSI(ln)
		if strings.Contains(plain, "DATE") {
			header = plain
		}
		if strings.Contains(plain, "\u2192") && row == "" {
			row = plain
		}
	}
	if header == "" || row == "" {
		t.Fatalf("could not find header and row:\n%s", strings.Join(lines, "\n"))
	}

	if got, want := runeIndex(header, "WINDOW"), runeIndex(row, "06:00"); got != want {
		t.Errorf("WINDOW heading at column %d, times start at %d\nheader: %q\nrow:    %q",
			got, want, header, row)
	}
	// Minutes are right-aligned, so the heading and the unit should end together.
	if got, want := runeIndex(header, "MINS")+4, runeIndex(row, "m")+1; got != want {
		t.Errorf("MINS heading ends at column %d, minutes end at %d\nheader: %q\nrow:    %q",
			got, want, header, row)
	}
	if got, want := runeIndex(header, "VOLUME"), runeIndex(row, "219"); got != want {
		t.Errorf("VOLUME heading at column %d, volume at %d\nheader: %q\nrow:    %q",
			got, want, header, row)
	}
}

func TestHistoryStillShowsHeadingsWhenEmpty(t *testing.T) {
	z := model.Zone{Name: "Green House 2", SwitchEntity: "switch.manifold_11"}
	m := New([]model.Controller{{Name: "C", Zones: []model.Zone{z}}}, nil, "", nil)
	out := m.history(z)
	if !strings.Contains(out, "No runs on record") {
		t.Errorf("empty history lost its message:\n%s", out)
	}
}

func TestFinishedZoneMinutesAreNotMistakenForACountdown(t *testing.T) {
	// Regression: with one zone running, the TODAY column header flipped to
	// LEFT for the whole table. A zone that had finished its two hours showed
	// "120m" under a header claiming it had 120 minutes remaining.
	done := sampleZone()
	done.Name = "Food Forest"
	done.Status = "off"
	done.TodayMinutes = 120

	running := sampleZone()
	running.Name = "Cactus Fence"
	running.Status = "on"
	running.Active = &model.ActiveRun{
		Start: time.Now().Add(-time.Minute), Duration: 2 * time.Hour,
	}

	m := New([]model.Controller{{Name: "C", Zones: []model.Zone{done, running}}}, nil, "", nil)
	m.now = time.Now()
	v := m.View()

	if strings.Contains(v, "LEFT") {
		t.Errorf("column header flips while a zone runs:\n%s", v)
	}
	// The running zone's remaining time appears next to RUNNING, so it belongs
	// to one zone and cannot be read as a column-wide value.
	if !strings.Contains(v, "RUNNING 1h") {
		t.Errorf("countdown not shown beside RUNNING:\n%s", v)
	}
}

func TestViewFitsTheTerminalHeight(t *testing.T) {
	// Regression: panes grew to fit their content, pushing the title, weather
	// line and warnings off the top where they could not be seen at all.
	z := sampleZone()
	z.Status = "on"
	z.Active = &model.ActiveRun{Start: time.Now().Add(-time.Hour), Duration: 3 * time.Hour}
	for i := 0; i < 20; i++ {
		d := time.Now().AddDate(0, 0, -i)
		z.Runs = append(z.Runs, model.Run{Started: d, Ended: d.Add(time.Hour), Minutes: 60})
	}
	zones := []model.Zone{z, sampleZone(), sampleZone(), sampleZone(),
		sampleZone(), sampleZone(), sampleZone()}

	for _, h := range []int{24, 30, 40, 60} {
		m := New([]model.Controller{{Name: "C", Zones: zones}}, nil,
			"a warning that takes a line", nil)
		n, _ := m.Update(tea.WindowSizeMsg{Width: 120, Height: h})
		got := lipgloss.Height(n.(Model).View())
		if got > h {
			t.Errorf("at %d rows the view rendered %d lines", h, got)
		}
	}
}
