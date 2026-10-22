package hasscfg

import (
	"os"
	"time"
	"strings"
	"testing"
)

func load(t *testing.T) string {
	t.Helper()
	b, err := os.ReadFile("testdata/configuration.yaml")
	if err != nil {
		t.Fatalf("read fixture: %v", err)
	}
	return string(b)
}

func TestExtractBlockStopsAtNextTopLevelKey(t *testing.T) {
	block, ok := ExtractBlock(load(t), "recorder")
	if !ok {
		t.Fatal("recorder block not found")
	}
	if !strings.Contains(block, "db_url") {
		t.Errorf("block missing db_url:\n%s", block)
	}
	if strings.Contains(block, "irrigation_unlimited") {
		t.Errorf("block ran past its end:\n%s", block)
	}
}

func TestExtractBlockMissingKey(t *testing.T) {
	if _, ok := ExtractBlock(load(t), "nosuchkey"); ok {
		t.Error("expected ok=false for absent key")
	}
}

func TestParseControllers(t *testing.T) {
	cfg, err := Parse(load(t))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if got, want := len(cfg.Controllers), 1; got != want {
		t.Fatalf("controller count = %d, want %d", got, want)
	}
	if cfg.Controllers[0].Name != "Middle Garden" {
		t.Errorf("controller name = %q", cfg.Controllers[0].Name)
	}
}

func TestParseZones(t *testing.T) {
	cfg, err := Parse(load(t))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if got, want := len(cfg.AllZones()), 3; got != want {
		t.Fatalf("zone count = %d, want %d", got, want)
	}
	z := cfg.AllZones()[0]
	if z.Name != "Cactus Fence" {
		t.Errorf("Name = %q", z.Name)
	}
	if z.SwitchEntity != "switch.cactus_fence_irrigation_controller" {
		t.Errorf("SwitchEntity = %q", z.SwitchEntity)
	}
	if z.Index != 0 {
		t.Errorf("Index = %d, want 0", z.Index)
	}
}

func TestZoneSensorEntityIsOneBased(t *testing.T) {
	cfg, _ := Parse(load(t))
	if got, want := cfg.AllZones()[0].SensorEntity(), "binary_sensor.irrigation_unlimited_c1_z1"; got != want {
		t.Errorf("SensorEntity() = %q, want %q", got, want)
	}
	if got, want := cfg.AllZones()[2].SensorEntity(), "binary_sensor.irrigation_unlimited_c1_z3"; got != want {
		t.Errorf("SensorEntity() = %q, want %q", got, want)
	}
}

func TestParseRecorderCredentials(t *testing.T) {
	cfg, err := Parse(load(t))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	r := cfg.Recorder
	if r.User != "hass" || r.Password != "hass" || r.Database != "hass" {
		t.Errorf("Recorder = %+v", r)
	}
}

func TestDSNUsesGivenHostNotTheFileHost(t *testing.T) {
	cfg, _ := Parse(load(t))
	got := cfg.DSN("accompaniment-of-shadows")
	if strings.Contains(got, "192.168.5.105") {
		t.Errorf("DSN reused the file's host: %q", got)
	}
	if !strings.Contains(got, "accompaniment-of-shadows") {
		t.Errorf("DSN missing requested host: %q", got)
	}
}

func TestWriteThenReadCache(t *testing.T) {
	dir := t.TempDir()
	t.Setenv("XDG_CACHE_HOME", dir)

	src := load(t)
	if err := writeCache(src); err != nil {
		t.Fatalf("writeCache: %v", err)
	}
	got, at, err := readCache()
	if err != nil {
		t.Fatalf("readCache: %v", err)
	}
	if got != src {
		t.Error("cache round-trip changed content")
	}
	if at.IsZero() {
		t.Error("cache timestamp is zero")
	}
}

func TestReadCacheMissing(t *testing.T) {
	t.Setenv("XDG_CACHE_HOME", t.TempDir())
	if _, _, err := readCache(); err == nil {
		t.Error("expected error for absent cache")
	}
}

func TestLoadFallsBackToCacheWhenSSHFails(t *testing.T) {
	t.Setenv("XDG_CACHE_HOME", t.TempDir())
	if err := writeCache(load(t)); err != nil {
		t.Fatalf("seed cache: %v", err)
	}
	// A host that cannot resolve forces the SSH path to fail.
	res := Source{Host: "no-such-host.invalid", Path: "/nonexistent"}.Load(t.Context())
	if res.Err == nil {
		t.Error("Err = nil, want the underlying ssh failure recorded")
	}
	if !res.FromCache {
		t.Fatal("FromCache = false, want true")
	}
	if len(res.Config.AllZones()) != 3 {
		t.Errorf("zones from cache = %d, want 3", len(res.Config.AllZones()))
	}
}

func TestLoadFailsWhenNoSSHAndNoCache(t *testing.T) {
	t.Setenv("XDG_CACHE_HOME", t.TempDir())
	res := Source{Host: "no-such-host.invalid", Path: "/nonexistent"}.Load(t.Context())
	if res.Err == nil {
		t.Fatal("Err = nil, want failure")
	}
	if res.FromCache {
		t.Error("FromCache = true with no cache present")
	}
}

func TestDSNKeepsAnExplicitPort(t *testing.T) {
	cfg, _ := Parse(load(t))
	got := cfg.DSN("127.0.0.1:15432")
	if strings.Contains(got, "15432:5432") {
		t.Errorf("DSN doubled the port: %q", got)
	}
	if !strings.Contains(got, "127.0.0.1:15432/") {
		t.Errorf("DSN lost the explicit port: %q", got)
	}
}

func TestDSNAddsDefaultPortToBareHost(t *testing.T) {
	cfg, _ := Parse(load(t))
	if got := cfg.DSN("db.example"); !strings.Contains(got, "db.example:5432/") {
		t.Errorf("DSN = %q, want default port appended", got)
	}
}

func TestScheduledIsReadFromWeekdayList(t *testing.T) {
	cfg, err := Parse(load(t))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	want := map[string]bool{
		"Cactus Fence":  true,  // weekday: [mon]
		"Green House 1": true,  // weekday: [mon, wed, sat]
		"Green House 2": false, // weekday: []
	}
	for _, z := range cfg.AllZones() {
		if got, ok := want[z.Name]; ok && z.Scheduled != got {
			t.Errorf("%s Scheduled = %v, want %v", z.Name, z.Scheduled, got)
		}
	}
}

func TestParseMultipleControllers(t *testing.T) {
	b, err := os.ReadFile("testdata/two-controllers.yaml")
	if err != nil {
		t.Fatalf("read fixture: %v", err)
	}
	cfg, err := Parse(string(b))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if got := len(cfg.Controllers); got != 2 {
		t.Fatalf("controllers = %d, want 2", got)
	}
	if cfg.Controllers[1].Name != "Back Orchard" {
		t.Errorf("second controller = %q", cfg.Controllers[1].Name)
	}
	if got := len(cfg.AllZones()); got != 3 {
		t.Errorf("AllZones = %d, want 3", got)
	}
}

func TestSensorEntityCarriesControllerNumber(t *testing.T) {
	b, _ := os.ReadFile("testdata/two-controllers.yaml")
	cfg, err := Parse(string(b))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	want := map[string]string{
		"Cactus Fence": "binary_sensor.irrigation_unlimited_c1_z1",
		"Apple Row":    "binary_sensor.irrigation_unlimited_c2_z1",
		"Pear Row":     "binary_sensor.irrigation_unlimited_c2_z2",
	}
	for _, z := range cfg.AllZones() {
		if got := z.SensorEntity(); got != want[z.Name] {
			t.Errorf("%s SensorEntity = %q, want %q", z.Name, got, want[z.Name])
		}
	}
}

func TestOmittedWeekdayMeansEveryDay(t *testing.T) {
	b, _ := os.ReadFile("testdata/two-controllers.yaml")
	cfg, _ := Parse(string(b))
	for _, z := range cfg.AllZones() {
		if z.Name == "Pear Row" && !z.Scheduled {
			t.Error("Pear Row has a schedule with no weekday key, which means every day, but read as unscheduled")
		}
	}
}

func TestSchedulesAreParsedNotJustCounted(t *testing.T) {
	// Regression: Scheduled was set correctly while Schedules stayed empty,
	// so every projection of upcoming runs came back blank.
	cfg, err := Parse(load(t))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	var cactus Zone
	for _, z := range cfg.AllZones() {
		if z.Name == "Cactus Fence" {
			cactus = z
		}
	}
	if len(cactus.Schedules) != 1 {
		t.Fatalf("Cactus Fence has %d schedules, want 1", len(cactus.Schedules))
	}
	sc := cactus.Schedules[0]
	if sc.Hour != 6 || sc.Minute != 0 {
		t.Errorf("time = %02d:%02d, want 06:00", sc.Hour, sc.Minute)
	}
	if sc.Duration != 2*time.Hour {
		t.Errorf("duration = %v, want 2h", sc.Duration)
	}
	if len(sc.Weekdays) != 1 || sc.Weekdays[0] != time.Monday {
		t.Errorf("weekdays = %v, want [Monday]", sc.Weekdays)
	}
}

func TestEmptyWeekdayListYieldsNoSchedule(t *testing.T) {
	cfg, _ := Parse(load(t))
	for _, z := range cfg.AllZones() {
		if z.Name == "Green House 2" && len(z.Schedules[0].Weekdays) != 0 {
			t.Errorf("Green House 2 weekdays = %v, want none", z.Schedules[0].Weekdays)
		}
	}
}
