package hass

import (
	"context"
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"testing"
	"time"
)

func TestParseDurationShapes(t *testing.T) {
	cases := []struct {
		in   string
		want time.Duration
	}{
		{"10m", 10 * time.Minute},
		{"90s", 90 * time.Second},
		{"1h30m", 90 * time.Minute},
		{"00:10:00", 10 * time.Minute},
		{"01:30:00", 90 * time.Minute},
		{"10:00", 10 * time.Minute}, // MM:SS
		{"10", 10 * time.Minute},    // bare number means minutes
		{"  15m  ", 15 * time.Minute},
	}
	for _, c := range cases {
		got, err := ParseDuration(c.in)
		if err != nil {
			t.Errorf("ParseDuration(%q) errored: %v", c.in, err)
			continue
		}
		if got != c.want {
			t.Errorf("ParseDuration(%q) = %v, want %v", c.in, got, c.want)
		}
	}
}

func TestParseDurationRejectsJunk(t *testing.T) {
	for _, in := range []string{"", "  ", "abc", "10x", "1:2:3:4", "a:b"} {
		if _, err := ParseDuration(in); err == nil {
			t.Errorf("ParseDuration(%q) accepted junk", in)
		}
	}
}

func TestFormatHMS(t *testing.T) {
	cases := map[time.Duration]string{
		10 * time.Minute: "00:10:00",
		90 * time.Second: "00:01:30",
		2 * time.Hour:    "02:00:00",
	}
	for d, want := range cases {
		if got := FormatHMS(d); got != want {
			t.Errorf("FormatHMS(%v) = %q, want %q", d, got, want)
		}
	}
}

func TestValidateRejectsOverCap(t *testing.T) {
	if err := Validate(MaxDuration + time.Second); err == nil {
		t.Error("accepted a duration over the cap")
	}
	if err := Validate(MaxDuration); err != nil {
		t.Errorf("rejected exactly the cap: %v", err)
	}
	if err := Validate(0); err == nil {
		t.Error("accepted a zero duration")
	}
	if err := Validate(-time.Minute); err == nil {
		t.Error("accepted a negative duration")
	}
}

func TestManualRunPostsExpectedPayload(t *testing.T) {
	var gotPath, gotAuth string
	var gotBody map[string]any
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotPath = r.URL.Path
		gotAuth = r.Header.Get("Authorization")
		b, _ := io.ReadAll(r.Body)
		json.Unmarshal(b, &gotBody)
		w.WriteHeader(http.StatusOK)
	}))
	defer srv.Close()

	c := New(srv.URL, "sekrit")
	if err := c.ManualRun(context.Background(), "binary_sensor.irrigation_unlimited_c1_z7", 10*time.Minute); err != nil {
		t.Fatalf("ManualRun: %v", err)
	}
	if want := "/api/services/irrigation_unlimited/manual_run"; gotPath != want {
		t.Errorf("path = %q, want %q", gotPath, want)
	}
	if gotAuth != "Bearer sekrit" {
		t.Errorf("auth = %q", gotAuth)
	}
	if gotBody["entity_id"] != "binary_sensor.irrigation_unlimited_c1_z7" {
		t.Errorf("entity_id = %v", gotBody["entity_id"])
	}
	if gotBody["time"] != "00:10:00" {
		t.Errorf("time = %v, want 00:10:00", gotBody["time"])
	}
}

func TestManualRunRefusesOverCapBeforeSending(t *testing.T) {
	called := false
	srv := httptest.NewServer(http.HandlerFunc(func(http.ResponseWriter, *http.Request) {
		called = true
	}))
	defer srv.Close()

	c := New(srv.URL, "t")
	if err := c.ManualRun(context.Background(), "x", 3*time.Hour); err == nil {
		t.Error("ManualRun accepted a duration over the cap")
	}
	if called {
		t.Error("ManualRun contacted Home Assistant despite an invalid duration")
	}
}

func TestCancelPostsToCancelService(t *testing.T) {
	var gotPath string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotPath = r.URL.Path
		w.WriteHeader(http.StatusOK)
	}))
	defer srv.Close()

	if err := New(srv.URL, "t").Cancel(context.Background(), "z"); err != nil {
		t.Fatalf("Cancel: %v", err)
	}
	if want := "/api/services/irrigation_unlimited/cancel"; gotPath != want {
		t.Errorf("path = %q, want %q", gotPath, want)
	}
}

func TestServiceErrorSurfacesBody(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusBadRequest)
		w.Write([]byte("entity not found"))
	}))
	defer srv.Close()

	err := New(srv.URL, "t").Cancel(context.Background(), "nope")
	if err == nil {
		t.Fatal("expected an error")
	}
	if !contains(err.Error(), "entity not found") {
		t.Errorf("error lost the server's explanation: %v", err)
	}
}

func contains(h, n string) bool {
	return len(h) >= len(n) && (func() bool {
		for i := 0; i+len(n) <= len(h); i++ {
			if h[i:i+len(n)] == n {
				return true
			}
		}
		return false
	})()
}
