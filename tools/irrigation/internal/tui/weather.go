package tui

import (
	"fmt"
	"strings"

	"github.com/solomon-b/nixos-config/tools/irrigation/internal/hass"
)

// rainWindow is how far ahead to look when deciding whether watering is about
// to be wasted. Two days covers the gap between most scheduled runs here.
const rainWindow = 2

// hotDay is where heat stops being weather and starts being a reason to water
// more, not less.
const hotDay = 95

// weatherLine is a single strip under the header: today, the next few days,
// and a verdict about whether the forecast should change what you do.
func (m Model) weatherLine() string {
	if len(m.forecast) == 0 {
		return ""
	}
	today := m.forecast[0]

	var parts []string
	parts = append(parts, fmt.Sprintf("%s %.0f°/%.0f°", today.Condition, today.High, today.Low))
	for _, d := range m.forecast[1:min(4, len(m.forecast))] {
		parts = append(parts, fmt.Sprintf("%s %.0f°", d.Date.Format("Mon"), d.High))
	}

	line := styleDim.Render(" " + strings.Join(parts, " · "))

	if rain := hass.RainAhead(m.forecast, rainWindow); rain >= hass.WetEnoughToSkip {
		line += styleRan.Render(fmt.Sprintf("  ·  %.2fin rain within %dd, consider skipping", rain, rainWindow))
		return line
	}

	// No rain coming. Say so explicitly, and flag heat, because on this system
	// the actionable forecast is usually the hot dry one rather than the wet one.
	hot := 0
	for _, d := range m.forecast[:min(rainWindow+1, len(m.forecast))] {
		if d.High >= hotDay {
			hot++
		}
	}
	if hot > 0 {
		line += styleWarn.Render(fmt.Sprintf("  ·  no rain, %d day(s) over %d°", hot, hotDay))
	} else {
		line += styleDim.Render("  ·  no rain forecast")
	}
	return line
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}
