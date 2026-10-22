package tui

import (
	"os"
	"testing"

	"github.com/charmbracelet/lipgloss"
	"github.com/muesli/termenv"
)

// TestMain pins a colour profile for the whole package.
//
// Without a TTY lipgloss degrades to the Ascii profile and drops every escape
// sequence, so any test asserting that something is highlighted would pass
// whether or not the styling was ever applied. Pinning the profile makes those
// assertions mean what they say.
func TestMain(m *testing.M) {
	lipgloss.SetColorProfile(termenv.TrueColor)
	os.Exit(m.Run())
}
