package tui

import (
	"context"
	"fmt"
	"strings"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/solomon-b/nixos-config/tools/irrigation/internal/hass"
	"github.com/solomon-b/nixos-config/tools/irrigation/internal/model"
)

// Reloader re-reads everything from source. Refresh re-reads
// configuration.yaml over SSH as well as the database, so a schedule change is
// picked up without restarting the tool.
type Reloader func(context.Context) ([]model.Controller, []model.Valve, string, error)

type reloadedMsg struct {
	controllers []model.Controller
	valves      []model.Valve
	warning     string
	err         error
}

type Model struct {
	controllers []model.Controller
	active      int
	valves      []model.Valve
	cursor      int
	warning     string
	now         time.Time
	reload      Reloader
	loading     bool
	loadErr     error
	width       int
	height      int
	since       time.Time

	hass      *hass.Client
	mode      mode
	input     string
	pending   time.Duration
	actionMsg string
	actionErr error
	busy      bool
	// selected is keyed by sensor entity so a reload cannot shift the marks
	// onto the wrong rows.
	selected  map[string]bool
	fleetView bool

	sweeping     bool
	sweepZones   []model.Zone
	sweepResults []SweepResult
	sweepAt      int
	sweepDur     time.Duration
	sweepPending bool
	ackWait      time.Duration
	forecast     []hass.Day
	fleetCursor  int
	upcoming     []model.Upcoming
	upcomingView bool
}

func New(controllers []model.Controller, valves []model.Valve, warning string, reload Reloader) Model {
	// 80x24 keeps the first frame sane before the terminal reports its size.
	return Model{controllers: controllers, valves: valves, warning: warning, now: time.Now(),
		reload: reload, width: 80, height: 24, since: time.Now().AddDate(0, 0, -30),
		ackWait: defaultAckWait}
}

// zones returns the active controller's zones.
func (m Model) zones() []model.Zone {
	if m.active < 0 || m.active >= len(m.controllers) {
		return nil
	}
	return m.controllers[m.active].Zones
}

// WithHass enables the run and stop keys. Without it the tool is read-only.
func (m Model) WithHass(c *hass.Client) Model {
	m.hass = c
	return m
}

// WithUpcoming attaches the projected schedule.
func (m Model) WithUpcoming(u []model.Upcoming) Model {
	m.upcoming = u
	return m
}

// WithForecast attaches the weather outlook. Absent forecast simply hides the
// strip; the tool is about irrigation, not weather.
func (m Model) WithForecast(days []hass.Day) Model {
	m.forecast = days
	return m
}

// WithSince fixes the left edge of the calendar at the oldest data actually
// held, so the grid never implies knowledge it does not have.
func (m Model) WithSince(t time.Time) Model {
	if !t.IsZero() {
		m.since = t
	}
	return m
}

func (m Model) Init() tea.Cmd { return tickCmd() }

func (m Model) doReload() tea.Msg {
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()
	controllers, valves, warning, err := m.reload(ctx)
	return reloadedMsg{controllers: controllers, valves: valves, warning: warning, err: err}
}

func (m Model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tickMsg:
		// A running zone needs a moving countdown, so the clock advances every
		// second regardless of whether anything was re-read.
		m.now = time.Time(msg)
		return m, tickCmd()

	case tea.WindowSizeMsg:
		m.width, m.height = msg.Width, msg.Height
		return m, nil

	case tea.KeyMsg:
		// Prompts capture the keyboard so a stray "q" cannot quit mid-entry.
		if m.mode != modeNormal {
			return m.updatePrompt(msg)
		}
		switch msg.String() {
		case "q", "ctrl+c":
			return m, tea.Quit
		case "esc":
			if len(m.sweepResults) > 0 && !m.sweeping {
				m.sweepResults, m.sweepZones, m.sweepAt = nil, nil, 0
				return m, nil
			}
			return m, tea.Quit
		case "up", "k":
			if m.fleetView {
				if m.fleetCursor > 0 {
					m.fleetCursor--
				}
			} else if m.cursor > 0 {
				m.cursor--
			}
		case "down", "j":
			if m.fleetView {
				if m.fleetCursor < len(m.fleetValves())-1 {
					m.fleetCursor++
				}
			} else if m.cursor < len(m.zones())-1 {
				m.cursor++
			}
		case "x":
			if !m.fleetView {
				return m, nil
			}
			if !m.canActuate() {
				m.actionErr = errNoToken
				return m, nil
			}
			vs := m.fleetValves()
			if m.fleetCursor >= len(vs) {
				return m, nil
			}
			v := vs[m.fleetCursor]
			if !v.HasAutoClose || v.AutoCloseEntity == "" {
				m.actionErr = fmt.Errorf("%s does not expose an auto-shutoff switch", v.Device)
				return m, nil
			}
			m.busy = true
			m.actionErr, m.actionMsg = nil, ""
			return m, m.toggleAutoClose(v)
		case "p":
			if m.fleetView || !m.canActuate() {
				if !m.canActuate() {
					m.actionErr = errNoToken
				}
				return m, nil
			}
			if len(m.zones()) == 0 {
				return m, nil
			}
			m.mode = modeAdjust
			m.input = defaultAdjust
			m.actionErr, m.actionMsg = nil, ""
			return m, nil
		case "t":
			if !m.canActuate() {
				m.actionErr = errNoToken
				return m, nil
			}
			var running []string
			for _, z := range m.zones() {
				if isRunning(z.Status) {
					running = append(running, z.Name)
				}
			}
			if len(running) > 0 {
				// Opening more valves mid-run changes the pressure every other
				// zone is being judged on, so the verdicts would be worthless.
				m.actionErr = fmt.Errorf("cannot sweep while %s is running", strings.Join(running, ", "))
				return m, nil
			}
			m.mode = modeDuration
			m.sweepPending = true
			m.input = defaultSweepDuration
			m.actionErr, m.actionMsg = nil, ""
			return m, nil
		case "v":
			m.fleetView = !m.fleetView
			m.upcomingView = false
			return m, nil
		case "u":
			m.upcomingView = !m.upcomingView
			m.fleetView = false
			return m, nil
		case " ":
			if len(m.zones()) == 0 {
				return m, nil
			}
			m.toggleSelected(m.zones()[m.cursor].SensorEntity)
			return m, nil
		case "tab", "right", "l":
			if len(m.controllers) > 1 {
				m.active = (m.active + 1) % len(m.controllers)
				m.cursor = 0
				// Marks belong to the controller they were made on. Carrying
				// them across would let a blind "a" open valves you cannot see.
				m.selected = nil
			}
		case "shift+tab", "left", "h":
			if len(m.controllers) > 1 {
				m.active = (m.active - 1 + len(m.controllers)) % len(m.controllers)
				m.cursor = 0
				m.selected = nil
			}
		case "r":
			if !m.loading && m.reload != nil {
				m.loading = true
				return m, m.doReload
			}
		case "a":
			if !m.canActuate() {
				m.actionErr = errNoToken
				return m, nil
			}
			if len(m.zones()) == 0 {
				return m, nil
			}
			m.mode = modeDuration
			m.input = defaultDuration
			m.actionErr, m.actionMsg = nil, ""
			return m, nil
		case "c":
			if !m.canActuate() {
				m.actionErr = errNoToken
				return m, nil
			}
			if len(m.zones()) == 0 {
				return m, nil
			}
			var running []model.Zone
			for _, z := range m.targets() {
				if isRunning(z.Status) {
					running = append(running, z)
				}
			}
			if len(running) == 0 {
				return m, nil
			}
			m.busy = true
			m.actionErr, m.actionMsg = nil, ""
			return m, m.cancelZones(running)
		}

	case sweepStepMsg:
		m.sweepResults = append(m.sweepResults, msg.result)
		m.sweepAt = len(m.sweepResults)
		if m.sweepAt < len(m.sweepZones) {
			return m, m.sweepStep(m.sweepAt, m.sweepZones[m.sweepAt], m.sweepDur)
		}
		m.sweeping = false
		return m, nil

	case actionDoneMsg:
		m.busy = false
		if msg.err != nil {
			m.actionErr = msg.err
			return m, nil
		}
		m.actionMsg = fmt.Sprintf("%s %s", msg.zone, msg.verb)
		return m, settle()

	case settleMsg:
		if m.reload == nil {
			return m, nil
		}
		m.loading = true
		return m, m.doReload

	case reloadedMsg:
		m.loading = false
		if msg.err != nil {
			m.loadErr = msg.err
			return m, nil
		}
		m.controllers, m.valves, m.warning = msg.controllers, msg.valves, msg.warning
		// The schedule is projected from config, so a reload that picked up an
		// edited configuration.yaml must reproject it.
		m.upcoming = BuildUpcoming(msg.controllers, time.Now(), 3, 20)
		m.loadErr = nil
		m.now = time.Now()
		if m.active >= len(m.controllers) {
			m.active = 0
		}
		if m.cursor >= len(m.zones()) {
			m.cursor = 0
		}
	}
	return m, nil
}

func (m Model) View() string {
	var b strings.Builder

	title := styleHeader.Render("IRRIGATION")
	if m.active < len(m.controllers) {
		title += styleDim.Render(" · " + m.controllers[m.active].Name)
		if len(m.controllers) > 1 {
			title += styleDim.Render(fmt.Sprintf(" (%d/%d)", m.active+1, len(m.controllers)))
		}
	}
	stamp := styleDim.Render(m.now.Format("2006-01-02 15:04"))
	b.WriteString(padBetween(title, stamp, m.width))
	b.WriteString("\n")

	if w := m.weatherLine(); w != "" {
		b.WriteString(w)
		b.WriteString("\n")
	}

	if m.warning != "" {
		b.WriteString(styleWarn.Render("! " + m.warning))
		b.WriteString("\n")
	}

	if m.sweeping || len(m.sweepResults) > 0 {
		b.WriteString(stylePane.Width(m.width - 2).Render(m.sweepView()))
		return b.String()
	}

	if m.upcomingView {
		b.WriteString(stylePane.Width(m.width - 2).Render(m.upcomingView2()))
		b.WriteString("\n\n")
		b.WriteString(styleDim.Render(" u zones · r refresh · q quit"))
		return b.String()
	}

	if m.fleetView {
		b.WriteString(stylePane.Width(m.width - 2).Render(m.fleet()))
		b.WriteString("\n\n")
		footer := " ↑↓ valve · x toggle auto-shutoff · v zones · r refresh · q quit"
		if !m.canActuate() {
			footer = " ↑↓ valve · v zones · r refresh · q quit"
		}
		switch {
		case m.busy:
			footer = " working…"
		case m.actionErr != nil:
			b.WriteString(styleBad.Render(" " + m.actionErr.Error()))
			b.WriteString("\n")
		case m.actionMsg != "":
			b.WriteString(styleRan.Render(" " + m.actionMsg))
			b.WriteString("\n")
		}
		b.WriteString(styleDim.Render(footer))
		return b.String()
	}

	b.WriteString(stylePane.Width(m.width - 2).Render(m.calendar()))
	b.WriteString("\n")
	b.WriteString(" " + legend())
	b.WriteString("\n")

	if m.mode != modeNormal {
		b.WriteString(m.promptView())
		b.WriteString("\n")
	} else if len(m.zones()) > 0 {
		z := m.zones()[m.cursor]
		// Height left after the header, calendar box and footer.
		// Size the panes from the space actually left, measured rather than
		// guessed. Growing them to fit their content pushed the title, weather
		// and warnings off the top of the terminal, where they were invisible.
		paneH := m.height - lipgloss.Height(b.String()) - 3
		if paneH < 5 {
			paneH = 5
		}
		// Width() is content width and each border adds 2, so two panes side by
		// side must account for 4 columns of border between them.
		leftW := 30
		// Both boxes take the same height, or the shorter closes early and the
		// layout looks broken. Content past that height is clipped.
		// Height() sets a minimum, not a cap, so both panes are clipped as well
		// as sized. Without that the taller one grows and pushes the header off
		// the top of the terminal.
		detail := clip(m.detail(z), paneH)
		history := clip(m.historyN(z, paneH-3), paneH)
		left := stylePane.Width(leftW).Height(paneH).Render(detail)
		right := stylePane.Width(m.width - leftW - 4).Height(paneH).Render(history)
		b.WriteString(lipgloss.JoinHorizontal(lipgloss.Top, left, right))
		b.WriteString("\n")
	}

	keys := " ↑↓ zone · space select · a run · c stop · p adjust · t sweep · u next · v valves · r · q"
	if len(m.controllers) > 1 {
		keys = " ↑↓ zone · tab controller · space select · a run · c stop · r refresh · q quit"
	}
	if !m.canActuate() {
		keys = " ↑↓ zone · v valves · r refresh · q quit"
		if len(m.controllers) > 1 {
			keys = " ↑↓ zone · tab controller · v valves · r refresh · q quit"
		}
	}
	footer := styleDim.Render(keys)
	switch {
	case m.busy:
		footer = styleDim.Render(" working…")
	case m.loading:
		footer = styleDim.Render(" refreshing…")
	case m.actionErr != nil:
		footer = styleBad.Render(" " + m.actionErr.Error())
	case m.loadErr != nil:
		footer = styleBad.Render(" refresh failed: " + m.loadErr.Error())
	case m.actionMsg != "":
		footer = styleRan.Render(" " + m.actionMsg)
	}
	b.WriteString(footer)
	return b.String()
}

// calendarCols is how many day columns to draw: what fits, capped by how far
// the recorder's history actually reaches.
func (m Model) calendarWeeks() int {
	return calendarWeeks(m.width, dayIndex(m.since, m.now)+1)
}

// calendar renders one row per zone, one column per day.
func (m Model) calendar() string {
	weeks := m.calendarWeeks()
	n := weeks * daysPerWeek
	// Grid starts on a Monday so the weekday strip lines up and weeks group
	// cleanly. It never reaches back past the history actually held.
	from := mondayOf(m.now).AddDate(0, 0, -(weeks-1)*daysPerWeek)

	gap := m.width - 4 - CalendarWidth(weeks)
	if gap < 0 {
		gap = 0
	}
	filler := strings.Repeat(" ", gap)

	var b strings.Builder
	// Two header rows: dates above each week, weekday letters above each column.
	today := dayIndex(from, m.now)
	b.WriteString(pad("", nameCol) + dateHeader(from, weeks, m.now))
	b.WriteString("\n")
	b.WriteString(styleDim.Render(pad("ZONE", nameCol)))
	b.WriteString(weekdayHeader(weeks, today))
	b.WriteString(filler)
	b.WriteString(styleDim.Render("  " + pad("STATUS", statusCol) + rpad("NEXT", nextCol)))
	b.WriteString("\n")

	for i, z := range m.zones() {
		label, style := zoneStatus(z, m.now)
		if isRunning(z.Status) {
			// The countdown belongs beside RUNNING, not in the TODAY column.
			// That column header used to flip to LEFT whenever any zone was
			// active, which mislabelled every zone that was not.
			label, style = "RUNNING", styleRan
			if z.Active != nil {
				label = "RUNNING " + countdownShort(z.Active.Remaining(m.now))
			}
		}
		// Two columns carry two independent facts: which row the cursor is on,
		// and which rows are marked for the next action.
		mark := " "
		if m.selected[z.SensorEntity] {
			mark = styleRan.Render("•")
		}
		name := pad(z.Name, nameCol-3)
		if i == m.cursor {
			b.WriteString(styleSelected.Render(">") + mark + " " + styleSelected.Render(name))
		} else {
			b.WriteString(" " + mark + " " + name)
		}
		b.WriteString(renderCells(zoneDays(z, from, n, m.now)))
		b.WriteString(filler)
		b.WriteString("  " + style.Render(pad(label, statusCol)))
		// When this zone waters again. Unlike a daily total this is populated
		// for every scheduled zone on every day, rather than only after a run.
		if next := model.NextRuns(z.Name, "", z.Specs, m.now, 1); len(next) > 0 {
			b.WriteString(rpad(next[0].At.Format("Mon 15:04"), nextCol))
		} else {
			b.WriteString(styleDim.Render(rpad("-", nextCol)))
		}
		if i < len(m.zones())-1 {
			b.WriteString("\n")
		}
	}
	return b.String()
}

func pad(s string, n int) string {
	if len(s) >= n {
		return s[:n]
	}
	return s + strings.Repeat(" ", n-len(s))
}

func rpad(s string, n int) string {
	if len(s) >= n {
		return s
	}
	return strings.Repeat(" ", n-len(s)) + s
}

func padBetween(left, right string, width int) string {
	gap := width - lipgloss.Width(left) - lipgloss.Width(right)
	if gap < 1 {
		gap = 1
	}
	return left + strings.Repeat(" ", gap) + right
}

func (m Model) history(z model.Zone) string { return m.historyN(z, 0) }

// historyN lists runs newest first, showing at most max rows. When runs are
// dropped it says so, rather than silently implying that is all of them.
func (m Model) historyN(z model.Zone, max int) string {
	var b strings.Builder
	fmt.Fprintf(&b, "%s\n\n", styleHeader.Render(z.Name+" · run history"))
	if len(z.Runs) == 0 {
		b.WriteString(styleDim.Render("No runs on record."))
		return b.String()
	}
	// Widths track the row format below: "Mon 01-02" is 9, "06:00 → 08:00" is
	// 13, and minutes render as %5.0f plus the unit.
	fmt.Fprintf(&b, "%s\n", styleDim.Render(
		fmt.Sprintf("%-9s  %-13s  %6s  %s", "DATE", "WINDOW", "MINS", "VOLUME")))

	shown := 0
	for i := len(z.Runs) - 1; i >= 0; i-- {
		if max > 0 && shown >= max {
			fmt.Fprintf(&b, "%s\n", styleDim.Render(fmt.Sprintf("… %d earlier runs", i+1)))
			break
		}
		r := z.Runs[i]
		vol := styleDim.Render("no reading")
		if r.HasVolume {
			vol = fmt.Sprintf("%.0f", r.VolumeDelta)
		}
		fmt.Fprintf(&b, "%s  %s → %s  %5.0fm  %s\n",
			r.Started.Format("Mon 01-02"),
			r.Started.Format("15:04"),
			r.Ended.Format("15:04"),
			r.Minutes, vol)
		shown++
	}
	return b.String()
}

func (m Model) detail(z model.Zone) string {
	var b strings.Builder
	fmt.Fprintf(&b, "%s\n\n", styleHeader.Render(z.Name))

	// The "switch." prefix is on every entity and carries no information in a
	// column this narrow.
	valve := trunc(strings.TrimPrefix(z.SwitchEntity, "switch."), 20)
	if valve == "" {
		valve = styleWarn.Render("none configured")
	}
	fmt.Fprintf(&b, "%s %s\n", styleDim.Render("Valve  "), valve)

	if a := z.Active; a != nil {
		pct := a.Percent(m.now)
		fmt.Fprintf(&b, "\n%s\n", styleRan.Render("RUNNING"))
		fmt.Fprintf(&b, "%s %s %d%%\n", styleDim.Render("       "), progressBar(pct, 12), pct)
		fmt.Fprintf(&b, "%s %s\n", styleDim.Render("Started"), a.Start.Format("15:04:05"))
		fmt.Fprintf(&b, "%s %s\n", styleDim.Render("Ends   "), a.Ends().Format("15:04:05"))
		fmt.Fprintf(&b, "%s %s\n", styleDim.Render("Left   "), countdown(a.Remaining(m.now)))
		fmt.Fprintf(&b, "%s %s\n", styleDim.Render("For    "), shortDur(a.Duration))
		if a.ScheduleName != "" {
			fmt.Fprintf(&b, "%s %s\n", styleDim.Render("Source "), a.ScheduleName)
		}
	}

	// Every field below appears for every zone, scheduled or not. A dash means
	// "we looked and there is nothing", which is a different and more useful
	// statement than omitting the row entirely.
	b.WriteString("\n")
	none := styleDim.Render("-")

	if len(z.Runs) == 0 {
		fmt.Fprintf(&b, "%s %s\n", styleDim.Render("Last   "), styleDim.Render("never"))
		fmt.Fprintf(&b, "%s %s\n", styleDim.Render("Ran    "), none)
		fmt.Fprintf(&b, "%s %s\n", styleDim.Render("Volume "), none)
	} else {
		last := z.Runs[len(z.Runs)-1]
		fmt.Fprintf(&b, "%s %s → %s\n", styleDim.Render("Last   "),
			last.Started.Format("01-02 15:04"), last.Ended.Format("15:04"))
		fmt.Fprintf(&b, "%s %.0f min\n", styleDim.Render("Ran    "), last.Minutes)
		if base, ok := model.VolumeBaseline(z.Runs); ok {
			fmt.Fprintf(&b, "%s %.0f (avg %.0f)\n", styleDim.Render("Volume "), last.VolumeDelta, base)
		} else {
			fmt.Fprintf(&b, "%s %s\n", styleDim.Render("Volume "), styleDim.Render("not measured"))
		}
	}

	switch next, ok := model.NextRuns(z.Name, "", z.Specs, m.now, 1), z.Scheduled; {
	case ok && len(next) > 0:
		fmt.Fprintf(&b, "%s %s\n", styleDim.Render("Next   "), next[0].At.Format("Mon 01-02 15:04"))
	case !z.Scheduled:
		fmt.Fprintf(&b, "%s %s\n", styleDim.Render("Next   "), styleDim.Render("disabled"))
	default:
		fmt.Fprintf(&b, "%s %s\n", styleDim.Render("Next   "), styleDim.Render("no schedule"))
	}

	b.WriteString("\n")

	// Exact match, not Contains. "manifold_1" is a substring of
	// "switch.manifold_11", so a Contains check attributes the wrong valve.
	device := strings.TrimPrefix(z.SwitchEntity, "switch.")
	for _, v := range m.valves {
		if device == "" || v.Device != device {
			continue
		}
		fmt.Fprintf(&b, "%s %s ago\n", styleDim.Render("Heard  "), humanSince(v.LastHeard, m.now))
		if v.HasBattery {
			fmt.Fprintf(&b, "%s %d%%\n", styleDim.Render("Battery"), v.Battery)
		} else {
			fmt.Fprintf(&b, "%s %s\n", styleDim.Render("Battery"), none)
		}
		if v.HasLink {
			fmt.Fprintf(&b, "%s %d\n", styleDim.Render("Link   "), v.LinkQuality)
		} else {
			fmt.Fprintf(&b, "%s %s\n", styleDim.Render("Link   "), none)
		}
		if v.HasFlow {
			fmt.Fprintf(&b, "%s %.1f\n", styleDim.Render("Flow   "), v.Flow)
		} else {
			fmt.Fprintf(&b, "%s %s\n", styleDim.Render("Flow   "), none)
		}
		if v.HasDaily {
			fmt.Fprintf(&b, "%s %.0f\n", styleDim.Render("Today  "), v.DailyVolume)
		} else {
			fmt.Fprintf(&b, "%s %s\n", styleDim.Render("Today  "), none)
		}
		switch {
		case !v.HasAutoClose:
			fmt.Fprintf(&b, "%s %s\n", styleDim.Render("Autoshut"), none)
		case v.AutoClose:
			// Worth flagging: on a valve whose meter under-reads, this turns a
			// false shortage reading into a zone that silently does not water.
			fmt.Fprintf(&b, "%s %s\n", styleDim.Render("Autoshut"), styleWarn.Render("on"))
		default:
			fmt.Fprintf(&b, "%s %s\n", styleDim.Render("Autoshut"), "off")
		}
		if v.DeviceStatus != "" {
			style := styleDim
			if v.DeviceStatus != "normal_state" {
				style = styleWarn
			}
			fmt.Fprintf(&b, "%s %s\n", styleDim.Render("Status "), style.Render(v.DeviceStatus))
		}
		return b.String()
	}

	// Most valves in this fleet expose no battery or link quality entities at
	// all. That is how they are paired, not a fault, so the rows still appear
	// and read empty rather than vanishing.
	for _, label := range []string{"Heard  ", "Battery", "Link   ", "Flow   ", "Today  ", "Autoshut"} {
		fmt.Fprintf(&b, "%s %s\n", styleDim.Render(label), none)
	}
	return b.String()
}

func (m Model) Run() error {
	_, err := tea.NewProgram(m, tea.WithAltScreen()).Run()
	return err
}
