// Package preview holds the layout and colour primitives every adapter's fzf
// preview is built from, so the previews of different agents line up and read
// alike.
package preview

import (
	"strconv"
	"strings"
	"time"

	"agent_session/internal/turns"
)

// LabelWidth is what row labels are padded to, so the values line up, and
// CompactLabelWidth the same for a pane too narrow to spend fourteen columns
// on saying what a value is.
const (
	LabelWidth        = 14
	CompactLabelWidth = 6
)

// CompactTextLen is how much of a free-text field -- the last prompt, the last
// reply -- a compact preview shows, in runes. A phone-width pane wraps anything
// longer into a wall that pushes the rows above it off the top.
//
// WideTextLen is the same cap for the ordinary layout. It is generous rather
// than absent because a preview now shows two free-text blocks: an assistant
// turn runs to thousands of characters routinely, and one such answer would
// otherwise fill the pane and push the prompt it answers out of sight.
const (
	CompactTextLen = 240
	WideTextLen    = 600
)

// GrayRGB is the colour of labels and asides.
const GrayRGB = "170;170;170"

// JoinParts is the non-empty parts, joined. Every field a preview shows is one
// the agent may simply not have written, and a row built by concatenation would
// then read as a stray separator or vanish because its first half was the
// missing one.
func JoinParts(sep string, parts ...string) string {
	kept := make([]string, 0, len(parts))
	for _, p := range parts {
		if p != "" {
			kept = append(kept, p)
		}
	}
	return strings.Join(kept, sep)
}

// Annotate is a principal with a note appended, or nothing at all: a note that
// cannot stand on its own is not worth a row of its own either.
func Annotate(principal, sep, note string) string {
	if principal == "" {
		return ""
	}
	if note == "" {
		return principal
	}
	return principal + sep + note
}

// VersionLabel is `v1.2.3`, or "" for no version.
func VersionLabel(v string) string {
	if v == "" {
		return ""
	}
	return "v" + v
}

// The short spelling of a label in compact mode. Only the ones that do not
// already fit the narrow column; a label with no entry here is written as it
// is. One table for every adapter, so `last activity' cannot become `when' in
// one preview and `time' in the next.
//
// `status', `first prompt', `last prompt' and `last reply' have no entry:
// `status' already fits the narrow column, and the other three are block
// headings written by [Layout.Section], which never pads a label to a column
// width in the first place.
var compactLabels = map[string]string{
	"last activity": "when",
	"last step":     "step",
}

// A Layout writes the rows of one preview. Compact is for a pane the ordinary
// layout does not fit in -- a phone-sized terminal under Termux, or a narrow
// fzf preview window -- where a fourteen-column label and a blank line between
// every block cost more than they explain.
type Layout struct{ Compact bool }

// Row writes one `<label>  <value>  <aside>' line, or nothing when there is no
// value.
func (l Layout) Row(w *strings.Builder, c Painter, label, value, aside string) {
	if value == "" {
		return
	}

	width := LabelWidth
	if l.Compact {
		width = CompactLabelWidth
		if short := compactLabels[label]; short != "" {
			label = short
		}
	}

	pad := label
	if n := width - len(label); n > 0 {
		pad += strings.Repeat(" ", n)
	}

	w.WriteString(c.Gray(pad) + value)
	if aside != "" {
		w.WriteString(" " + c.Gray(aside))
	}
	w.WriteString("\n")
}

// TextLen is how much of a free-text field this layout shows, in runes.
func (l Layout) TextLen() int {
	if l.Compact {
		return CompactTextLen
	}
	return WideTextLen
}

// Section writes a block of free text under a bold heading: the last prompt,
// the last reply. `text` is cut to what the layout allows, and `empty` is what
// stands in when there is no text at all -- said in parentheses, because it is
// a note about the transcript rather than something out of it.
//
// One helper rather than four copies of the same six lines, so a heading, its
// truncation and its "nothing here" note cannot drift between adapters.
func (l Layout) Section(w *strings.Builder, c Painter, label, text, empty string) {
	w.WriteString(c.Bold(label) + "\n")
	if text == "" {
		w.WriteString("(" + empty + ")\n")
		return
	}
	w.WriteString(turns.Truncate(text, l.TextLen()) + "\n")
}

// Gap separates two blocks of a preview. It writes nothing in compact mode: a
// blank line is a fifteenth of what such a pane has to show anything in.
func (l Layout) Gap(w *strings.Builder) {
	if !l.Compact {
		w.WriteString("\n")
	}
}

// Row writes one row in the ordinary layout, for a caller with no layout of
// its own to carry.
func Row(w *strings.Builder, c Painter, label, value, aside string) {
	Layout{}.Row(w, c, label, value, aside)
}

// HumanAge is a duration as its single largest unit: `44s', `6m', `3h', `2d'.
// What a preview answers is how stale the session is, so one unit is enough
// and a second would only be noise.
func HumanAge(d time.Duration) string {
	secs := int64(d.Seconds())
	if secs < 0 {
		secs = 0
	}

	switch {
	case secs < 60:
		return strconv.FormatInt(secs, 10) + "s"
	case secs < 3600:
		return strconv.FormatInt(secs/60, 10) + "m"
	case secs < 86400:
		return strconv.FormatInt(secs/3600, 10) + "h"
	default:
		return strconv.FormatInt(secs/86400, 10) + "d"
	}
}

// A Painter wraps text in ANSI escapes, or does not. A value rather than a
// package flag so nothing can colour output the caller asked to be plain.
type Painter struct{ On bool }

func (c Painter) Fg(rgb, s string) string {
	if !c.On || rgb == "" || s == "" {
		return s
	}
	return "\x1b[38;2;" + rgb + "m" + s + "\x1b[0m"
}

func (c Painter) Gray(s string) string { return c.Fg(GrayRGB, s) }

// BoldFg is bold and coloured in one escape. Nesting `Bold(Fg(...))` would
// work, but the inner reset ends the bold too, leaving two resets to say one
// thing.
func (c Painter) BoldFg(rgb, s string) string {
	if !c.On || s == "" {
		return s
	}
	if rgb == "" {
		return c.Bold(s)
	}
	return "\x1b[1;38;2;" + rgb + "m" + s + "\x1b[0m"
}

func (c Painter) Bold(s string) string {
	if !c.On || s == "" {
		return s
	}
	return "\x1b[1m" + s + "\x1b[0m"
}
