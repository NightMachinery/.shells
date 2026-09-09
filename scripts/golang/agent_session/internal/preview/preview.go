// Package preview holds the layout and colour primitives every adapter's fzf
// preview is built from, so the previews of different agents line up and read
// alike.
package preview

import (
	"strconv"
	"strings"
	"time"
)

// LabelWidth is what row labels are padded to, so the values line up.
const LabelWidth = 14

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

// Row writes one `<label>  <value>  <aside>' line, or nothing when there is no
// value.
func Row(w *strings.Builder, c Painter, label, value, aside string) {
	if value == "" {
		return
	}

	pad := label
	if n := LabelWidth - len(label); n > 0 {
		pad += strings.Repeat(" ", n)
	}

	w.WriteString(c.Gray(pad) + value)
	if aside != "" {
		w.WriteString(" " + c.Gray(aside))
	}
	w.WriteString("\n")
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
