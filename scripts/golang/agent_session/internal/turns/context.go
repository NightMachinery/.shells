package turns

import (
	"strconv"
	"strings"
)

// ContextUsage is how much of its context window a session was holding when the
// model last answered. A zero Used means the transcript never said, which is a
// different thing from a session that was holding nothing: no agent is obliged
// to report usage, and an old or interrupted transcript may carry none at all.
// So it is a value type with a meaningful zero, and the zero renders as no line
// rather than as a confident "0 tokens".
type ContextUsage struct {
	Used   int
	Window int
}

// Line is what the document shows for this usage, or empty when there is
// nothing to show. A window the adapter could not name leaves the figure
// standing alone, since a total with nothing to compare it against still says
// how big the session got.
func (u ContextUsage) Line() string {
	if u.Used <= 0 {
		return ""
	}
	if u.Window <= 0 {
		return "Context window: " + Commas(u.Used) + " tokens"
	}
	// Rounded half up in integer arithmetic: a float here would make the
	// document's bytes depend on the platform's rounding, and TestPandocPathParity
	// compares two runs byte for byte.
	//
	// Deliberately not clamped at 100. A percentage over it means the window
	// inferred from the model id was too small for the session that actually
	// ran, and a line that says 125% is a fault anybody reading the transcript
	// will notice, where one quietly pinned at 100% hides the same fault
	// forever.
	pct := (u.Used*200 + u.Window) / (u.Window * 2)
	// The slash is spaced on both sides, and each space stays single: org opens
	// an emphasis run on a `/` that is followed directly by non-whitespace, so
	// `142,310 / 200,000` cannot start an italic span, while a tighter form
	// could the moment another slash turned up later on the line.
	return "Context window: " + Commas(u.Used) + " / " + Commas(u.Window) +
		" tokens (" + strconv.Itoa(pct) + "%)"
}

// Commas writes an integer with ASCII thousands separators: 142310 becomes
// "142,310". Plain ASCII and no dependency, because the one thing a rendered
// transcript must never do is depend on a locale.
func Commas(n int) string {
	s := strconv.Itoa(n)
	sign := ""
	if strings.HasPrefix(s, "-") {
		sign, s = "-", s[1:]
	}
	if len(s) <= 3 {
		return sign + s
	}

	var b strings.Builder
	head := len(s) % 3
	if head > 0 {
		b.WriteString(s[:head])
	}
	for i := head; i < len(s); i += 3 {
		if b.Len() > 0 {
			b.WriteByte(',')
		}
		b.WriteString(s[i : i+3])
	}
	return sign + b.String()
}
