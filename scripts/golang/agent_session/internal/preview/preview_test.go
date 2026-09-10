package preview

import (
	"strings"
	"testing"
	"time"
)

func TestHumanAge(t *testing.T) {
	cases := []struct {
		in   time.Duration
		want string
	}{
		{0, "0s"},
		{-5 * time.Second, "0s"}, // a clock skewed into the future is not "-5s"
		{44 * time.Second, "44s"},
		{59*time.Second + 999*time.Millisecond, "59s"},
		{time.Minute, "1m"},
		{90 * time.Second, "1m"}, // one unit, deliberately: not "1m 30s"
		{6 * time.Minute, "6m"},
		{time.Hour, "1h"},
		{3 * time.Hour, "3h"},
		{24 * time.Hour, "1d"},
		{25 * 24 * time.Hour, "25d"},
	}

	for _, tc := range cases {
		if got := HumanAge(tc.in); got != tc.want {
			t.Errorf("HumanAge(%v) = %q, want %q", tc.in, got, tc.want)
		}
	}
}

// A row whose leading field was never recorded must still show what it has.
// Concatenation would have dropped the whole row, since it started from the
// missing half.
func TestJoinParts(t *testing.T) {
	cases := []struct {
		sep  string
		in   []string
		want string
	}{
		{" · ", []string{"opus-5", "high effort"}, "opus-5 · high effort"},
		{" · ", []string{"opus-5", ""}, "opus-5"},
		{" · ", []string{"", "high effort"}, "high effort"},
		{" · ", []string{"", ""}, ""},
		{" @ ", []string{"~/scripts", "HEAD"}, "~/scripts @ HEAD"},
		{" @ ", []string{"", "HEAD"}, "HEAD"},
		{" · ", []string{"abc", "", ".claude", "v1"}, "abc · .claude · v1"},
	}

	for _, tc := range cases {
		if got := JoinParts(tc.sep, tc.in...); got != tc.want {
			t.Errorf("JoinParts(%q, %q) = %q, want %q", tc.sep, tc.in, got, tc.want)
		}
	}
}

func TestAnnotate(t *testing.T) {
	cases := []struct {
		principal, note, want string
	}{
		{"~/scripts", "HEAD", "~/scripts @ HEAD"},
		{"~/scripts", "", "~/scripts"},
		// The note alone is dropped: "cwd  topic" would claim a branch name is
		// a directory.
		{"", "HEAD", ""},
		{"", "", ""},
	}

	for _, tc := range cases {
		if got := Annotate(tc.principal, " @ ", tc.note); got != tc.want {
			t.Errorf("Annotate(%q, %q) = %q, want %q", tc.principal, tc.note, got, tc.want)
		}
	}
}

// The two layouts differ in three ways and no more: how wide the label column
// is, what the long labels are called, and whether a gap is a blank line.
func TestLayoutRow(t *testing.T) {
	off := Painter{On: false}

	cases := []struct {
		name    string
		compact bool
		label   string
		want    string
	}{
		{"ordinary pads to LabelWidth", false, "cwd", "cwd           ~/scripts (2m ago)\n"},
		{"ordinary keeps the long label", false, "last activity", "last activity ~/scripts (2m ago)\n"},
		{"compact pads to CompactLabelWidth", true, "cwd", "cwd   ~/scripts (2m ago)\n"},
		{"compact shortens the long label", true, "last activity", "when  ~/scripts (2m ago)\n"},
		{"compact shortens the agy label", true, "last step", "step  ~/scripts (2m ago)\n"},
		// Longer than the compact column: it keeps its one separating space
		// rather than running into the value.
		{"compact leaves an unmapped label alone", true, "steps", "steps ~/scripts (2m ago)\n"},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			w := &strings.Builder{}
			Layout{Compact: tc.compact}.Row(w, off, tc.label, "~/scripts", "(2m ago)")
			if got := w.String(); got != tc.want {
				t.Errorf("got %q, want %q", got, tc.want)
			}
		})
	}

	// A row with nothing to say is no row at all, in either layout.
	for _, compact := range []bool{false, true} {
		w := &strings.Builder{}
		Layout{Compact: compact}.Row(w, off, "cwd", "", "(2m ago)")
		if w.String() != "" {
			t.Errorf("compact=%v: an empty value wrote %q", compact, w.String())
		}
	}

	// The package-level Row is the ordinary layout and nothing else.
	plain, viaLayout := &strings.Builder{}, &strings.Builder{}
	Row(plain, off, "last activity", "now", "")
	Layout{}.Row(viaLayout, off, "last activity", "now", "")
	if plain.String() != viaLayout.String() {
		t.Errorf("Row = %q, Layout{}.Row = %q", plain.String(), viaLayout.String())
	}
}

func TestLayoutGap(t *testing.T) {
	w := &strings.Builder{}
	Layout{}.Gap(w)
	if w.String() != "\n" {
		t.Errorf("ordinary gap = %q, want a blank line", w.String())
	}

	w = &strings.Builder{}
	Layout{Compact: true}.Gap(w)
	if w.String() != "" {
		t.Errorf("compact gap = %q, want nothing", w.String())
	}
}

func TestPainterOffEmitsNoEscapes(t *testing.T) {
	off := Painter{On: false}
	for _, got := range []string{off.Gray("x"), off.Bold("x"), off.Fg("1;2;3", "x"), off.BoldFg("1;2;3", "x")} {
		if got != "x" {
			t.Errorf("colour off still produced %q", got)
		}
	}

	on := Painter{On: true}
	// One escape for both attributes, not a nested pair whose inner reset
	// would end the bold early.
	if got := on.BoldFg("1;2;3", "x"); got != "\x1b[1;38;2;1;2;3mx\x1b[0m" {
		t.Errorf("boldFg = %q", got)
	}
	// An unknown profile has no colour, and must not lose the bold with it.
	if got := on.BoldFg("", "x"); got != "\x1b[1mx\x1b[0m" {
		t.Errorf("boldFg with no colour = %q", got)
	}
	if got := on.Fg("", "x"); got != "x" {
		t.Errorf("fg with no colour = %q", got)
	}
}
