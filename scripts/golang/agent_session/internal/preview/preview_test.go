package preview

import (
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
