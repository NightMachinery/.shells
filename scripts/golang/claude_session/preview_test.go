package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestProfileOf(t *testing.T) {
	cases := []struct{ path, want string }{
		{"/Users/e/.claude/projects/-Users-e-scripts/abc.jsonl", ".claude"},
		{"/Users/e/.claude-work/projects/-Users-e-scripts/abc.jsonl", ".claude-work"},
		// Subagent transcripts sit a level deeper; the profile is still the
		// config home, not whatever is nearest.
		{"/Users/e/.claude/projects/-p/abc/subagents/x.jsonl", ".claude"},
		// Nothing to go on rather than a guess.
		{"/tmp/loose.jsonl", ""},
	}

	for _, tc := range cases {
		if got := profileOf(tc.path); got != tc.want {
			t.Errorf("profileOf(%q) = %q, want %q", tc.path, got, tc.want)
		}
	}
}

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
		if got := humanAge(tc.in); got != tc.want {
			t.Errorf("humanAge(%v) = %q, want %q", tc.in, got, tc.want)
		}
	}
}

// A `user` record carrying the fields that ride on ordinary records.
func previewMsgLine(slug, stamp, cwd, branch, version, effort, model string) string {
	return fmt.Sprintf(
		`{"type":"assistant","timestamp":%q,"slug":%q,"cwd":%q,"gitBranch":%q,`+
			`"version":%q,"effort":%q,"message":{"model":%q,"content":[{"type":"text","text":"hi"}]}}`,
		stamp, slug, cwd, branch, version, effort, model)
}

func writePreviewTranscript(t *testing.T, name string, lines []string) string {
	t.Helper()

	p := filepath.Join(t.TempDir(), name)
	if err := os.WriteFile(p, []byte(strings.Join(lines, "\n")+"\n"), 0o600); err != nil {
		t.Fatalf("write: %v", err)
	}
	return p
}

func TestScanPreviewKeepsTheNewestOfEach(t *testing.T) {
	p := writePreviewTranscript(t, "s.jsonl", []string{
		previewMsgLine("snuggly-orbit", "2026-09-08T10:00:00.000Z", "/tmp/a", "main", "2.1.1", "low", "claude-sonnet-5"),
		`{"type":"ai-title","aiTitle":"early guess"}`,
		`{"type":"permission-mode","permissionMode":"plan"}`,
		`{"type":"mode","mode":"normal"}`,
		`{"type":"last-prompt","lastPrompt":"the first thing asked"}`,
		previewMsgLine("snuggly-orbit", "2026-09-08T11:00:00.000Z", "/tmp/b", "topic", "2.1.2", "high", "claude-opus-5"),
		`{"type":"ai-title","aiTitle":"what it turned into"}`,
		`{"type":"last-prompt","lastPrompt":"the last thing asked"}`,
	})

	got := scanPreview(p, previewWindow)

	for _, tc := range []struct{ field, got, want string }{
		{"name", got.name.resolve(), "what it turned into"},
		{"prompt", got.prompt, "the last thing asked"},
		{"stamp", got.stamp, "2026-09-08T11:00:00.000Z"},
		{"permMode", got.permMode, "plan"},
		{"mode", got.mode, "normal"},
		{"cwd", got.cwd, "/tmp/b"},
		{"branch", got.branch, "topic"},
		{"version", got.version, "2.1.2"},
		{"effort", got.effort, "high"},
		{"model", got.model, "claude-opus-5"},
	} {
		if tc.got != tc.want {
			t.Errorf("%s = %q, want %q", tc.field, tc.got, tc.want)
		}
	}
}

// The fields the preview reads are not all on the records whose `type` names
// them: Claude Code writes `permissionMode` on ordinary `user` records as well
// as on its own `permission-mode` events, and the newest of the two is the
// current one.
func TestScanPreviewTakesPermissionModeFromAnyRecord(t *testing.T) {
	p := writePreviewTranscript(t, "s.jsonl", []string{
		`{"type":"permission-mode","permissionMode":"plan"}`,
		`{"type":"user","timestamp":"2026-09-08T12:00:00.000Z","slug":"s","permissionMode":"auto"}`,
	})

	if got := scanPreview(p, previewWindow).permMode; got != "auto" {
		t.Errorf("permMode = %q, want %q", got, "auto")
	}
}

// A byte-tail starts mid-line, so the first line is dropped -- but only when
// the tail really is a tail. Getting this wrong cost the shell version the only
// record a one-record session had.
func TestScanPreviewWindowSmallerThanFile(t *testing.T) {
	first := `{"type":"ai-title","aiTitle":"out of reach"}`
	lines := []string{first}
	for i := 0; i < 40; i++ {
		lines = append(lines, previewMsgLine("snuggly-orbit", "2026-09-08T10:00:00.000Z", "/tmp/a", "main", "2.1.1", "low", "claude-opus-5"))
	}
	p := writePreviewTranscript(t, "s.jsonl", lines)

	// A window that cannot reach the title: the slug is the fallback.
	if got := scanPreview(p, 512).name.resolve(); got != "snuggly-orbit" {
		t.Errorf("small window: got %q, want the slug %q", got, "snuggly-orbit")
	}
	// The whole file: the title is found.
	if got := scanPreview(p, previewWindow).name.resolve(); got != "out of reach" {
		t.Errorf("full window: got %q, want %q", got, "out of reach")
	}
}

func TestScanPreviewSingleRecordSurvives(t *testing.T) {
	p := writePreviewTranscript(t, "s.jsonl", []string{
		`{"type":"user","timestamp":"2026-09-08T12:00:00.000Z","slug":"snuggly-orbit"}`,
	})

	if got := scanPreview(p, previewWindow).name.resolve(); got != "snuggly-orbit" {
		t.Errorf("got %q, want %q -- a whole-file window has no partial first line", got, "snuggly-orbit")
	}
}

func TestScanPreviewMissingFile(t *testing.T) {
	if got := scanPreview(filepath.Join(t.TempDir(), "nope.jsonl"), previewWindow); got.name.resolve() != "" {
		t.Errorf("a missing file should read as empty, got %+v", got)
	}
}

// An empty transcript is a real thing -- a session that has written nothing
// yet -- and the arithmetic around the window must not panic on it.
func TestScanPreviewEmptyFile(t *testing.T) {
	p := filepath.Join(t.TempDir(), "empty.jsonl")
	if err := os.WriteFile(p, nil, 0o600); err != nil {
		t.Fatalf("write: %v", err)
	}

	if got := scanPreview(p, previewWindow); got.name.resolve() != "" || got.stamp != "" {
		t.Errorf("an empty file should read as empty, got %+v", got)
	}
}

// `-bytes=0' means the whole file, which is how a title older than the default
// window is still reachable.
func TestScanPreviewZeroWindowReadsAll(t *testing.T) {
	lines := []string{`{"type":"ai-title","aiTitle":"far back"}`}
	for i := 0; i < 40; i++ {
		lines = append(lines, previewMsgLine("snuggly-orbit", "2026-09-08T10:00:00.000Z", "/tmp/a", "main", "2.1.1", "low", "claude-opus-5"))
	}
	p := writePreviewTranscript(t, "s.jsonl", lines)

	if got := scanPreview(p, 512).name.resolve(); got != "snuggly-orbit" {
		t.Fatalf("precondition: a 512-byte window should miss the title, got %q", got)
	}
	if got := scanPreview(p, 0).name.resolve(); got != "far back" {
		t.Errorf("zero window: got %q, want %q", got, "far back")
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
		if got := joinParts(tc.sep, tc.in...); got != tc.want {
			t.Errorf("joinParts(%q, %q) = %q, want %q", tc.sep, tc.in, got, tc.want)
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
		if got := annotate(tc.principal, " @ ", tc.note); got != tc.want {
			t.Errorf("annotate(%q, %q) = %q, want %q", tc.principal, tc.note, got, tc.want)
		}
	}
}

func TestPainterOffEmitsNoEscapes(t *testing.T) {
	off := painter{on: false}
	for _, got := range []string{off.gray("x"), off.bold("x"), off.fg("1;2;3", "x"), off.boldFg("1;2;3", "x")} {
		if got != "x" {
			t.Errorf("colour off still produced %q", got)
		}
	}

	on := painter{on: true}
	// One escape for both attributes, not a nested pair whose inner reset
	// would end the bold early.
	if got := on.boldFg("1;2;3", "x"); got != "\x1b[1;38;2;1;2;3mx\x1b[0m" {
		t.Errorf("boldFg = %q", got)
	}
	// An unknown profile has no colour, and must not lose the bold with it.
	if got := on.boldFg("", "x"); got != "\x1b[1mx\x1b[0m" {
		t.Errorf("boldFg with no colour = %q", got)
	}
	if got := on.fg("", "x"); got != "x" {
		t.Errorf("fg with no colour = %q", got)
	}
}
