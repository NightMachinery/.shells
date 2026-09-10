package claude

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent_session/internal/preview"
	"agent_session/internal/session"
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

// What compact mode is for: a pane that cannot fit the ordinary layout. The
// uuid leaves the subtitle, the labels shrink, and the blank lines go -- but
// nothing a reader actually needs is lost.
func TestPreviewCompact(t *testing.T) {
	const uuid = "87e1476d-1111-4111-8111-87e1476d0000"
	p := writePreviewTranscript(t, uuid+".jsonl", []string{
		previewMsgLine("snuggly-orbit", "2026-09-08T10:00:00.000Z", "/tmp/a", "main", "2.1.1", "high", "claude-opus-5"),
		`{"type":"ai-title","aiTitle":"a session with a name"}`,
		`{"type":"last-prompt","lastPrompt":"` + strings.Repeat("x", 400) + `"}`,
	})

	full, err := Adapter{}.Preview(p, session.PreviewOpts{Bytes: previewWindow, Color: false})
	if err != nil {
		t.Fatal(err)
	}
	compact, err := Adapter{}.Preview(p, session.PreviewOpts{Bytes: previewWindow, Color: false, Compact: true})
	if err != nil {
		t.Fatal(err)
	}

	if !strings.Contains(full, uuid) || !strings.Contains(full, "last activity") {
		t.Errorf("the ordinary layout changed:\n%s", full)
	}
	if strings.Contains(compact, uuid) {
		t.Errorf("compact still carries the uuid:\n%s", compact)
	}
	if !strings.Contains(compact, "when ") || strings.Contains(compact, "last activity") {
		t.Errorf("compact should label the time `when':\n%s", compact)
	}
	if strings.Contains(compact, "\n\n") {
		t.Errorf("compact should have no blank lines:\n%s", compact)
	}
	// Everything that says what the session is stays.
	for _, want := range []string{"a session with a name", "opus-5 · high effort", "/tmp/a @ main", "last prompt"} {
		if !strings.Contains(compact, want) {
			t.Errorf("compact lacks %q:\n%s", want, compact)
		}
	}
	if strings.Contains(compact, strings.Repeat("x", preview.CompactTextLen+1)) {
		t.Errorf("compact should cut the prompt to %d runes:\n%s", preview.CompactTextLen, compact)
	}
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
		{"record", got.record, "the last thing asked"},
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

// ** the conversation sections

// The message lines a conversation is made of, spelled out rather than built
// by a helper: what each of these tests is about is the exact shape of the
// record, so hiding it behind a constructor would hide the thing under test.
const (
	typedLine      = `{"type":"user","timestamp":"2026-09-08T12:00:00.000Z","message":{"role":"user","content":[{"type":"text","text":%q}]}}`
	toolResultLine = `{"type":"user","timestamp":"2026-09-08T12:01:00.000Z","message":{"role":"user",` +
		`"content":[{"type":"tool_result","tool_use_id":"t1","content":"1234 files"}]}}`
	toolUseLine = `{"type":"assistant","timestamp":"2026-09-08T12:01:00.000Z","message":{"model":"claude-opus-5",` +
		`"content":[{"type":"tool_use","id":"t1","name":"Bash","input":{"command":"ls"}}]}}`
	thinkingLine = `{"type":"assistant","timestamp":"2026-09-08T12:02:00.000Z","message":{"model":"claude-opus-5",` +
		`"content":[{"type":"thinking","thinking":"weighing it up"}]}}`
)

func assistantLine(text string) string {
	return fmt.Sprintf(`{"type":"assistant","timestamp":"2026-09-08T12:03:00.000Z","message":{"model":"claude-opus-5",`+
		`"content":[{"type":"text","text":%q}]}}`, text)
}

// The whole point of the redesign: `last-prompt` records are almost never
// inside the tail window, so the section is built from the newest message the
// user actually typed. When both are there, the typed one wins -- it is the
// newer of the two, and the record only repeats an older turn.
func TestPreviewLastPromptIsTheNewestTypedMessage(t *testing.T) {
	p := writePreviewTranscript(t, "s.jsonl", []string{
		fmt.Sprintf(typedLine, "the first thing asked"),
		`{"type":"last-prompt","lastPrompt":"a stale bookkeeping copy"}`,
		fmt.Sprintf(typedLine, "the newest thing asked"),
		toolUseLine,
		toolResultLine,
	})

	got, err := Adapter{}.Preview(p, session.PreviewOpts{Bytes: previewWindow})
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(got, "last prompt\nthe newest thing asked\n") {
		t.Errorf("want the newest typed message as the last prompt:\n%s", got)
	}
	if strings.Contains(got, "a stale bookkeeping copy") {
		t.Errorf("the `last-prompt' record should only be a fallback:\n%s", got)
	}
}

// A turn that ran long ends in tool traffic, and tool results wear the `user`
// type. Neither they nor the calls they answer may be mistaken for a prompt.
func TestPreviewLastPromptSkipsToolTraffic(t *testing.T) {
	lines := []string{fmt.Sprintf(typedLine, "run the tests")}
	for i := 0; i < 20; i++ {
		lines = append(lines, toolUseLine, toolResultLine)
	}
	p := writePreviewTranscript(t, "s.jsonl", lines)

	got, err := Adapter{}.Preview(p, session.PreviewOpts{Bytes: previewWindow})
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(got, "last prompt\nrun the tests\n") {
		t.Errorf("want the typed message under the tool traffic:\n%s", got)
	}
}

// The last reply is the last thing the model *said*, not the last record it
// wrote: a turn ends in a tool call more often than not, and a call or a
// thinking block has no text a reader could use.
func TestPreviewLastReplySkipsToolUseAndThinking(t *testing.T) {
	p := writePreviewTranscript(t, "s.jsonl", []string{
		fmt.Sprintf(typedLine, "how many files?"),
		assistantLine("I will count them."),
		thinkingLine,
		toolUseLine,
		toolResultLine,
	})

	got, err := Adapter{}.Preview(p, session.PreviewOpts{Bytes: previewWindow})
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(got, "last reply\nI will count them.\n") {
		t.Errorf("want the last assistant text block as the last reply:\n%s", got)
	}
}

// With nothing typed inside the window and no `last-prompt` record either, the
// head scan says what the session was started for, and the heading says so.
func TestPreviewFallsBackToTheFirstPrompt(t *testing.T) {
	lines := []string{fmt.Sprintf(typedLine, "what this session was for")}
	for i := 0; i < 40; i++ {
		lines = append(lines, toolUseLine, toolResultLine)
	}
	p := writePreviewTranscript(t, "s.jsonl", lines)

	got, err := Adapter{}.Preview(p, session.PreviewOpts{Bytes: 1024})
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(got, "first prompt\nwhat this session was for\n") {
		t.Errorf("want the head scan's first user message:\n%s", got)
	}
	if strings.Contains(got, "last prompt") {
		t.Errorf("the heading should say which prompt this is:\n%s", got)
	}
}

// A transcript under a config home with a live session record for it is shown
// as running, and one with no record is not shown as anything. The record is
// written for this test's own pid, because [recordAlive] checks it.
func TestPreviewStatus(t *testing.T) {
	const uuid = "87e1476d-2222-4222-8222-87e1476d2222"

	newHome := func(t *testing.T, status string) string {
		t.Helper()
		home := t.TempDir()
		dir := filepath.Join(home, "projects", "-tmp-p")
		if err := os.MkdirAll(dir, 0o700); err != nil {
			t.Fatal(err)
		}
		body := fmt.Sprintf(typedLine, "still going") + "\n"
		if err := os.WriteFile(filepath.Join(dir, uuid+".jsonl"), []byte(body), 0o600); err != nil {
			t.Fatal(err)
		}
		if status != "" {
			if err := os.MkdirAll(filepath.Join(home, "sessions"), 0o700); err != nil {
				t.Fatal(err)
			}
			rec := fmt.Sprintf(`{"pid":%d,"sessionId":%q,"status":%q}`, os.Getpid(), uuid, status)
			name := fmt.Sprintf("%d.json", os.Getpid())
			if err := os.WriteFile(filepath.Join(home, "sessions", name), []byte(rec), 0o600); err != nil {
				t.Fatal(err)
			}
		}
		return filepath.Join(dir, uuid+".jsonl")
	}

	cases := []struct{ status, emoji string }{{"busy", "⏳"}, {"idle", "💤"}}
	for _, tc := range cases {
		p := newHome(t, tc.status)

		got, err := Adapter{}.Preview(p, session.PreviewOpts{Bytes: previewWindow})
		if err != nil {
			t.Fatal(err)
		}
		if !strings.HasPrefix(got, tc.emoji+" ") {
			t.Errorf("%s: want the name line prefixed by %q:\n%s", tc.status, tc.emoji, got)
		}
		if !strings.Contains(got, "status") || !strings.Contains(got, tc.status) {
			t.Errorf("%s: want a status row:\n%s", tc.status, got)
		}

		// Compact mode carries the emoji and nothing else: a narrow pane has
		// no room for a row that the name line already says.
		compact, err := Adapter{}.Preview(p, session.PreviewOpts{Bytes: previewWindow, Compact: true})
		if err != nil {
			t.Fatal(err)
		}
		if !strings.HasPrefix(compact, tc.emoji+" ") || strings.Contains(compact, "status") {
			t.Errorf("%s: compact should show the emoji alone:\n%s", tc.status, compact)
		}
	}

	// No record: the session is finished, and nothing is claimed about it.
	got, err := Adapter{}.Preview(newHome(t, ""), session.PreviewOpts{Bytes: previewWindow})
	if err != nil {
		t.Fatal(err)
	}
	if strings.ContainsAny(got, "⏳💤") || strings.Contains(got, "status") {
		t.Errorf("a transcript with no live record should carry no status:\n%s", got)
	}
}
