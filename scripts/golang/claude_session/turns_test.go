package main

import (
	"encoding/json"
	"flag"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func mkRecord(t *testing.T, role, ts string, blocks ...map[string]any) record {
	t.Helper()
	raw, err := json.Marshal(blocks)
	if err != nil {
		t.Fatal(err)
	}
	return record{Type: role, Timestamp: ts, Message: &message{Content: raw}}
}

func decodeAll(records []record) [][]block {
	out := make([][]block, len(records))
	for i := range records {
		out[i] = decodeBlocks(records[i].Message)
	}
	return out
}

func renderOne(records []record, opts renderOpts) string {
	blocks := decodeAll(records)
	results := indexResults(records, blocks)
	turns := buildTurns(records, blocks, results)
	return strings.Join(renderTurns(turns, results, opts, 1), "")
}

// Claude Code writes one record per content block, so an assistant turn must
// not become a run of near-identical headings.
func TestConsecutiveSameRoleRecordsMerge(t *testing.T) {
	records := []record{
		mkRecord(t, "user", "2026-08-10T10:00:00.000Z", map[string]any{"type": "text", "text": "hi"}),
		mkRecord(t, "assistant", "2026-08-10T10:01:00.000Z", map[string]any{"type": "text", "text": "one"}),
		mkRecord(t, "assistant", "2026-08-10T10:01:10.000Z", map[string]any{"type": "text", "text": "two"}),
		mkRecord(t, "assistant", "2026-08-10T10:01:20.000Z", map[string]any{"type": "text", "text": "three"}),
	}

	got := renderOne(records, renderOpts{org: true})
	if n := strings.Count(got, "* Assistant"); n != 1 {
		t.Errorf("want 1 assistant heading, got %d:\n%s", n, got)
	}
	for _, want := range []string{"one", "two", "three"} {
		if !strings.Contains(got, want) {
			t.Errorf("lost content %q:\n%s", want, got)
		}
	}
}

// A tool result arrives as a user record. It belongs under the call it
// answers, not as a message nobody wrote.
func TestToolResultNestsUnderItsCall(t *testing.T) {
	records := []record{
		mkRecord(t, "assistant", "2026-08-10T10:00:00.000Z", map[string]any{
			"type": "tool_use", "id": "tu_1", "name": "Bash",
			"input": map[string]any{"command": "ls", "description": "List things"},
		}),
		mkRecord(t, "user", "2026-08-10T10:00:01.000Z", map[string]any{
			"type": "tool_result", "tool_use_id": "tu_1", "content": "a\nb\nc\nd",
		}),
	}

	got := renderOne(records, renderOpts{org: true})
	if strings.Contains(got, "* User") {
		t.Errorf("a results-only record should not produce a user heading:\n%s", got)
	}
	if !strings.Contains(got, "** Tool Use: Bash · List things") {
		t.Errorf("missing tool heading:\n%s", got)
	}
	if !strings.Contains(got, "*** Result") {
		t.Errorf("result should be nested one level under the call:\n%s", got)
	}
}

// A result whose call is not in this transcript still has to show up.
func TestOrphanToolResultStillRenders(t *testing.T) {
	records := []record{
		mkRecord(t, "user", "2026-08-10T10:00:00.000Z", map[string]any{
			"type": "tool_result", "tool_use_id": "missing", "content": "stranded",
		}),
	}

	got := renderOne(records, renderOpts{org: true})
	if !strings.Contains(got, "stranded") {
		t.Errorf("orphan result was dropped:\n%s", got)
	}
}

// A user record that mixes results with typed text keeps its heading for the
// text.
func TestUserRecordWithBothResultAndText(t *testing.T) {
	records := []record{
		mkRecord(t, "assistant", "2026-08-10T10:00:00.000Z", map[string]any{
			"type": "tool_use", "id": "tu_1", "name": "Bash",
			"input": map[string]any{"command": "ls"},
		}),
		mkRecord(t, "user", "2026-08-10T10:00:01.000Z",
			map[string]any{"type": "tool_result", "tool_use_id": "tu_1", "content": "out"},
			map[string]any{"type": "text", "text": "actually stop"},
		),
	}

	got := renderOne(records, renderOpts{org: true})
	if !strings.Contains(got, "* User") {
		t.Errorf("typed text needs its own user heading:\n%s", got)
	}
	if !strings.Contains(got, "actually stop") {
		t.Errorf("lost the typed text:\n%s", got)
	}
}

func TestShortResultGoesOnTheHeading(t *testing.T) {
	call := map[string]any{
		"type": "tool_use", "id": "tu_1", "name": "Bash",
		"input": map[string]any{"command": "false"},
	}

	for _, c := range []struct{ content, want string }{
		{"Exit code 1", "*** Result: Exit code 1"},
		{"", "*** Result: (no output)"},
		{"line one\nline two", "*** Result\n"},
		{strings.Repeat("x", resultInlineMax+1), "*** Result\n"},
	} {
		records := []record{
			mkRecord(t, "assistant", "2026-08-10T10:00:00.000Z", call),
			mkRecord(t, "user", "2026-08-10T10:00:00.000Z", map[string]any{
				"type": "tool_result", "tool_use_id": "tu_1", "content": c.content,
			}),
		}
		got := renderOne(records, renderOpts{org: true})
		if !strings.Contains(got, c.want) {
			t.Errorf("content %q: want %q in:\n%s", c.content, c.want, got)
		}
	}
}

// A short command inlined as =...= breaks the moment it contains an `=`, so
// code-bearing keys are always blocks.
func TestCommandAlwaysRendersAsABlock(t *testing.T) {
	records := []record{
		mkRecord(t, "assistant", "2026-08-10T10:00:00.000Z", map[string]any{
			"type": "tool_use", "id": "tu_1", "name": "Bash",
			"input": map[string]any{"command": `echo "=== zshlang ==="`},
		}),
	}

	got := renderOne(records, renderOpts{org: true})
	if strings.Contains(got, "- command ::") {
		t.Errorf("command must not be inlined as a bullet:\n%s", got)
	}
	if !strings.Contains(got, "#+begin_src zsh\necho \"=== zshlang ===\"\n#+end_src") {
		t.Errorf("command should be a zsh block:\n%s", got)
	}
}

// The turn heading already carries the time; a sub-heading repeats it only
// when it says something different.
func TestSubHeadingStampsOnlyWhenTheyDiffer(t *testing.T) {
	same := []record{
		mkRecord(t, "assistant", "2026-08-10T10:00:00.000Z", map[string]any{
			"type": "tool_use", "id": "a", "name": "Bash", "input": map[string]any{"command": "ls"},
		}),
		mkRecord(t, "assistant", "2026-08-10T10:00:30.000Z", map[string]any{
			"type": "tool_use", "id": "b", "name": "Bash", "input": map[string]any{"command": "pwd"},
		}),
	}
	if got := renderOne(same, renderOpts{org: true}); strings.Contains(got, "[10:0") {
		t.Errorf("same minute should not be restamped:\n%s", got)
	}

	later := []record{
		same[0],
		mkRecord(t, "assistant", "2026-08-10T10:07:00.000Z", map[string]any{
			"type": "tool_use", "id": "b", "name": "Bash", "input": map[string]any{"command": "pwd"},
		}),
	}
	got := renderOne(later, renderOpts{org: true})
	if !strings.Contains(got, "]") || !strings.Contains(got, ":07]") {
		t.Errorf("a different minute should be stamped:\n%s", got)
	}
}

// Nesting a subagent transcript pushes every heading down a level.
func TestBaseLevelOffsetsHeadings(t *testing.T) {
	records := []record{
		mkRecord(t, "user", "2026-08-10T10:00:00.000Z", map[string]any{"type": "text", "text": "hi"}),
	}
	got := renderOne(records, renderOpts{org: true, base: 2})
	if !strings.Contains(got, "*** User") {
		t.Errorf("want a level-3 heading with base=2:\n%s", got)
	}
}

func TestEmptyTurnsProduceNoHeading(t *testing.T) {
	records := []record{
		mkRecord(t, "assistant", "2026-08-10T10:00:00.000Z", map[string]any{"type": "thinking", "thinking": "   "}),
	}
	if got := strings.TrimSpace(renderOne(records, renderOpts{org: true})); got != "" {
		t.Errorf("want nothing, got:\n%s", got)
	}
}

func mkRaw(t *testing.T, obj map[string]any) record {
	t.Helper()
	raw, err := json.Marshal(obj)
	if err != nil {
		t.Fatal(err)
	}
	var rec record
	if err := json.Unmarshal(raw, &rec); err != nil {
		t.Fatal(err)
	}
	return rec
}

// Recaps, notices and the rest are system records, not messages, which is why
// they were being dropped along with the bookkeeping types.
func TestEventRecordsRender(t *testing.T) {
	base := mkRecord(t, "assistant", "2026-08-10T10:00:00.000Z",
		map[string]any{"type": "text", "text": "working"})

	cases := []struct {
		name string
		rec  map[string]any
		want string
	}{
		{"recap", map[string]any{
			"type": "system", "subtype": "away_summary",
			"timestamp": "2026-08-10T10:05:00.000Z", "content": "Goal was X; next Y.",
		}, "** Recap"},
		{"informational", map[string]any{
			"type": "system", "subtype": "informational",
			"timestamp": "2026-08-10T10:05:00.000Z", "content": "Auto mode lets Claude...",
		}, "** Notice"},
		{"model fallback", map[string]any{
			"type": "system", "subtype": "model_consent_fallback",
			"timestamp": "2026-08-10T10:05:00.000Z", "content": "Switched to Sonnet 5",
		}, "** Model fallback"},
		{"slash command", map[string]any{
			"type": "system", "subtype": "local_command",
			"timestamp": "2026-08-10T10:05:00.000Z",
			"content":   "<command-name>/model</command-name>\n<command-message>model</command-message>",
		}, "** Command: /model"},
		{"pull request", map[string]any{
			"type": "pr-link", "timestamp": "2026-08-10T10:05:00.000Z",
			"prNumber": 1306, "prRepository": "y3owk1n/neru",
			"prUrl": "https://github.com/y3owk1n/neru/pull/1306",
		}, "** Pull request y3owk1n/neru#1306"},
		{"externally edited file", map[string]any{
			"type": "attachment", "timestamp": "2026-08-10T10:05:00.000Z",
			"attachment": map[string]any{
				"type": "edited_text_file", "displayPath": "config.toml", "snippet": "1\tx",
			},
		}, "** Edited outside the session · config.toml"},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			records := conversationRecords([]record{base, mkRaw(t, c.rec)})
			if len(records) != 2 {
				t.Fatalf("record was filtered out: kept %d of 2", len(records))
			}
			if got := renderOne(records, renderOpts{org: true}); !strings.Contains(got, c.want) {
				t.Errorf("want %q in:\n%s", c.want, got)
			}
		})
	}
}

// A compaction separates two phases of a conversation, so it stands alone
// rather than hanging off whichever turn happened to precede it.
func TestCompactBoundaryIsItsOwnTurn(t *testing.T) {
	rec := mkRaw(t, map[string]any{
		"type": "system", "subtype": "compact_boundary",
		"timestamp": "2026-08-10T10:05:00.000Z", "content": "Conversation compacted",
		"compactMetadata": map[string]any{
			"trigger": "manual", "preTokens": 476980, "postTokens": 11820,
		},
	})
	got := renderOne(conversationRecords([]record{rec}), renderOpts{org: true})
	if !strings.Contains(got, "* Context compacted") || !strings.Contains(got, "476980 → 11820 tokens") {
		t.Errorf("got:\n%s", got)
	}
}

// turn_duration measures the turn before it and belongs in its heading.
func TestTurnDurationLandsOnTheHeading(t *testing.T) {
	records := conversationRecords([]record{
		mkRecord(t, "assistant", "2026-08-10T10:00:00.000Z",
			map[string]any{"type": "text", "text": "working"}),
		mkRaw(t, map[string]any{
			"type": "system", "subtype": "turn_duration",
			"timestamp": "2026-08-10T10:04:02.000Z", "durationMs": 242000,
		}),
	})
	if got := renderOne(records, renderOpts{org: true}); !strings.Contains(got, "· 4m2s") {
		t.Errorf("want the duration on the heading:\n%s", got)
	}
}

// Bookkeeping must not reach the document.
func TestBookkeepingRecordsAreDropped(t *testing.T) {
	for _, obj := range []map[string]any{
		{"type": "mode", "mode": "normal"},
		{"type": "permission-mode", "permissionMode": "auto"},
		{"type": "bridge-session", "bridgeSessionId": "cse_x"},
		{"type": "file-history-snapshot", "messageId": "x"},
		{"type": "last-prompt", "lastPrompt": "hi"},
		{"type": "queue-operation", "operation": "enqueue", "content": "later"},
		{"type": "system", "subtype": "stop_hook_summary", "level": "suggestion"},
		{"type": "attachment", "attachment": map[string]any{"type": "task_reminder"}},
	} {
		if got := conversationRecords([]record{mkRaw(t, obj)}); len(got) != 0 {
			t.Errorf("%v should have been dropped", obj["type"])
		}
	}
}

func TestShortDuration(t *testing.T) {
	for _, c := range []struct {
		ms   int64
		want string
	}{{0, ""}, {4500, "4s"}, {242000, "4m2s"}, {566155, "9m26s"}, {7500000, "2h5m"}} {
		if got := shortDuration(time.Duration(c.ms) * time.Millisecond); got != c.want {
			t.Errorf("shortDuration(%dms) = %q, want %q", c.ms, got, c.want)
		}
	}
}

// Paths inside ~/.claude/projects start with a dash, since project
// directories are named after the cwd they belong to.
func TestGuardPathArgs(t *testing.T) {
	dir := t.TempDir()
	dashed := filepath.Join(dir, "-Users-evar-scripts.jsonl")
	if err := os.WriteFile(dashed, []byte("{}\n"), 0o600); err != nil {
		t.Fatal(err)
	}

	cwd, _ := os.Getwd()
	defer os.Chdir(cwd)
	if err := os.Chdir(dir); err != nil {
		t.Fatal(err)
	}

	fs := flag.NewFlagSet("render", flag.ContinueOnError)
	format := fs.String("format", "md", "")
	diff := fs.Bool("diff", true, "")

	argv := []string{"-format=org", "-diff=false", "-Users-evar-scripts.jsonl"}
	if err := fs.Parse(guardPathArgs(fs, argv)); err != nil {
		t.Fatalf("parse: %v", err)
	}

	if *format != "org" || *diff {
		t.Errorf("real flags stopped parsing: format=%q diff=%v", *format, *diff)
	}
	if got := fs.Arg(0); got != "./-Users-evar-scripts.jsonl" {
		t.Errorf("path argument = %q, want it spelled with a ./ prefix", got)
	}
}

// A file named like a flag must not shadow the flag.
func TestGuardPathArgsLeavesRealFlagsAlone(t *testing.T) {
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "-diff"), []byte("x"), 0o600); err != nil {
		t.Fatal(err)
	}
	cwd, _ := os.Getwd()
	defer os.Chdir(cwd)
	os.Chdir(dir)

	fs := flag.NewFlagSet("render", flag.ContinueOnError)
	diff := fs.Bool("diff", false, "")
	if err := fs.Parse(guardPathArgs(fs, []string{"-diff"})); err != nil {
		t.Fatalf("parse: %v", err)
	}
	if !*diff {
		t.Error("-diff was treated as a path instead of a flag")
	}
}

// agent-name is the name Claude Code resolved for itself, so it wins when
// present; it just does not always exist.
func TestSessionNamePrecedence(t *testing.T) {
	line := func(obj map[string]any) string {
		b, _ := json.Marshal(obj)
		return string(b) + "\n"
	}

	cases := []struct{ name, body, want string }{
		{"agent-name wins",
			line(map[string]any{"type": "custom-title", "customTitle": "Custom"}) +
				line(map[string]any{"type": "agent-name", "agentName": "Resolved"}),
			"Resolved"},
		{"custom-title beats ai-title",
			line(map[string]any{"type": "ai-title", "aiTitle": "Generated"}) +
				line(map[string]any{"type": "custom-title", "customTitle": "Custom"}),
			"Custom"},
		{"ai-title beats slug",
			line(map[string]any{"type": "user", "slug": "sharded-bouncing-clarke"}) +
				line(map[string]any{"type": "ai-title", "aiTitle": "Generated"}),
			"Generated"},
		{"last name wins",
			line(map[string]any{"type": "agent-name", "agentName": "First"}) +
				line(map[string]any{"type": "agent-name", "agentName": "Second"}),
			"Second"},
		{"unnamed session", line(map[string]any{"type": "user"}), ""},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			path := filepath.Join(t.TempDir(), "s.jsonl")
			if err := os.WriteFile(path, []byte(c.body), 0o600); err != nil {
				t.Fatal(err)
			}
			fh, err := os.Open(path)
			if err != nil {
				t.Fatal(err)
			}
			defer fh.Close()

			if got := sessionName(fh); got != c.want {
				t.Errorf("sessionName = %q, want %q", got, c.want)
			}
		})
	}
}
