package main

import (
	"encoding/json"
	"strings"
	"testing"
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
