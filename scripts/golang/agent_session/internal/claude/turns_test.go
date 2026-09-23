package claude

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent_session/internal/turns"
)

func mkRecord(t *testing.T, role, ts string, blocks ...map[string]any) record {
	t.Helper()
	raw, err := json.Marshal(blocks)
	if err != nil {
		t.Fatal(err)
	}
	return record{Type: role, Timestamp: ts, Message: &message{Content: raw}}
}

func decodeAll(records []record) [][]turns.Block {
	out := make([][]turns.Block, len(records))
	for i := range records {
		out[i] = decodeBlocks(records[i].Message)
	}
	return out
}

func renderOne(records []record, opts turns.Style) string {
	blocks := decodeAll(records)
	results := indexResults(records, blocks)
	ts := buildTurns(records, blocks, results)
	return strings.Join(turns.RenderTurns(ts, results, opts, 1), "")
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

	got := renderOne(records, turns.Style{Org: true})
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

	got := renderOne(records, turns.Style{Org: true})
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

	got := renderOne(records, turns.Style{Org: true})
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

	got := renderOne(records, turns.Style{Org: true})
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
		{strings.Repeat("x", turns.ResultInlineMax+1), "*** Result\n"},
	} {
		records := []record{
			mkRecord(t, "assistant", "2026-08-10T10:00:00.000Z", call),
			mkRecord(t, "user", "2026-08-10T10:00:00.000Z", map[string]any{
				"type": "tool_result", "tool_use_id": "tu_1", "content": c.content,
			}),
		}
		got := renderOne(records, turns.Style{Org: true})
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

	got := renderOne(records, turns.Style{Org: true})
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
	if got := renderOne(same, turns.Style{Org: true}); strings.Contains(got, "[10:0") {
		t.Errorf("same minute should not be restamped:\n%s", got)
	}

	later := []record{
		same[0],
		mkRecord(t, "assistant", "2026-08-10T10:07:00.000Z", map[string]any{
			"type": "tool_use", "id": "b", "name": "Bash", "input": map[string]any{"command": "pwd"},
		}),
	}
	got := renderOne(later, turns.Style{Org: true})
	if !strings.Contains(got, "]") || !strings.Contains(got, ":07]") {
		t.Errorf("a different minute should be stamped:\n%s", got)
	}
}

// Nesting a subagent transcript pushes every heading down a level.
func TestBaseLevelOffsetsHeadings(t *testing.T) {
	records := []record{
		mkRecord(t, "user", "2026-08-10T10:00:00.000Z", map[string]any{"type": "text", "text": "hi"}),
	}
	got := renderOne(records, turns.Style{Org: true, Base: 2})
	if !strings.Contains(got, "*** User") {
		t.Errorf("want a level-3 heading with base=2:\n%s", got)
	}
}

func TestEmptyTurnsProduceNoHeading(t *testing.T) {
	records := []record{
		mkRecord(t, "assistant", "2026-08-10T10:00:00.000Z", map[string]any{"type": "thinking", "thinking": "   "}),
	}
	if got := strings.TrimSpace(renderOne(records, turns.Style{Org: true})); got != "" {
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
			if got := renderOne(records, turns.Style{Org: true}); !strings.Contains(got, c.want) {
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
	got := renderOne(conversationRecords([]record{rec}), turns.Style{Org: true})
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
	if got := renderOne(records, turns.Style{Org: true}); !strings.Contains(got, "· 4m2s") {
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

// A message sent while a turn was running reaches the model only as a
// `queued_command` attachment, with no user record of its own. Dropping it
// hid every mid-turn question, so the answer after it read as a non sequitur.
func TestQueuedMessageSplitsTheTurn(t *testing.T) {
	records := conversationRecords([]record{
		mkRecord(t, "assistant", "2026-08-10T10:00:00.000Z",
			map[string]any{"type": "text", "text": "working"}),
		mkRaw(t, map[string]any{
			"type": "attachment", "timestamp": "2026-08-10T10:01:00.000Z",
			"attachment": map[string]any{
				"type": "queued_command", "commandMode": "prompt",
				"prompt": "btw, use Sonnet", "origin": map[string]any{"kind": "human"},
			},
		}),
		mkRecord(t, "assistant", "2026-08-10T10:01:05.000Z",
			map[string]any{"type": "text", "text": "Noted on Sonnet."}),
	})

	got := renderOne(records, turns.Style{Org: true})
	if !strings.Contains(got, "btw, use Sonnet") || !strings.Contains(got, "· queued") {
		t.Errorf("queued message missing or unlabelled:\n%s", got)
	}
	if n := strings.Count(got, "* Assistant"); n != 2 {
		t.Errorf("want the queued message to split the answer in two, got %d assistant headings:\n%s", n, got)
	}
	if strings.Index(got, "btw") > strings.Index(got, "Noted") {
		t.Errorf("queued message should precede the reply to it:\n%s", got)
	}
}

// A queued message the model was also handed as a record of its own is shown
// once, as that record.
func TestQueuedMessageDeliveredAsRecordIsNotRepeated(t *testing.T) {
	note := "<task-notification><task-id>b1</task-id><status>completed</status></task-notification>"
	records := conversationRecords([]record{
		mkRaw(t, map[string]any{
			"type": "attachment", "timestamp": "2026-08-10T10:01:00.000Z",
			"attachment": map[string]any{
				"type": "queued_command", "commandMode": "task-notification", "prompt": note,
			},
		}),
		mkRaw(t, map[string]any{
			"type": "user", "timestamp": "2026-08-10T10:01:01.000Z",
			"message": map[string]any{"content": note},
		}),
	})
	if len(records) != 1 || records[0].Type != "user" {
		t.Errorf("want only the user record kept, got %d records", len(records))
	}
}

// A compaction summary quotes the messages before it; that is not a delivery,
// and must not hide the queued message it quotes.
func TestCompactSummaryDoesNotHideQueuedMessage(t *testing.T) {
	records := conversationRecords([]record{
		mkRaw(t, map[string]any{
			"type": "attachment", "timestamp": "2026-08-10T10:01:00.000Z",
			"attachment": map[string]any{
				"type": "queued_command", "commandMode": "prompt", "prompt": "what happened to X?",
			},
		}),
		mkRaw(t, map[string]any{
			"type": "user", "isCompactSummary": true, "timestamp": "2026-08-10T11:00:00.000Z",
			"message": map[string]any{"content": "Summary. User asked: what happened to X?"},
		}),
	})
	if len(records) != 2 {
		t.Errorf("want the queued message kept beside the summary, got %d records", len(records))
	}
}

// A short queued reply is not a copy of every longer message that contains it.
func TestShortQueuedMessageIsNotMistakenForADuplicate(t *testing.T) {
	records := conversationRecords([]record{
		mkRaw(t, map[string]any{
			"type": "user", "timestamp": "2026-08-10T10:00:00.000Z",
			"message": map[string]any{"content": "ok, now run the second cell"},
		}),
		mkRaw(t, map[string]any{
			"type": "attachment", "timestamp": "2026-08-10T10:01:00.000Z",
			"attachment": map[string]any{"type": "queued_command", "commandMode": "prompt", "prompt": "ok"},
		}),
	})
	if len(records) != 2 {
		t.Errorf("want the queued \"ok\" kept, got %d records", len(records))
	}
}

// Claude Code marks both its own notes and the messages other agents send as
// `isMeta`; only the second kind says where it came from.
func TestDeliveredMetaMessagesRender(t *testing.T) {
	meta := func(content string, extra map[string]any) record {
		obj := map[string]any{
			"type": "user", "isMeta": true, "timestamp": "2026-08-10T10:02:00.000Z",
			"message": map[string]any{"content": content},
		}
		for k, v := range extra {
			obj[k] = v
		}
		return mkRaw(t, obj)
	}
	records := conversationRecords([]record{
		meta("Another Claude session sent a message: hand-back text",
			map[string]any{"origin": map[string]any{"kind": "peer", "from": "a1ece39e"}, "promptSource": "system"}),
		meta("The coordinator sent a message while you were working: stop",
			map[string]any{"origin": map[string]any{"kind": "coordinator"}}),
		meta("Hourly recovery check", map[string]any{"promptSource": "system"}),
		meta("Base directory for this skill: /x", nil),
		meta("[Image: original 100x100]", nil),
	})
	if len(records) != 3 {
		t.Fatalf("want the three delivered messages kept, got %d", len(records))
	}

	got := renderOne(records, turns.Style{Org: true})
	for _, want := range []string{
		"* Agent message [", "· from a1ece39e", "hand-back text",
		"* Coordinator message [", "* Scheduled prompt [",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("want %q in:\n%s", want, got)
		}
	}
	// The hourly prompt repeats; it opens closed.
	if i := strings.Index(got, "* Scheduled prompt"); i < 0 || !strings.Contains(got[i:], ":VISIBILITY: folded") {
		t.Errorf("scheduled prompt should be folded:\n%s", got)
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

// The record Claude Code writes to name the seat the session is running on. The
// `[1m]` suffix of a long-context seat appears here and nowhere else.
func mkSeat(id string) record {
	return record{Type: "attachment", Attachment: &attachment{
		Type: "model", Identity: &modelIdentity{ModelID: id},
	}}
}

func mkUsage(model string, in, cacheCreate, cacheRead, out int) record {
	return record{Type: "assistant", Message: &message{Model: model, Usage: &usage{
		InputTokens:         in,
		CacheCreationTokens: cacheCreate,
		CacheReadTokens:     cacheRead,
		OutputTokens:        out,
	}}}
}

// The line shows the state after the last model response, so the newest record
// that actually reported a figure wins, and nothing that never held the window
// may displace it.
func TestLastContextUsage(t *testing.T) {
	seat := mkSeat("claude-opus-5")
	last := mkUsage("claude-opus-5", 1000, 2000, 139310, 50000)

	cases := []struct {
		name      string
		in        []record
		inherited string
		want      turns.ContextUsage
	}{
		{name: "nothing said", want: turns.ContextUsage{}},
		{
			// The output tokens came back and are in the *next* request's
			// window, not this one's: 1000 + 2000 + 139310, and not a token of
			// the 50000.
			name: "the last reporting record wins, output excluded",
			in:   []record{seat, mkUsage("claude-opus-5", 9, 9, 9, 9), last},
			want: turns.ContextUsage{Used: 142310},
		},
		{
			// A message Claude Code wrote itself never made a request.
			name: "a trailing synthetic record does not win",
			in:   []record{seat, last, mkUsage("<synthetic>", 1, 1, 1, 1)},
			want: turns.ContextUsage{Used: 142310},
		},
		{
			// A resumed or interrupted turn leaves the counters behind at zero.
			name: "an all-zero usage does not win",
			in:   []record{seat, last, mkUsage("claude-opus-5", 0, 0, 0, 0)},
			want: turns.ContextUsage{Used: 142310},
		},
		{
			name: "a meta assistant record does not win",
			in: func() []record {
				meta := mkUsage("claude-opus-5", 5, 5, 5, 5)
				meta.IsMeta = true
				return []record{seat, last, meta}
			}(),
			want: turns.ContextUsage{Used: 142310},
		},
		{
			name: "a user record does not win",
			in: func() []record {
				u := mkUsage("claude-opus-5", 5, 5, 5, 5)
				u.Type = "user"
				return []record{seat, last, u}
			}(),
			want: turns.ContextUsage{Used: 142310},
		},
		{
			// The seat, not the message, is where `[1m]` is written.
			name: "a long-context seat widens the window",
			in:   []record{mkSeat("claude-opus-5[1m]"), last},
			want: turns.ContextUsage{Used: 142310, Window: 1000000},
		},
		{
			// A transcript from before Claude Code wrote the record. No
			// denominator is better than a wrong one.
			name: "no seat leaves the window unknown",
			in:   []record{last},
			want: turns.ContextUsage{Used: 142310},
		},
		{
			// A bare id does not say which seat: locally, transcripts naming
			// a plain `claude-opus-5` peak well past 200,000.
			name: "a bare seat leaves the window unknown too",
			in:   []record{mkSeat("claude-opus-5"), last},
			want: turns.ContextUsage{Used: 142310},
		},
		{
			// A subagent, which never carries a seat of its own.
			name:      "an inherited seat fills in for a file that names none",
			in:        []record{last},
			inherited: "claude-opus-5[1m]",
			want:      turns.ContextUsage{Used: 142310, Window: 1000000},
		},
		{
			// A subagent that was put on another model does not inherit.
			name:      "an inherited seat for another model is dropped",
			in:        []record{mkUsage("claude-haiku-4-5-20251001", 1000, 2000, 139310, 0)},
			inherited: "claude-opus-5[1m]",
			want:      turns.ContextUsage{Used: 142310},
		},
		{
			// A resumed session writes its seat record at the resume, which
			// can land after the last message that reported usage.
			name: "a seat after the last figure still counts",
			in:   []record{last, mkSeat("claude-opus-5[1m]")},
			want: turns.ContextUsage{Used: 142310, Window: 1000000},
		},
		{
			// `/model` mid-session: the nearest seat *behind* the message is
			// the one it ran on, not the last one in the file.
			name: "the nearest preceding seat wins over a later one",
			in: []record{
				mkSeat("claude-opus-5[1m]"), last,
				mkSeat("claude-fable-5-1"), mkUsage("claude-fable-5-1", 0, 0, 0, 0),
			},
			want: turns.ContextUsage{Used: 142310, Window: 1000000},
		},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			if got := lastContextUsage(c.in, c.inherited); got != c.want {
				t.Errorf("got %+v, want %+v", got, c.want)
			}
		})
	}
}

// Claude Code writes the usage but never the capacity, so the window comes from
// the seat.
func TestContextWindowInference(t *testing.T) {
	for _, c := range []struct {
		seat  string
		model string
		used  int
		want  int
	}{
		{"claude-opus-5[1m]", "claude-opus-5", 1000, 1_000_000},
		// A bare id does not say which seat, so it earns no denominator.
		{"claude-opus-5", "claude-opus-5", 1000, windowUnknown},
		{"", "claude-opus-5", 1000, windowUnknown},
		// A session holding this much proves the large seat by itself.
		{"claude-opus-5", "claude-opus-5", 240_000, 1_000_000},
		{"", "claude-opus-5", 240_000, 1_000_000},
		// A seat that does not describe this message is not believed: a
		// subagent on its own model keeps none of its parent's `[1m]`.
		{"claude-opus-5[1m]", "claude-haiku-4-5-20251001", 1000, windowUnknown},
	} {
		if got := contextWindow(c.seat, c.model, c.used); got != c.want {
			t.Errorf("contextWindow(%q, %q, %d) = %d, want %d", c.seat, c.model, c.used, got, c.want)
		}
	}
}

// The usage field rides on the same narrow struct both tail scans decode, so a
// record without one must still decode rather than fail the line.
func TestMsgRecordStillDecodesWithoutUsage(t *testing.T) {
	rec := mkRaw(t, map[string]any{
		"type": "assistant", "timestamp": "2026-08-10T10:00:00.000Z",
		"message": map[string]any{
			"model":   "claude-opus-5",
			"content": []map[string]any{{"type": "text", "text": "hi"}},
		},
	})
	if rec.Message == nil || rec.Message.Usage != nil {
		t.Fatalf("message = %+v", rec.Message)
	}
	if got := lastContextUsage([]record{rec}, ""); got != (turns.ContextUsage{}) {
		t.Errorf("got %+v, want the zero value", got)
	}
}
