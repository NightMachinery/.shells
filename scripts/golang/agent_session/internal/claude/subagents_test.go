package claude

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent_session/internal/session"
	"agent_session/internal/turns"
)

func TestModelLabel(t *testing.T) {
	cases := []struct{ in, want string }{
		{"claude-opus-5", "Opus5"},
		{"claude-sonnet-5", "Sonnet5"},
		{"claude-fable-5", "Fable5"},
		{"claude-fable-5-1", "Fable5.1"},
		// The build date is noise in a heading; shortModel already drops it.
		{"claude-haiku-4-5-20251001", "Haiku4.5"},
		// Not a model, but if it ever reached here it must not crash.
		{"<synthetic>", "Synthetic"},
		{"", ""},
		// Anything unrecognised keeps its words, one capital each, minus the
		// vendor prefix.
		{"claude-something-odd", "SomethingOdd"},
		{"codex-auto-review", "CodexAutoReview"},
		{"gpt-5-codex", "GPT5Codex"},
	}

	for _, c := range cases {
		if got := turns.ModelLabel(c.in); got != c.want {
			t.Errorf("turns.ModelLabel(%q) = %q, want %q", c.in, got, c.want)
		}
	}
}

func assistantAt(model string) record {
	return record{Type: "assistant", Message: &message{Model: model}}
}

func TestModelMode(t *testing.T) {
	cases := []struct {
		name string
		in   []record
		want string
	}{
		{"nothing", nil, ""},
		{
			name: "one model throughout",
			in:   []record{assistantAt("claude-opus-5"), assistantAt("claude-opus-5")},
			want: "claude-opus-5",
		},
		{
			// The case the mode exists for: a stray record must not decide it.
			name: "a minority model does not win",
			in: []record{
				assistantAt("claude-opus-5"), assistantAt("claude-sonnet-5"),
				assistantAt("claude-opus-5"), assistantAt("claude-opus-5"),
			},
			want: "claude-opus-5",
		},
		{
			// `<synthetic>` marks a message Claude Code wrote itself, so it gets
			// no vote even when it is the commonest record.
			name: "synthetic does not vote",
			in: []record{
				assistantAt("<synthetic>"), assistantAt("<synthetic>"),
				assistantAt("<synthetic>"), assistantAt("claude-fable-5-1"),
			},
			want: "claude-fable-5-1",
		},
		{
			name: "synthetic only leaves nothing to report",
			in:   []record{assistantAt("<synthetic>")},
			want: "",
		},
		{
			// Deterministic, because the document has to be byte-identical run
			// to run: a tie goes to whichever was seen first, not to whatever
			// map iteration happens to yield.
			name: "a tie goes to the first seen",
			in: []record{
				assistantAt("claude-sonnet-5"), assistantAt("claude-opus-5"),
				assistantAt("claude-sonnet-5"), assistantAt("claude-opus-5"),
			},
			want: "claude-sonnet-5",
		},
		{
			name: "user records and empty models are ignored",
			in: []record{
				{Type: "user", Message: &message{Model: "claude-opus-5"}},
				assistantAt(""),
				{Type: "assistant"},
				assistantAt("claude-haiku-4-5-20251001"),
			},
			want: "claude-haiku-4-5-20251001",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := modelMode(tc.in); got != tc.want {
				t.Errorf("got %q, want %q", got, tc.want)
			}
		})
	}
}

func TestSubagentTitle(t *testing.T) {
	full := subagent{id: "abc", meta: subagentMeta{AgentType: "Explore", Description: "Find the thing"}}

	if got, want := full.title("claude-opus-5"), "@Opus5 Explore · Find the thing"; got != want {
		t.Errorf("with a model: got %q, want %q", got, want)
	}
	// A transcript that never said which model keeps the heading it always had.
	if got, want := full.title(""), "Explore · Find the thing"; got != want {
		t.Errorf("without a model: got %q, want %q", got, want)
	}

	bare := subagent{id: "abc"}
	if got, want := bare.title("claude-fable-5-1"), "@Fable5.1 Subagent abc"; got != want {
		t.Errorf("no meta: got %q, want %q", got, want)
	}
	if got, want := bare.title(""), "Subagent abc"; got != want {
		t.Errorf("no meta, no model: got %q, want %q", got, want)
	}
}

// A parent and an inlined agent each report their own window: the agent runs
// in one of its own, which is half the reason to spawn it. Both windows come
// from the parent's seat record, since `message.model` never spells the `[1m]`
// of a long-context seat and a subagent transcript names no seat at all.
// Synthetic store, written from the documented record shape; no real transcript
// is read.
func TestDocumentAndSubagentContextWindow(t *testing.T) {
	for _, c := range []struct {
		seatID string
		window int
		docPct string
		subPct string
	}{
		{"claude-opus-5", windowUnknown, "142,310 tokens", "8,000 tokens"},
		{"claude-opus-5[1m]", 1_000_000, "142,310 / 1,000,000 tokens (14%)", "8,000 / 1,000,000 tokens (1%)"},
	} {
		t.Run(c.seatID, func(t *testing.T) {
			documentAndSubagentContextWindow(t, c.seatID, c.window, c.docPct, c.subPct)
		})
	}
}

func documentAndSubagentContextWindow(t *testing.T, seatID string, window int, docLine, subLine string) {
	t.Helper()
	dir := t.TempDir()
	id := "11111111-1111-4111-8111-111111111111"

	line := func(obj map[string]any) string {
		b, err := json.Marshal(obj)
		if err != nil {
			t.Fatal(err)
		}
		return string(b) + "\n"
	}
	assistant := func(ts string, in, cacheCreate, cacheRead, out int, blocks ...map[string]any) string {
		return line(map[string]any{
			"type": "assistant", "timestamp": ts,
			"message": map[string]any{
				"model": "claude-opus-5", "content": blocks,
				"usage": map[string]any{
					"input_tokens": in, "cache_creation_input_tokens": cacheCreate,
					"cache_read_input_tokens": cacheRead, "output_tokens": out,
				},
			},
		})
	}

	parent := filepath.Join(dir, id+".jsonl")
	// The seat record. Only the parent gets one: no subagent transcript carries
	// one, so the agent's own line can only come from inheriting this.
	body := line(map[string]any{
		"type": "attachment", "timestamp": "2026-08-10T10:00:00.000Z",
		"attachment": map[string]any{
			"type":     "model",
			"identity": map[string]any{"modelId": seatID},
		},
	}) + line(map[string]any{
		"type": "user", "timestamp": "2026-08-10T10:00:00.000Z",
		"message": map[string]any{"content": []map[string]any{{"type": "text", "text": "go and look"}}},
	}) +
		assistant("2026-08-10T10:00:10.000Z", 500, 1000, 20000, 300,
			map[string]any{"type": "tool_use", "id": "tu_1", "name": "Agent",
				"input": map[string]any{"description": "Find the thing"}}) +
		assistant("2026-08-10T10:05:00.000Z", 1000, 2000, 139310, 50000,
			map[string]any{"type": "text", "text": "found it"})
	if err := os.WriteFile(parent, []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}

	subDir := filepath.Join(dir, id, "subagents")
	if err := os.MkdirAll(subDir, 0o755); err != nil {
		t.Fatal(err)
	}
	subBody := line(map[string]any{
		"type": "user", "timestamp": "2026-08-10T10:00:11.000Z",
		"message": map[string]any{"content": []map[string]any{{"type": "text", "text": "find the thing"}}},
	}) +
		assistant("2026-08-10T10:00:20.000Z", 500, 500, 7000, 200,
			map[string]any{"type": "text", "text": "here it is"})
	if err := os.WriteFile(filepath.Join(subDir, "agent-1.jsonl"), []byte(subBody), 0o600); err != nil {
		t.Fatal(err)
	}
	meta := line(map[string]any{
		"agentType": "Explore", "description": "Find the thing", "toolUseId": "tu_1",
	})
	if err := os.WriteFile(filepath.Join(subDir, "agent-1.meta.json"), []byte(meta), 0o600); err != nil {
		t.Fatal(err)
	}

	doc, err := Adapter{}.Document(parent, session.DocOpts{Subagents: true})
	if err != nil {
		t.Fatal(err)
	}
	if want := (turns.ContextUsage{Used: 142310, Window: window}); doc.Context != want {
		t.Errorf("document context = %+v, want %+v", doc.Context, want)
	}
	if len(doc.Subagents) != 1 {
		t.Fatalf("subagents = %+v", doc.Subagents)
	}
	if want := (turns.ContextUsage{Used: 8000, Window: window}); doc.Subagents[0].Context != want {
		t.Errorf("subagent context = %+v, want %+v", doc.Subagents[0].Context, want)
	}

	out, err := turns.Render(doc, turns.Options{Format: "org", Jobs: 1})
	if err != nil {
		t.Fatal(err)
	}
	if !strings.HasPrefix(out, "Context window: "+docLine+"\n") {
		t.Errorf("the session's own line should open the document:\n%s", out)
	}
	want := "** @Opus5 Explore · Find the thing\n:PROPERTIES:\n:VISIBILITY: folded\n:END:\n\n" +
		"Context window: " + subLine
	if !strings.Contains(out, want) {
		t.Errorf("the agent's line should sit right under its heading:\n%s", out)
	}
}
