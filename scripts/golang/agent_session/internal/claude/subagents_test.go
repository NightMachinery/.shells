package claude

import (
	"testing"

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
