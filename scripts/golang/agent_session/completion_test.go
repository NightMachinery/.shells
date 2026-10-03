package main

import (
	"agent_session/internal/claude"
	"agent_session/internal/codex"
	"os"
	"path/filepath"
	"testing"
)

func TestCompletionContext(t *testing.T) {
	for _, agent := range []string{"claude", "codex"} {
		text := `{"type":"assistant","message":{"role":"assistant","content":[{"type":"text","text":"alpha\nreply"}]}}
{"type":"assistant","message":{"role":"assistant","content":[{"type":"thinking","thinking":"HIDDEN"}]}}
{"type":"user","message":{"role":"user","content":[{"type":"text","text":"newest"}]}}
`
		if agent == "codex" {
			text = `{"type":"response_item","payload":{"type":"message","role":"assistant","content":[{"type":"output_text","text":"alpha\nreply"}]}}
{"type":"response_item","payload":{"type":"reasoning","summary":[],"encrypted_content":"HIDDEN"}}
{"type":"response_item","payload":{"type":"message","role":"user","content":[{"type":"input_text","text":"newest"}]}}
`
		}
		p := filepath.Join(t.TempDir(), "fixture.jsonl")
		if err := os.WriteFile(p, []byte(text), 0600); err != nil {
			t.Fatal(err)
		}
		if agent == "claude" {
			c, err := (claude.Adapter{}).CompletionContext(p)
			if err != nil || c.LastReply != "alpha\nreply" || len(c.Corpus) != 2 || c.Corpus[0] != "newest" {
				t.Fatalf("%+v %v", c, err)
			}
		} else {
			c, err := (codex.Adapter{}).CompletionContext(p)
			if err != nil || c.LastReply != "alpha\nreply" || len(c.Corpus) != 2 || c.Corpus[0] != "newest" {
				t.Fatalf("%+v %v", c, err)
			}
		}
	}
}
