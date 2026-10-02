package codex

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent_session/internal/turns"
)

func TestHandoffExportStrictVisibleHistory(t *testing.T) {
	dir := filepath.Join(t.TempDir(), "sessions", "2026", "10", "02")
	if err := os.MkdirAll(dir, 0700); err != nil {
		t.Fatal(err)
	}
	source := filepath.Join(dir, "rollout-parent.jsonl")
	contents := `{"type":"session_meta","payload":{"id":"parent-exact"}}
{"type":"response_item","payload":{"type":"message","role":"user","content":[{"type":"input_text","text":"<INSTRUCTIONS>old scaffold</INSTRUCTIONS>\nrequest"}]}}
{"type":"response_item","payload":{"type":"reasoning","summary":[{"type":"summary_text","text":"visible summary"}],"content":[{"type":"text","text":"hidden raw reasoning"}],"encrypted_content":"hidden encrypted reasoning"}}
{"type":"response_item","payload":{"type":"message","role":"assistant","content":[{"type":"output_text","text":"visible answer"}]}}
`
	// Match Codex's actual line-based instruction wrapper.
	contents = strings.Replace(contents, "<INSTRUCTIONS>old scaffold</INSTRUCTIONS>", "<INSTRUCTIONS>\\nold scaffold\\n</INSTRUCTIONS>", 1)
	if err := os.WriteFile(source, []byte(contents), 0600); err != nil {
		t.Fatal(err)
	}
	child := filepath.Join(dir, "rollout-child.jsonl")
	if err := os.WriteFile(child, []byte(`{"type":"session_meta","payload":{"id":"child-exact","parent_thread_id":"parent-exact"}}
{"type":"response_item","payload":{"type":"message","role":"assistant","content":[{"type":"output_text","text":"child history"}]}}
`), 0600); err != nil {
		t.Fatal(err)
	}
	doc, err := HandoffDocument(source)
	if err != nil {
		t.Fatal(err)
	}
	rendered, err := turns.Render(doc, turns.Options{Format: "md"})
	if err != nil {
		t.Fatal(err)
	}
	for _, needle := range []string{"request", "visible summary", "visible answer", "child history"} {
		if !strings.Contains(rendered, needle) {
			t.Fatalf("missing %q: %s", needle, rendered)
		}
	}
	for _, needle := range []string{"old scaffold", "hidden raw reasoning", "hidden encrypted reasoning"} {
		if strings.Contains(rendered, needle) {
			t.Fatalf("leaked %q", needle)
		}
	}
	if err = os.WriteFile(child, []byte("{"), 0600); err != nil {
		t.Fatal(err)
	}
	// Child discovery requires readable session metadata; a corrupt later record
	// must fail, rather than produce a partial child's transcript.
	if err = os.WriteFile(child, []byte("{\"type\":\"session_meta\",\"payload\":{\"id\":\"child-exact\",\"parent_thread_id\":\"parent-exact\"}}\n{"), 0600); err != nil {
		t.Fatal(err)
	}
	if _, err = HandoffDocument(source); err == nil {
		t.Fatal("accepted corrupt child")
	}
	if err = os.WriteFile(source, []byte(contents+"{"), 0600); err != nil {
		t.Fatal(err)
	}
	if _, err = HandoffDocument(source); err == nil {
		t.Fatal("accepted truncated source")
	}
}
