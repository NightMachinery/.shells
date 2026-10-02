package handoff

import (
	"bytes"
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestValidateCompactResult(t *testing.T) {
	result := `{"type":"result","subtype":"success","session_id":"source-exact","is_error":false,"result":"done"}`
	boundary := `{"type":"system","subtype":"compact_boundary","session_id":"source-exact"}`
	for _, tc := range []struct {
		name, stream string
		ok, noop     bool
	}{
		{"compacted", boundary + "\n" + result, true, false},
		{"documented-noop", strings.Replace(result, "done", "Not enough messages to compact.", 1), true, true},
		{"no-boundary", result, false, false},
		{"cli-success-compaction-error", strings.Replace(result, "done", "Error during compaction: session limit", 1), false, false},
		{"boundary-only", boundary, false, false},
		{"wrong-session", boundary + "\n" + strings.Replace(result, "source-exact", "other", 1), false, false},
		{"wrong-init-session", `{"type":"system","subtype":"init","session_id":"other"}` + "\n" + boundary + "\n" + result, false, false},
		{"missing-result-id", boundary + "\n" + strings.Replace(result, `"session_id":"source-exact",`, "", 1), false, false},
		{"failed-result", boundary + "\n" + strings.Replace(result, `"is_error":false`, `"is_error":true`, 1), false, false},
		{"malformed", boundary + "\n{", false, false},
		{"trailing-record", boundary + "\n" + result + "\n" + boundary, false, false},
		{"speculative-noop", strings.Replace(result, "done", "There is not enough conversation history to compact.", 1), false, false},
	} {
		t.Run(tc.name, func(t *testing.T) {
			path := filepath.Join(t.TempDir(), "compact.jsonl")
			if err := os.WriteFile(path, []byte(tc.stream), 0600); err != nil {
				t.Fatal(err)
			}
			var progress bytes.Buffer
			err := ValidateCompactResult(path, "source-exact", &progress)
			if (err == nil) != tc.ok {
				t.Fatalf("err=%v want success=%t", err, tc.ok)
			}
			if tc.name == "cli-success-compaction-error" && !strings.Contains(err.Error(), "Error during compaction: session limit") {
				t.Fatal("compaction failure reason was lost")
			}
			if tc.noop && !strings.Contains(progress.String(), "insufficient conversation history") {
				t.Fatal("noop not reported")
			}
		})
	}
}
func TestClientCancellationReapsChild(t *testing.T) {
	o := fixture(t, "timeout")
	ctx, cancel := context.WithTimeout(context.Background(), 100*time.Millisecond)
	defer cancel()
	c, err := start(ctx, o.Codex, nil, nil)
	if err != nil {
		t.Fatal(err)
	}
	err = c.call("initialize", map[string]any{}, nil)
	if err == nil {
		t.Fatal("hung child succeeded")
	}
	c.close()
	if c.cmd.ProcessState == nil {
		t.Fatal("child was not waited/reaped")
	}
	c.close() // lifecycle cleanup is idempotent
}
