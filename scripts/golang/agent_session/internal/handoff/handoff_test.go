package handoff

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// A subprocess speaks the real JSON-RPC wire protocol. It only touches this
// test's temporary directory and never executes an injected historical tool.
func TestFakeServer(t *testing.T) {
	if os.Getenv("HANDOFF_FAKE_CHILD") != "1" {
		return
	}
	scenario := os.Getenv("HANDOFF_FAKE_SCENARIO")
	dir := os.Getenv("HANDOFF_FAKE_DIR")
	dec := json.NewDecoder(os.Stdin)
	enc := json.NewEncoder(os.Stdout)
	notify := func(method string, params any) { _ = enc.Encode(map[string]any{"method": method, "params": params}) }
	logfile, _ := os.OpenFile(filepath.Join(dir, "calls.jsonl"), os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0600)
	defer logfile.Close()
	for {
		var req struct {
			ID     json.RawMessage
			Method string
			Params json.RawMessage
		}
		if err := dec.Decode(&req); err != nil {
			os.Exit(0)
		}
		_ = json.NewEncoder(logfile).Encode(req)
		result := any(map[string]any{})
		reply := func() { _ = enc.Encode(map[string]any{"id": req.ID, "result": result}) }
		switch req.Method {
		case "initialized":
			continue
		case "initialize":
			if scenario == "timeout-child" {
				child := exec.Command("/bin/sleep", "3600")
				if err := child.Start(); err != nil {
					fmt.Fprintln(os.Stderr, err)
					os.Exit(4)
				}
				_ = os.WriteFile(filepath.Join(dir, "child.pid"), []byte(fmt.Sprint(child.Process.Pid)), 0600)
				time.Sleep(time.Hour)
			}
			if scenario == "timeout" {
				time.Sleep(time.Hour)
			}
			if scenario == "exit" {
				os.Exit(7)
			}
			if scenario == "bad-response-id" {
				_ = enc.Encode(map[string]any{"id": 999, "result": result})
				continue
			}
		case "config/read":
			model := "gpt-6.1-sol"
			if scenario == "unsupported-model" {
				model = "gpt-5.4"
			}
			if scenario == "default-model" {
				model = ""
			}
			result = map[string]any{"config": map[string]any{"model": model}}
		case "model/list":
			result = map[string]any{"data": []any{map[string]any{"model": "gpt-6.1-sol", "isDefault": true}}}
		case "externalAgentConfig/import":
			if scenario == "server-request" {
				_ = enc.Encode(map[string]any{"id": "approval-1", "method": "item/commandExecution/requestApproval", "params": map[string]any{"command": "printf inert"}})
				var denial struct{ Error *rpcError }
				_ = dec.Decode(&denial)
				if denial.Error == nil {
					fmt.Fprintln(os.Stderr, "server request was not denied")
					os.Exit(4)
				}
				os.Exit(0)
			}
			target := "11111111-1111-4111-8111-111111111111"
			source := filepath.Join(dir, "source.jsonl")
			if scenario == "target-path" {
				target = filepath.Join(dir, "target.jsonl")
			}
			if scenario == "remote-target" {
				target = "https://example.invalid/thread/remote"
			}
			if scenario == "wrong-source" {
				source = filepath.Join(dir, "other.jsonl")
			}
			completed := func(importID string) {
				successes := []any{map[string]any{"itemType": "SESSIONS", "source": source, "target": target}}
				failures := []any{}
				if scenario == "import-failure" {
					successes = nil
					failures = []any{map[string]any{"source": source, "message": "synthetic import failure"}}
				}
				notify("externalAgentConfig/import/completed", map[string]any{"importId": importID, "itemTypeResults": []any{map[string]any{"itemType": "SESSIONS", "successes": successes, "failures": failures}}})
			}
			completed("unrelated-import")
			completed("import-exact")
			result = map[string]any{"importId": "import-exact"}
		case "thread/start":
			result = map[string]any{"model": "gpt-6.1-sol", "thread": map[string]any{"id": "11111111-1111-4111-8111-111111111111"}}
		case "thread/inject_items":
			_ = os.WriteFile(filepath.Join(dir, "injected.json"), req.Params, 0600)
			if scenario == "inject-error" {
				_ = enc.Encode(map[string]any{"id": req.ID, "error": rpcError{123, "synthetic injection error"}})
				continue
			}
		case "thread/compact/start":
			notify("turn/started", map[string]any{"threadId": "other-thread", "turn": map[string]any{"id": "unrelated-turn"}})
			notify("turn/started", map[string]any{"threadId": "11111111-1111-4111-8111-111111111111", "turn": map[string]any{"id": "compact-turn"}})
			if scenario != "missing-item" {
				notify("item/completed", map[string]any{"threadId": "11111111-1111-4111-8111-111111111111", "turnId": "compact-turn", "item": map[string]any{"type": "contextCompaction", "id": "compact-item"}})
			}
			status := "completed"
			if scenario == "failed-turn" {
				status = "failed"
			}
			turnID := "compact-turn"
			if scenario == "wrong-turn-id" {
				turnID = "wrong-turn"
			}
			notify("turn/completed", map[string]any{"threadId": "11111111-1111-4111-8111-111111111111", "turn": map[string]any{"id": turnID, "status": status, "error": nil}})
			if scenario != "not-persisted" {
				_ = os.WriteFile(filepath.Join(dir, "persisted"), []byte("contextCompaction"), 0600)
			}
		case "thread/read":
			id := "11111111-1111-4111-8111-111111111111"
			if scenario == "wrong-read-id" {
				id = "wrong-thread"
			}
			result = map[string]any{"thread": map[string]any{"id": id}}
		case "thread/resume":
			_, err := os.Stat(filepath.Join(dir, "persisted"))
			items := []any{}
			if err == nil {
				items = append(items, map[string]any{"type": "contextCompaction"})
			}
			result = map[string]any{"thread": map[string]any{"id": "11111111-1111-4111-8111-111111111111", "turns": []any{map[string]any{"items": items}}}}
		default:
			fmt.Fprintf(os.Stderr, "unexpected method %s\n", req.Method)
			os.Exit(4)
		}
		reply()
	}
}

func fixture(t *testing.T, scenario string) Options {
	t.Helper()
	dir := t.TempDir()
	binary, err := os.Executable()
	if err != nil {
		t.Fatal(err)
	}
	wrapper := filepath.Join(dir, "codex-fake")
	quote := func(s string) string { return "'" + strings.ReplaceAll(s, "'", "'\\''") + "'" }
	if err = os.WriteFile(wrapper, []byte("#!/bin/sh\nexec "+quote(binary)+" -test.run '^TestFakeServer$' -- \"$@\"\n"), 0700); err != nil {
		t.Fatal(err)
	}
	t.Setenv("HANDOFF_FAKE_CHILD", "1")
	t.Setenv("HANDOFF_FAKE_SCENARIO", scenario)
	t.Setenv("HANDOFF_FAKE_DIR", dir)
	source := filepath.Join(dir, "source.jsonl")
	content := `{"type":"user","message":{"content":"first request"}}
{"type":"assistant","message":{"content":[{"type":"text","text":"first answer"},{"type":"tool_use","id":"tool-1","name":"Bash","input":{"command":"printf inert"}}]}}
{"type":"user","message":{"content":[{"type":"tool_result","tool_use_id":"tool-1","content":"historical result"}]}}
{"type":"system","subtype":"compact_boundary","compactMetadata":{"trigger":"manual","preTokens":100,"postTokens":10}}
{"type":"user","isCompactSummary":true,"message":{"content":"summary of earlier history"}}
{"type":"user","isMeta":true,"message":{"content":"harness secret instruction"}}
{"type":"user","message":{"content":"# AGENTS.md instructions for /example\n<INSTRUCTIONS>source instructions</INSTRUCTIONS>\n<environment_context>old environment</environment_context>\nlast request"}}
{"type":"assistant","message":{"content":"last answer"}}
`
	if err = os.WriteFile(source, []byte(content), 0600); err != nil {
		t.Fatal(err)
	}
	if err = os.WriteFile(filepath.Join(dir, "target.jsonl"), []byte("{\"type\":\"session_meta\",\"payload\":{\"id\":\"11111111-1111-4111-8111-111111111111\"}}\n"), 0600); err != nil {
		t.Fatal(err)
	}
	return Options{Mode: "native", Cwd: dir, Transcript: source, Codex: wrapper, Timeout: 5 * time.Second, Progress: io.Discard}
}
func TestNativeExactMappingAndEarlyNotifications(t *testing.T) {
	for _, scenario := range []string{"success", "target-path"} {
		t.Run(scenario, func(t *testing.T) {
			o := fixture(t, scenario)
			before, _ := os.ReadFile(o.Transcript)
			id, err := Run(context.Background(), o)
			if err != nil {
				t.Fatal(err)
			}
			if id != "11111111-1111-4111-8111-111111111111" {
				t.Fatalf("id %q", id)
			}
			after, _ := os.ReadFile(o.Transcript)
			if !bytes.Equal(before, after) {
				t.Fatal("source mutated")
			}
			calls, _ := os.ReadFile(filepath.Join(o.Cwd, "calls.jsonl"))
			var imported struct {
				Params struct {
					Items []struct {
						Type string `json:"itemType"`
					} `json:"migrationItems"`
				}
			}
			for _, line := range bytes.Split(calls, []byte("\n")) {
				if bytes.Contains(line, []byte("externalAgentConfig/import")) {
					if err = json.Unmarshal(line, &imported); err != nil {
						t.Fatal(err)
					}
				}
			}
			if len(imported.Params.Items) != 1 || imported.Params.Items[0].Type != "SESSIONS" {
				t.Fatalf("non session-only import: %s", calls)
			}
		})
	}
}
func TestFailuresAndTimeoutsFailClosed(t *testing.T) {
	for _, tc := range []struct{ scenario, mode, want string }{
		{"bad-response-id", "native", "unexpected response id"}, {"exit", "native", "output ended"}, {"timeout", "native", "deadline exceeded"},
		{"wrong-source", "native", "exactly one success"}, {"remote-target", "native", "unsupported target"}, {"import-failure", "native", "synthetic import failure"},
		{"wrong-read-id", "native", "exact destination"}, {"server-request", "native", "request"},
		{"unsupported-model", "compact", "supported 1.05m model"}, {"inject-error", "compact", "recover with codex resume 11111111-1111-4111-8111-111111111111"},
		{"failed-turn", "compact", "ended failed"}, {"missing-item", "compact", "deadline exceeded"}, {"wrong-turn-id", "compact", "deadline exceeded"},
		{"not-persisted", "compact", "no persisted contextCompaction"},
	} {
		t.Run(tc.scenario, func(t *testing.T) {
			o := fixture(t, tc.scenario)
			o.Mode = tc.mode
			if strings.Contains(tc.want, "deadline") {
				o.Timeout = 150 * time.Millisecond
			}
			id, err := Run(context.Background(), o)
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("id=%q err=%v, want %s", id, err, tc.want)
			}
			if id != "" {
				t.Fatalf("failed handoff returned success id %q", id)
			}
		})
	}
}
func TestCompactFullHistoryAndPersistentResume(t *testing.T) {
	for _, scenario := range []string{"success", "default-model"} {
		t.Run(scenario, func(t *testing.T) {
			o := fixture(t, scenario)
			o.Mode = "compact"
			o.Args = []string{"--model", "gpt-6.1-sol", "--config", "model_reasoning_effort=\"high\""}
			id, err := Run(context.Background(), o)
			if err != nil {
				t.Fatal(err)
			}
			if id != "11111111-1111-4111-8111-111111111111" {
				t.Fatal(id)
			}
			b, err := os.ReadFile(filepath.Join(o.Cwd, "injected.json"))
			if err != nil {
				t.Fatal(err)
			}
			var injected struct{ Items []historyItem }
			if err = json.Unmarshal(b, &injected); err != nil {
				t.Fatal(err)
			}
			var text strings.Builder
			for _, item := range injected.Items {
				if item.Role != "assistant" || item.Type != "message" {
					t.Fatalf("executable or retained user history: %+v", item)
				}
				for _, c := range item.Content {
					text.WriteString(c.Text)
					text.WriteString("\n")
				}
			}
			content := text.String()
			previous := -1
			for _, needle := range []string{"first request", "first answer", "printf inert", "historical result", "Context compacted", "summary of earlier history", "last request", "last answer"} {
				pos := strings.Index(content, needle)
				if pos <= previous {
					t.Fatalf("missing/out of order %q in %s", needle, content)
				}
				previous = pos
			}
			for _, needle := range []string{"harness secret instruction", "source instructions", "old environment", "# AGENTS.md"} {
				if strings.Contains(content, needle) {
					t.Fatalf("source scaffold retained: %q", needle)
				}
			}
			calls, _ := os.ReadFile(filepath.Join(o.Cwd, "calls.jsonl"))
			if bytes.Contains(calls, []byte(`"Method":"turn/start"`)) {
				t.Fatal("ordinary model turn started")
			}
			if bytes.Count(calls, []byte(`"Method":"initialize"`)) != 2 || !bytes.Contains(calls, []byte(`"Method":"thread/resume"`)) {
				t.Fatalf("no fresh persistence verification: %s", calls)
			}
			if !bytes.Contains(calls, []byte(`"model_context_window":1050000`)) {
				t.Fatalf("missing context window: %s", calls)
			}
		})
	}
}
func TestStrictHistoryRejectsCorruption(t *testing.T) {
	for _, input := range []string{"{", "null", `{"type":"user"}`, `{"type":"user","message":{"content":{}}}`, `{"type":"user","message":{"content":["invalid"]}}`} {
		p := filepath.Join(t.TempDir(), "corrupt.jsonl")
		_ = os.WriteFile(p, []byte(input), 0600)
		if _, err := history(p); err == nil {
			t.Fatalf("accepted corrupt input %s", input)
		}
	}
}
func TestConfigArguments(t *testing.T) {
	got, err := ConfigArguments([]string{"--model=gpt-6.1-sol", "--config", "key=\"literal $(printf inert)\""})
	if err != nil {
		t.Fatal(err)
	}
	want := []string{"--config", "model=\"gpt-6.1-sol\"", "--config", "key=\"literal $(printf inert)\""}
	if fmt.Sprint(got) != fmt.Sprint(want) {
		t.Fatalf("%v != %v", got, want)
	}
	for _, argv := range [][]string{{"--profile", "work"}, {"--model"}, {"--config="}, {"resume", "id"}} {
		if _, err = ConfigArguments(argv); err == nil {
			t.Fatalf("accepted %v", argv)
		}
	}
}

func TestCLIStdoutContract(t *testing.T) {
	binary := filepath.Join(t.TempDir(), "agent_session")
	build := exec.Command("go", "build", "-o", binary, ".")
	build.Dir = "../.."
	if out, err := build.CombinedOutput(); err != nil {
		t.Fatalf("build CLI: %v: %s", err, out)
	}
	for _, scenario := range []string{"success", "import-failure"} {
		t.Run(scenario, func(t *testing.T) {
			o := fixture(t, scenario)
			cmd := exec.Command(binary, "claude", "handoff", "-mode", "native", "-cwd", o.Cwd, "-timeout", "2s", "-codex", o.Codex, o.Transcript, "--", "--model", "gpt-6.1-sol")
			var out, stderr bytes.Buffer
			cmd.Stdout = &out
			cmd.Stderr = &stderr
			err := cmd.Run()
			if scenario == "success" {
				if err != nil || out.String() != "11111111-1111-4111-8111-111111111111\n" {
					t.Fatalf("err=%v stdout=%q stderr=%s", err, out.String(), stderr.String())
				}
			} else if err == nil || out.Len() != 0 {
				t.Fatalf("failed CLI leaked success output: err=%v stdout=%q", err, out.String())
			}
		})
	}
}
