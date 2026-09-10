package agy

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"agent_session/internal/proc"
	"agent_session/internal/session"
	"agent_session/internal/turns"
)

// The adapter under test; a composite literal in an `if` init would need
// parentheses.
var ad = Adapter{}

const (
	mainID = "aaaaaaaa-1111-4111-8111-aaaaaaaaaaaa"
	subID  = "bbbbbbbb-2222-4222-8222-bbbbbbbbbbbb"
	bareID = "cccccccc-3333-4333-8333-cccccccccccc"
)

// A synthetic state directory, written from the documented step shape and the
// key names of the JSON mirror. No real conversation is read by these tests.
func writeStore(t *testing.T) (home, brain, main string) {
	t.Helper()
	home = t.TempDir()
	brain = filepath.Join(home, "brain")

	logs := func(id string) string {
		d := filepath.Join(brain, id, ".system_generated", "logs")
		if err := os.MkdirAll(d, 0o755); err != nil {
			t.Fatal(err)
		}
		return d
	}
	line := func(m map[string]any) string {
		b, err := json.Marshal(m)
		if err != nil {
			t.Fatal(err)
		}
		return string(b)
	}

	steps := []string{
		line(map[string]any{"step_index": 0, "source": "USER_EXPLICIT", "type": "USER_INPUT",
			"status": "DONE", "created_at": "2026-09-08T10:00:00Z",
			"content": "<USER_REQUEST>\nlist the files\n</USER_REQUEST>\nWorkspace: /tmp/proj"}),
		line(map[string]any{"step_index": 1, "source": "MODEL", "type": "PLANNER_RESPONSE",
			"status": "DONE", "created_at": "2026-09-08T10:00:05Z",
			"thinking": "They want a listing.", "content": "I'll look at the directory.",
			"tool_calls": []map[string]any{{"name": "list_dir", "args": map[string]any{"path": "/tmp/proj"}}}}),
		line(map[string]any{"step_index": 2, "source": "SYSTEM", "type": "LIST_DIRECTORY",
			"status": "DONE", "created_at": "2026-09-08T10:00:06Z", "content": "foo.go\nbar.go"}),
		line(map[string]any{"step_index": 3, "source": "MODEL", "type": "RUN_COMMAND",
			"status": "ERROR", "created_at": "2026-09-08T10:00:07Z", "content": "exit status 1",
			"tool_calls": []map[string]any{{"name": "run_command", "args": `{"command":"go test ./..."}`}}}),
		// A kind this code has never heard of still has to render.
		line(map[string]any{"step_index": 4, "source": "SYSTEM", "type": "SOME_FUTURE_KIND",
			"status": "RUNNING", "created_at": "2026-09-08T10:00:08Z", "content": "who knows"}),
		line(map[string]any{"step_index": 5, "source": "MODEL", "type": "INVOKE_SUBAGENT",
			"status": "DONE", "created_at": "2026-09-08T10:00:09Z",
			"content": "spawned conversation " + subID}),
		line(map[string]any{"step_index": 6, "source": "SYSTEM", "type": "CONVERSATION_HISTORY",
			"status": "DONE", "created_at": "2026-09-08T10:00:20Z", "content": "Earlier: listed and tested."}),
		line(map[string]any{"step_index": 7, "source": "USER_EXPLICIT", "type": "USER_INPUT",
			"status": "DONE", "created_at": "2026-09-08T10:01:00Z", "content": "thanks"}),
	}

	main = filepath.Join(logs(mainID), "transcript_full.jsonl")
	if err := os.WriteFile(main, []byte(strings.Join(steps, "\n")+"\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	// The compact twin, whose long fields were cut: it must not be preferred.
	compact := line(map[string]any{"step_index": 0, "source": "USER_EXPLICIT", "type": "USER_INPUT",
		"status": "DONE", "created_at": "2026-09-08T10:00:00Z", "content": "<USER_REQUEST>\nlist th",
		"truncated_fields": []string{"content"}})
	if err := os.WriteFile(filepath.Join(logs(mainID), "transcript.jsonl"), []byte(compact+"\n"), 0o600); err != nil {
		t.Fatal(err)
	}

	// The subagent's conversation, compact only.
	subSteps := []string{
		line(map[string]any{"step_index": 0, "source": "USER_EXPLICIT", "type": "USER_INPUT",
			"status": "DONE", "created_at": "2026-09-08T10:00:10Z", "content": "<USER_REQUEST>explore</USER_REQUEST>"}),
		line(map[string]any{"step_index": 1, "source": "MODEL", "type": "PLANNER_RESPONSE",
			"status": "DONE", "created_at": "2026-09-08T10:00:12Z", "content": "Two files."}),
	}
	if err := os.WriteFile(filepath.Join(logs(subID), "transcript.jsonl"), []byte(strings.Join(subSteps, "\n")+"\n"), 0o600); err != nil {
		t.Fatal(err)
	}

	// A brain directory that never wrote a transcript: not a session.
	if err := os.MkdirAll(filepath.Join(brain, bareID), 0o755); err != nil {
		t.Fatal(err)
	}

	meta := map[string]any{"conversations": map[string]any{
		mainID: map[string]any{
			"summary": map[string]any{"ID": mainID, "Title": "Listing the files", "Preview": "list the files",
				"NumSteps": 8, "AgentName": "Planner", "WorkspaceURIs": nil},
			"is_internal": false, "last_modified_time": "2026-09-08T10:01:00Z"},
		subID: map[string]any{
			"summary": map[string]any{"ID": subID, "Title": "", "Preview": "explore the repo",
				"AgentName": "Explorer", "WorkspaceURIs": []string{"file:///tmp/proj"}},
			"is_internal": true, "last_modified_time": "2026-09-08T10:00:12Z"},
	}}
	if err := os.MkdirAll(filepath.Join(home, "cache"), 0o755); err != nil {
		t.Fatal(err)
	}
	raw, _ := json.Marshal(meta)
	if err := os.WriteFile(filepath.Join(home, "cache", "conversation_metadata.json"), raw, 0o600); err != nil {
		t.Fatal(err)
	}
	last, _ := json.Marshal(map[string]string{"/tmp/proj": mainID})
	if err := os.WriteFile(filepath.Join(home, "cache", "last_conversations.json"), last, 0o600); err != nil {
		t.Fatal(err)
	}

	history := strings.Join([]string{
		`{"conversationId":"` + mainID + `","display":"list the files","timestamp":1788252000000,"workspace":"/tmp/other"}`,
		`{"conversationId":"` + mainID + `","display":"thanks","timestamp":1788252060000,"workspace":"/tmp/proj"}`,
	}, "\n") + "\n"
	if err := os.WriteFile(filepath.Join(home, "history.jsonl"), []byte(history), 0o600); err != nil {
		t.Fatal(err)
	}

	// The caches are per state directory and each test gets a fresh one, but
	// the maps live for the process.
	t.Cleanup(func() {
		metaMu.Lock()
		delete(metaCache, home)
		metaMu.Unlock()
		cwdMu.Lock()
		delete(cwdCache, home)
		cwdMu.Unlock()
	})
	return home, brain, main
}

func TestUserText(t *testing.T) {
	for _, c := range []struct{ in, want string }{
		{"<USER_REQUEST>\nhi\n</USER_REQUEST>", "hi"},
		{"<USER_REQUEST>hi</USER_REQUEST>\nWorkspace: /tmp", "hi"},
		{"<USER_REQUEST>\nunclosed", "unclosed"},
		{"  plain  ", "plain"},
		{"", ""},
	} {
		if got := userText(c.in); got != c.want {
			t.Errorf("userText(%q) = %q, want %q", c.in, got, c.want)
		}
	}
}

func TestTypeLabel(t *testing.T) {
	for _, c := range []struct{ in, want string }{
		{"RUN_COMMAND", "Run command"},
		{"USER_INPUT", "User input"},
		{"SOME_FUTURE_KIND", "Some future kind"},
		{"GENERIC", "Generic"},
		{"", "Step"},
	} {
		if got := typeLabel(c.in); got != c.want {
			t.Errorf("typeLabel(%q) = %q, want %q", c.in, got, c.want)
		}
	}
}

func TestCallArgsDecodesBothShapes(t *testing.T) {
	obj := callArgs(json.RawMessage(`{"path":"/x"}`))
	if string(obj) != `{"path":"/x"}` {
		t.Errorf("object args = %s", obj)
	}
	str := callArgs(json.RawMessage(`"{\"path\":\"/x\"}"`))
	if string(str) != `{"path":"/x"}` {
		t.Errorf("string-encoded args = %s", str)
	}
	other := callArgs(json.RawMessage(`"just a string"`))
	var m map[string]string
	if err := json.Unmarshal(other, &m); err != nil || m["args"] != "just a string" {
		t.Errorf("scalar args = %s (%v)", other, err)
	}
	if callArgs(nil) != nil {
		t.Error("empty args should be nil")
	}
}

func TestListSkipsTranscriptlessAndPrefersFull(t *testing.T) {
	home, brain, main := writeStore(t)

	infos, err := ad.List([]string{brain}, session.ListOpts{SnippetLen: 120, NameLen: 40, Jobs: 2})
	if err != nil {
		t.Fatal(err)
	}
	// The subagent's conversation is a conversation too: it has a transcript.
	// The bare directory is not.
	if len(infos) != 2 {
		t.Fatalf("want two rows, got %d: %+v", len(infos), infos)
	}

	var got session.Info
	for _, i := range infos {
		if i.Path == main {
			got = i
		}
		if strings.Contains(i.Path, bareID) {
			t.Error("a brain directory with no transcript must not be listed")
		}
	}
	if got.Path == "" {
		t.Fatalf("the full transcript was not listed: %+v", infos)
	}
	if got.Name != "Listing the files" {
		t.Errorf("name = %q, want the title", got.Name)
	}
	if got.Snippet != "list the files" {
		t.Errorf("snippet = %q, want the unwrapped request", got.Snippet)
	}
	if got.Rel != filepath.Join(mainID, ".system_generated", "logs", "transcript_full.jsonl") {
		t.Errorf("rel = %q", got.Rel)
	}
	if got.Epoch == 0 || got.Stamp == "" {
		t.Errorf("no time: %+v", got)
	}

	// The name falls back to the preview when there is no title.
	for _, i := range infos {
		if strings.Contains(i.Path, subID) && i.Name != "explore the repo" {
			t.Errorf("subagent name = %q, want the preview", i.Name)
		}
	}

	// -cwd matches the last workspace history recorded, not the first.
	scoped, err := ad.List([]string{brain}, session.ListOpts{Cwd: "/tmp/proj", SnippetLen: 1, NameLen: 1, Jobs: 1})
	if err != nil {
		t.Fatal(err)
	}
	if len(scoped) != 2 {
		// main by history, sub by its workspace URI.
		t.Errorf("-cwd /tmp/proj matched %d rows: %+v", len(scoped), scoped)
	}
	if _, err := ad.List([]string{brain}, session.ListOpts{Cwd: "/tmp/other", SnippetLen: 1, NameLen: 1, Jobs: 1}); err == nil {
		t.Error("-cwd on a stale workspace should match nothing")
	}
	_ = home
}

// `-last-by user` dates a conversation by the last step the user is the
// source of. Everything after it is the agent working on its own, which is
// exactly what a picker sorted by conversation must not be moved by.
func TestListLastByUser(t *testing.T) {
	_, brain, main := writeStore(t)

	// The fixture ends on the user's "thanks"; give it a tail of the agent
	// still working, so the two modes differ.
	step, err := json.Marshal(map[string]any{"step_index": 8, "source": "MODEL", "type": "RUN_COMMAND",
		"status": "DONE", "created_at": "2026-09-08T10:02:00Z", "content": "ok"})
	if err != nil {
		t.Fatal(err)
	}
	fh, err := os.OpenFile(main, os.O_APPEND|os.O_WRONLY, 0o600)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := fh.Write(append(step, '\n')); err != nil {
		t.Fatal(err)
	}
	fh.Close()

	stamp := func(o session.ListOpts) string {
		o.SnippetLen, o.NameLen, o.Jobs = 120, 40, 1
		infos, err := ad.List([]string{brain}, o)
		if err != nil {
			t.Fatal(err)
		}
		for _, i := range infos {
			if i.Path == main {
				return time.Unix(i.Epoch, 0).UTC().Format(time.RFC3339)
			}
		}
		t.Fatalf("the main conversation is missing from %+v", infos)
		return ""
	}

	if got := stamp(session.ListOpts{}); got != "2026-09-08T10:02:00Z" {
		t.Errorf("-last-by any: got %q, want the newest step", got)
	}
	if got := stamp(session.ListOpts{LastBy: session.LastByUser}); got != "2026-09-08T10:01:00Z" {
		t.Errorf("-last-by user: got %q, want the last user step", got)
	}
}

func TestNameAndMeta(t *testing.T) {
	_, _, main := writeStore(t)

	if n, err := ad.Name(main); err != nil || n != "Listing the files" {
		t.Errorf("Name = %q (%v)", n, err)
	}
	m, err := ad.Meta(main)
	if err != nil {
		t.Fatal(err)
	}
	if m.ID != mainID || m.Name != "Listing the files" || m.Cwd != "/tmp/proj" {
		t.Errorf("Meta = %+v", m)
	}
	if _, err := ad.Name(filepath.Join(t.TempDir(), "nope.jsonl")); err == nil {
		t.Error("a missing file should fail")
	}
}

func TestDocumentTurns(t *testing.T) {
	_, _, main := writeStore(t)

	doc, err := ad.Document(main, session.DocOpts{Subagents: true})
	if err != nil {
		t.Fatal(err)
	}

	var shape []string
	for _, tu := range doc.Turns {
		if tu.Heading != "" {
			shape = append(shape, tu.Heading)
			continue
		}
		shape = append(shape, tu.Role)
	}
	want := "user,assistant,Conversation history,user"
	if strings.Join(shape, ",") != want {
		t.Errorf("turns = %v, want %v", shape, want)
	}

	if doc.Turns[0].Blocks[0].B.Text != "list the files" {
		t.Errorf("the request wrapper survived: %q", doc.Turns[0].Blocks[0].B.Text)
	}

	var types []string
	for _, b := range doc.Turns[1].Blocks {
		types = append(types, b.B.Type)
	}
	// thinking, the planner's prose, its tool call, the directory listing as a
	// verbatim event, the failed command's event and call, the unknown kind,
	// and the subagent step, whose description is prose.
	if strings.Join(types, ",") != "thinking,text,tool_use,event,event,tool_use,event,notice" {
		t.Errorf("assistant blocks = %v", types)
	}

	out, err := turns.Render(doc, turns.Options{Format: "org", Jobs: 1, Diff: true})
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{
		"** Tool Use: list_dir",
		"** List directory",
		"** Run command · error",
		"** Some future kind · running",
		"* Conversation history",
		"* Subagents",
		"** Explorer · explore the repo",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("rendered document lacks %q:\n%s", want, out)
		}
	}
	// The unknown kind's body is kept verbatim, not read as markdown.
	if !strings.Contains(out, "#+begin_example\nwho knows\n#+end_example") {
		t.Errorf("an event body should be a block:\n%s", out)
	}
}

func TestPreview(t *testing.T) {
	_, _, main := writeStore(t)

	out, err := ad.Preview(main, session.PreviewOpts{Bytes: 400 << 10, Color: false})
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{"Listing the files", mainID, "Planner", "8 steps", "/tmp/proj", "thanks"} {
		if !strings.Contains(out, want) {
			t.Errorf("preview lacks %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "\x1b[") {
		t.Error("colour off still emitted escapes")
	}
	if _, err := ad.Preview(filepath.Join(t.TempDir(), "nope.jsonl"), session.PreviewOpts{}); err == nil {
		t.Error("a missing file should fail")
	}
}

func TestIsAgyCmd(t *testing.T) {
	for _, c := range []struct {
		in   string
		want bool
	}{
		{"agy", true},
		{"/opt/homebrew/bin/agy --conversation x", true},
		{"agy remote-control", false},
		{"agy --version", false},
		{"legacy --x", false},
		{"vim agy.md", false},
	} {
		if got := isAgyCmd(c.in); got != c.want {
			t.Errorf("isAgyCmd(%q) = %v", c.in, got)
		}
	}
}

func TestLivePairsByWorkspace(t *testing.T) {
	_, brain, main := writeStore(t)

	proc.ResetShared()
	defer proc.ResetShared()
	oldRun := proc.Run
	proc.Run = func(name string, args ...string) ([]byte, error) {
		switch name {
		case "ps":
			return []byte("" +
				"  100     1 tmux\n" +
				"  200   100 -zsh\n" +
				"  300   200 /opt/homebrew/bin/agy\n" +
				"  400     1 agy remote-control\n" +
				"  500     1 /opt/homebrew/bin/agy\n"), nil
		case "lsof":
			// 300 sits in the workspace; 500 is somewhere with no conversation.
			return []byte("p300\nn/tmp/proj\np500\nn/tmp/elsewhere\n"), nil
		case "tmux":
			return []byte("200\tagy-work\n"), nil
		}
		return nil, nil
	}
	defer func() { proc.Run = oldRun }()

	rows, err := ad.Live([]string{brain})
	if err != nil {
		t.Fatal(err)
	}
	if len(rows) != 1 {
		t.Fatalf("want one row, got %+v", rows)
	}
	r := rows[0]
	if r.PID != 300 || r.ID != mainID || r.Transcript != main || r.Cwd != "/tmp/proj" ||
		r.Tmux != "agy-work" || r.Name != "Listing the files" || r.Status != "running" {
		t.Errorf("row = %+v", r)
	}
	if strings.Count(r.Row(), "\t") != 6 {
		t.Errorf("row has the wrong shape: %q", r.Row())
	}
}

func TestLiveWithNoProcesses(t *testing.T) {
	_, brain, _ := writeStore(t)

	proc.ResetShared()
	defer proc.ResetShared()
	oldRun := proc.Run
	proc.Run = func(name string, args ...string) ([]byte, error) {
		if name == "ps" {
			return []byte("  100     1 /bin/zsh\n"), nil
		}
		return nil, nil
	}
	defer func() { proc.Run = oldRun }()

	rows, err := ad.Live([]string{brain})
	if err != nil || len(rows) != 0 {
		t.Errorf("want no rows and no error, got %+v (%v)", rows, err)
	}
}

func TestProseStep(t *testing.T) {
	plain := "total 8\ndrwxr-xr-x  2 evar staff"
	md := "### Key factors\n\n- one\n- two"

	for _, c := range []struct {
		typ, body string
		want      bool
	}{
		// A known prose kind, whatever its body looks like.
		{"SEARCH_WEB", plain, true},
		{"PLANNER_RESPONSE", plain, true},
		{"CONVERSATION_HISTORY", plain, true},
		// A known output kind, even when the output contains markdown.
		{"RUN_COMMAND", md, false},
		{"VIEW_FILE", md, false},
		{"GREP_SEARCH", md, false},
		// An unknown kind is judged by its body, since the enum keeps growing.
		{"SOME_FUTURE_KIND", md, true},
		{"SOME_FUTURE_KIND", plain, false},
		{"SOME_FUTURE_KIND", "see [docs](https://example.com)", true},
		{"SOME_FUTURE_KIND", "a **bold** claim", true},
		{"SOME_FUTURE_KIND", "", false},
		// `#include` is not a heading, and `-fPIC` is not a bullet.
		{"SOME_FUTURE_KIND", "#include <stdio.h>\n-fPIC", false},
	} {
		if got := proseStep(c.typ, c.body); got != c.want {
			t.Errorf("proseStep(%q, %q) = %v, want %v", c.typ, c.body, got, c.want)
		}
	}
}
