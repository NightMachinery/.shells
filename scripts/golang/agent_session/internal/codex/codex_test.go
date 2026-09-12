package codex

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

// The adapter under test; a composite literal in an `if` init would need parentheses.
var ad = Adapter{}

// A synthetic store: `<home>/sessions/2026/09/08/rollout-...jsonl` plus the
// index and a lock, written from the documented record shapes. No real
// transcript is ever read by these tests.
func writeStore(t *testing.T) (home string, main string, sub string) {
	t.Helper()
	home = t.TempDir()
	day := filepath.Join(home, "sessions", "2026", "09", "08")
	if err := os.MkdirAll(day, 0o755); err != nil {
		t.Fatal(err)
	}

	mainID := "11111111-1111-4111-8111-111111111111"
	subID := "22222222-2222-4222-8222-222222222222"
	main = filepath.Join(day, "rollout-2026-09-08T10-00-00-"+mainID+".jsonl")
	sub = filepath.Join(day, "rollout-2026-09-08T10-05-00-"+subID+".jsonl")

	rec := func(ts, typ string, payload any) string {
		raw, _ := json.Marshal(payload)
		b, _ := json.Marshal(map[string]any{"timestamp": ts, "type": typ, "payload": json.RawMessage(raw)})
		return string(b)
	}
	msg := func(role, partType, text string) map[string]any {
		return map[string]any{"type": "message", "role": role,
			"content": []map[string]any{{"type": partType, "text": text}}}
	}

	lines := []string{
		rec("2026-09-08T10:00:00.000Z", "session_meta", map[string]any{
			"id": mainID, "cwd": "/tmp/proj", "originator": "codex_cli_rs", "cli_version": "0.153.4"}),
		rec("2026-09-08T10:00:00.100Z", "turn_context", map[string]any{
			"cwd": "/tmp/proj", "model": "gpt-5.3-codex", "effort": "high"}),
		rec("2026-09-08T10:00:00.200Z", "response_item", msg("developer", "input_text", "be terse")),
		rec("2026-09-08T10:00:00.300Z", "response_item", msg("user", "input_text", "<environment_context>\n<cwd>/tmp/proj</cwd>\n</environment_context>")),
		rec("2026-09-08T10:00:00.400Z", "response_item", msg("user", "input_text", "please list the files")),
		rec("2026-09-08T10:00:05.000Z", "response_item", map[string]any{"type": "reasoning",
			"summary": []map[string]any{{"type": "summary_text", "text": "Listing first."}}}),
		rec("2026-09-08T10:00:06.000Z", "response_item", map[string]any{"type": "function_call", "name": "shell",
			"arguments": `{"command":["bash","-lc","ls -la"],"workdir":"/tmp/proj"}`, "call_id": "call_1"}),
		rec("2026-09-08T10:00:07.000Z", "response_item", map[string]any{"type": "function_call_output",
			"call_id": "call_1", "output": "a\nb"}),
		rec("2026-09-08T10:00:08.000Z", "response_item", map[string]any{"type": "custom_tool_call", "name": "apply_patch",
			"input": "*** Begin Patch\n*** Update File: /tmp/proj/foo.go\n@@\n-old\n+new\n*** End Patch", "call_id": "call_2"}),
		rec("2026-09-08T10:00:09.000Z", "response_item", map[string]any{"type": "custom_tool_call_output",
			"call_id": "call_2", "output": map[string]any{"output": "Success. Updated the following files:\nM foo.go", "metadata": map[string]any{"exit_code": 0}}}),
		rec("2026-09-08T10:00:10.000Z", "response_item", msg("assistant", "output_text", "Done: two files.")),
		rec("2026-09-08T10:00:11.000Z", "event_msg", map[string]any{"type": "token_count", "info": nil}),
		rec("2026-09-08T10:01:00.000Z", "compacted", map[string]any{"message": "Earlier: listed and patched."}),
		rec("2026-09-08T10:02:00.000Z", "response_item", msg("user", "input_text", "thanks")),
	}
	if err := os.WriteFile(main, []byte(strings.Join(lines, "\n")+"\n"), 0o600); err != nil {
		t.Fatal(err)
	}

	subLines := []string{
		rec("2026-09-08T10:05:00.000Z", "session_meta", map[string]any{
			"id": subID, "cwd": "/tmp/proj", "parent_thread_id": mainID}),
		rec("2026-09-08T10:05:00.100Z", "turn_context", map[string]any{"cwd": "/tmp/proj", "model": "gpt-5.3-codex-mini"}),
		rec("2026-09-08T10:05:01.000Z", "response_item", msg("user", "input_text", "explore the repo")),
		rec("2026-09-08T10:05:02.000Z", "response_item", msg("assistant", "output_text", "It has two files.")),
	}
	if err := os.WriteFile(sub, []byte(strings.Join(subLines, "\n")+"\n"), 0o600); err != nil {
		t.Fatal(err)
	}

	index := strings.Join([]string{
		`{"id":"` + mainID + `","thread_name":"first name","updated_at":"2026-09-08T10:00:30Z"}`,
		`{"id":"` + mainID + `","thread_name":"Listing and patching","updated_at":"2026-09-08T10:01:30Z"}`,
		`{"id":"` + subID + `","thread_name":"was named","updated_at":"2026-09-08T10:05:30Z"}`,
		`{"id":"` + subID + `","thread_name":"","updated_at":"2026-09-08T10:06:30Z"}`,
	}, "\n") + "\n"
	if err := os.WriteFile(filepath.Join(home, "session_index.jsonl"), []byte(index), 0o600); err != nil {
		t.Fatal(err)
	}
	return home, main, sub
}

func TestListFiltersSubagentsAndCwd(t *testing.T) {
	home, main, sub := writeStore(t)
	root := filepath.Join(home, "sessions")

	infos, err := ad.List([]string{root}, session.ListOpts{SnippetLen: 120, NameLen: 40, Jobs: 2})
	if err != nil {
		t.Fatal(err)
	}
	if len(infos) != 1 || infos[0].Path != main {
		t.Fatalf("want only the parent rollout, got %+v", infos)
	}
	got := infos[0]
	if got.Name != "Listing and patching" {
		t.Errorf("name: last index line should win, got %q", got.Name)
	}
	if got.Snippet != "please list the files" {
		t.Errorf("snippet skipped the wrong things: %q", got.Snippet)
	}
	if got.Rel != filepath.Join("2026", "09", "08", filepath.Base(main)) {
		t.Errorf("rel = %q", got.Rel)
	}
	if got.Stamp == "" || got.Epoch == 0 {
		t.Errorf("no time: %+v", got)
	}

	with, err := ad.List([]string{root}, session.ListOpts{Subagents: true, SnippetLen: 120, NameLen: 40, Jobs: 1})
	if err != nil {
		t.Fatal(err)
	}
	if len(with) != 2 {
		t.Errorf("-subagents should list both, got %d", len(with))
	}
	for _, i := range with {
		if i.Path == sub && i.Name != "" {
			t.Errorf("a cleared name should be empty, got %q", i.Name)
		}
	}

	if _, err := ad.List([]string{root}, session.ListOpts{Cwd: "/elsewhere", SnippetLen: 1, NameLen: 1, Jobs: 1}); err == nil {
		t.Error("-cwd with no match should fail")
	}
	scoped, err := ad.List([]string{root}, session.ListOpts{Cwd: "/tmp/proj/", SnippetLen: 1, NameLen: 1, Jobs: 1})
	if err != nil || len(scoped) != 1 {
		t.Errorf("-cwd should match the cleaned cwd: %v %d", err, len(scoped))
	}
}

// `-last-by user` dates a thread by the last message the user typed. A thread
// left running writes reasoning and tool output for as long as it works, and
// the context Codex injects ahead of a prompt is a user message too, so
// neither may be what the row is dated by.
func TestListLastByUser(t *testing.T) {
	home, main, _ := writeStore(t)
	root := filepath.Join(home, "sessions")

	// The fixture ends on the user's "thanks"; give it a tail of the kind
	// that makes the two modes differ.
	rec := func(ts, typ string, payload any) string {
		raw, _ := json.Marshal(payload)
		b, _ := json.Marshal(map[string]any{"timestamp": ts, "type": typ, "payload": json.RawMessage(raw)})
		return string(b)
	}
	tail := strings.Join([]string{
		rec("2026-09-08T10:03:00.000Z", "response_item", map[string]any{"type": "message", "role": "assistant",
			"content": []map[string]any{{"type": "output_text", "text": "you are welcome"}}}),
		rec("2026-09-08T10:04:00.000Z", "response_item", map[string]any{"type": "message", "role": "user",
			"content": []map[string]any{{"type": "input_text", "text": "<environment_context>\n<cwd>/tmp/proj</cwd>\n</environment_context>"}}}),
	}, "\n") + "\n"

	fh, err := os.OpenFile(main, os.O_APPEND|os.O_WRONLY, 0o600)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := fh.WriteString(tail); err != nil {
		t.Fatal(err)
	}
	fh.Close()

	stamp := func(o session.ListOpts) string {
		o.SnippetLen, o.NameLen, o.Jobs = 120, 40, 1
		infos, err := ad.List([]string{root}, o)
		if err != nil {
			t.Fatal(err)
		}
		if len(infos) != 1 {
			t.Fatalf("want one rollout, got %d", len(infos))
		}
		return time.Unix(infos[0].Epoch, 0).UTC().Format(time.RFC3339)
	}

	if got := stamp(session.ListOpts{}); got != "2026-09-08T10:04:00Z" {
		t.Errorf("-last-by any: got %q, want the newest record", got)
	}
	if got := stamp(session.ListOpts{LastBy: session.LastByUser}); got != "2026-09-08T10:02:00Z" {
		t.Errorf("-last-by user: got %q, want the last typed prompt", got)
	}
}

func TestNameAndMeta(t *testing.T) {
	_, main, sub := writeStore(t)

	if n, _ := ad.Name(main); n != "Listing and patching" {
		t.Errorf("Name = %q", n)
	}
	if n, _ := ad.Name(sub); n != "" {
		t.Errorf("cleared name should be empty, got %q", n)
	}
	m, err := ad.Meta(main)
	if err != nil {
		t.Fatal(err)
	}
	if m.ID != "11111111-1111-4111-8111-111111111111" || m.Cwd != "/tmp/proj" || m.Name != "Listing and patching" {
		t.Errorf("Meta = %+v", m)
	}
	if _, err := ad.Name(filepath.Join(t.TempDir(), "nope.jsonl")); err == nil {
		t.Error("a missing file should fail")
	}
}

func TestDocumentTurns(t *testing.T) {
	_, main, _ := writeStore(t)

	doc, err := ad.Document(main, session.DocOpts{Subagents: true})
	if err != nil {
		t.Fatal(err)
	}

	roles := make([]string, 0, len(doc.Turns))
	for _, tu := range doc.Turns {
		if tu.Heading != "" {
			roles = append(roles, tu.Heading)
			continue
		}
		roles = append(roles, tu.Role)
	}
	want := []string{"Session instructions", "user", "assistant", "Context compacted", "user"}
	if strings.Join(roles, ",") != strings.Join(want, ",") {
		t.Errorf("turns = %v, want %v", roles, want)
	}

	// The launch scaffold is its own folded turn, kept verbatim because it is
	// XML rather than prose, and the developer message is not a turn at all.
	sc := doc.Turns[0]
	if !sc.Folded {
		t.Errorf("scaffold turn is not folded")
	}
	if len(sc.Blocks) != 1 || sc.Blocks[0].B.Type != "event" ||
		sc.Blocks[0].B.Name != "Environment context" ||
		sc.Blocks[0].B.Text != "<cwd>/tmp/proj</cwd>" {
		t.Errorf("scaffold block = %+v", sc.Blocks[0].B)
	}

	// What the person typed is the user turn, with no scaffolding in it.
	if doc.Turns[1].Blocks[0].B.Text != "please list the files" {
		t.Errorf("first user block = %q", doc.Turns[1].Blocks[0].B.Text)
	}

	// Reasoning, two tool calls and the message merge into one assistant turn.
	a := doc.Turns[2]
	if a.Model != "gpt-5.3-codex" {
		t.Errorf("model = %q", a.Model)
	}
	types := make([]string, 0, len(a.Blocks))
	for _, b := range a.Blocks {
		types = append(types, b.B.Type)
	}
	if strings.Join(types, ",") != "thinking,tool_use,tool_use,text" {
		t.Errorf("assistant blocks = %v", types)
	}

	// The argv array became the script, and the result is indexed by call id.
	var in map[string]string
	if err := json.Unmarshal(a.Blocks[1].B.Input, &in); err != nil {
		t.Fatal(err)
	}
	if in["command"] != "ls -la" {
		t.Errorf("command = %q", in["command"])
	}
	if r, ok := doc.Results["call_1"]; !ok || r.Body != "a\nb" {
		t.Errorf("result for call_1 = %+v", r)
	}
	if r := doc.Results["call_2"]; !strings.HasPrefix(r.Body, "Success.") {
		t.Errorf("structured output not unwrapped: %q", r.Body)
	}

	if len(doc.Subagents) != 1 || !strings.Contains(doc.Subagents[0].Title, "@Gpt5.3codexmini") &&
		!strings.Contains(doc.Subagents[0].Title, "Subagent 22222222") {
		t.Errorf("subagents = %+v", doc.Subagents)
	}
	if len(doc.Subagents[0].Turns) != 2 {
		t.Errorf("subagent turns = %d", len(doc.Subagents[0].Turns))
	}

	// And the whole thing renders, with the Codex headlines.
	out, err := turns.Render(doc, turns.Options{Format: "org", Jobs: 1, Diff: true})
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{
		"** Tool Use: shell · ls -la",
		"** Tool Use: apply_patch · /tmp/proj/foo.go",
		"#+begin_src diff",
		"* Context compacted",
		"* Subagents",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("rendered document lacks %q:\n%s", want, out)
		}
	}
}

func TestSplitSections(t *testing.T) {
	// The shape of a real first message: instructions, environment, then what
	// the person typed.
	text := "<INSTRUCTIONS>\n# Guidelines\n\nbe careful\n</INSTRUCTIONS>\n" +
		"<environment_context>\n  <cwd>/tmp</cwd>\n</environment_context>\n" +
		"design the thing"

	secs, typed := splitSections(text)
	if len(secs) != 2 {
		t.Fatalf("sections = %+v", secs)
	}
	if secs[0].tag != "INSTRUCTIONS" || secs[0].body != "# Guidelines\n\nbe careful" {
		t.Errorf("first section = %+v", secs[0])
	}
	if secs[1].tag != "environment_context" || secs[1].body != "  <cwd>/tmp</cwd>" {
		t.Errorf("second section = %+v", secs[1])
	}
	if typed != "design the thing" {
		t.Errorf("typed = %q", typed)
	}

	// A close on the body's own last line, which some sections use.
	secs, typed = splitSections("<user_instructions>\nx</user_instructions>")
	if len(secs) != 1 || secs[0].body != "x" || typed != "" {
		t.Errorf("same-line close: %+v, typed %q", secs, typed)
	}

	// A tag inside a sentence is text, not a section.
	secs, typed = splitSections("use <b>bold</b> here")
	if len(secs) != 0 || typed != "use <b>bold</b> here" {
		t.Errorf("inline tag: %+v, typed %q", secs, typed)
	}

	// An unclosed opening tag is text too.
	secs, typed = splitSections("<open>\nbody")
	if len(secs) != 0 || typed != "<open>\nbody" {
		t.Errorf("unclosed: %+v, typed %q", secs, typed)
	}

	for _, c := range []struct {
		tag              string
		scaffold, prose_ bool
	}{
		{"INSTRUCTIONS", true, true},
		{"user_instructions", true, true},
		{"environment_context", true, false},
		{"turn_aborted", false, false},
	} {
		if got := scaffoldSection(c.tag); got != c.scaffold {
			t.Errorf("scaffoldSection(%q) = %v", c.tag, got)
		}
		if got := proseSection(c.tag); got != c.prose_ {
			t.Errorf("proseSection(%q) = %v", c.tag, got)
		}
	}
	if got := sectionLabel("environment_context"); got != "Environment context" {
		t.Errorf("sectionLabel = %q", got)
	}
}

func TestScaffoldText(t *testing.T) {
	for _, c := range []struct {
		in   string
		want bool
	}{
		{"<environment_context>\nx\n</environment_context>", true},
		{"<user_instructions>\nx</user_instructions>", true},
		{"<turn_aborted/>", false},
		{"<b>bold</b> and more", false},
		{"plain", false},
		{"<a href=x>y</a>", false},
	} {
		if got := scaffoldText(c.in); got != c.want {
			t.Errorf("scaffoldText(%q) = %v", c.in, got)
		}
	}
}

func TestIsCodexCmd(t *testing.T) {
	for _, c := range []struct {
		in   string
		want bool
	}{
		{"codex", true},
		{"codex --search", true},
		{"node /opt/homebrew/lib/node_modules/@openai/codex/bin/codex.js --search", true},
		{"/opt/homebrew/bin/codex resume abc", true},
		{"codex exec -", false},
		{"node /x/codex.js agents", false},
		{"codex_status.py", false},
		{"/Users/x/codex-notes/editor", false},
		{"vim codex.md", false},
	} {
		if got := isCodexCmd(c.in); got != c.want {
			t.Errorf("isCodexCmd(%q) = %v", c.in, got)
		}
	}
}

func TestLivePairsLockHolders(t *testing.T) {
	home, main, sub := writeStore(t)
	root := filepath.Join(home, "sessions")
	lockDir := filepath.Join(home, "thread-writer-locks")
	if err := os.MkdirAll(lockDir, 0o755); err != nil {
		t.Fatal(err)
	}
	mainLock := filepath.Join(lockDir, "11111111-1111-4111-8111-111111111111.lock")
	subLock := filepath.Join(lockDir, "22222222-2222-4222-8222-222222222222.lock")
	staleLock := filepath.Join(lockDir, "33333333-3333-4333-8333-333333333333.lock")
	for _, p := range []string{mainLock, subLock, staleLock} {
		os.WriteFile(p, nil, 0o600)
	}
	_ = sub

	proc.ResetShared()
	defer proc.ResetShared()
	oldRun := proc.Run
	proc.Run = func(name string, args ...string) ([]byte, error) {
		switch name {
		case "ps":
			return []byte("" +
				"  100     1 /bin/zsh -l\n" +
				"  200   100 tmux\n" +
				"  300   200 -zsh\n" +
				"  400   300 node /x/@openai/codex/bin/codex.js --search\n" +
				"  401   400 /x/vendor/bin/codex -c foo\n" +
				"  600     1 /Applications/ChatGPT.app/Contents/MacOS/ChatGPT\n" +
				"  601   600 /Applications/ChatGPT.app/Contents/Resources/codex -c bar\n"), nil
		case "lsof":
			if len(args) > 0 && args[0] == "-Fpn" {
				// The holders: the native child holds the parent's lock, the
				// desktop app the subagent's; the stale lock is nobody's.
				return []byte("p401\nn" + mainLock + "\np601\nn" + subLock + "\n"), nil
			}
			return []byte("p400\nn/tmp/proj\np401\nn/tmp/proj\np601\nn/\n"), nil
		case "tmux":
			return []byte("300\tcodex-work\n"), nil
		}
		return nil, nil
	}
	defer func() { proc.Run = oldRun }()

	rows, err := ad.Live([]string{root})
	if err != nil {
		t.Fatal(err)
	}
	// The subagent's thread is dropped even though its lock is held.
	if len(rows) != 1 {
		t.Fatalf("want one row, got %+v", rows)
	}
	r := rows[0]
	if r.PID != 400 {
		t.Errorf("pid should be the launcher, the topmost codex ancestor: %+v", r)
	}
	if r.Tmux != "codex-work" || r.Transcript != main || r.Name != "Listing and patching" || r.Cwd != "/tmp/proj" || r.Status != "running" {
		t.Errorf("row = %+v", r)
	}
	if got := r.Row(); strings.Count(got, "\t") != 7 {
		t.Errorf("row has %d tabs: %q", strings.Count(got, "\t"), got)
	}
}

func TestTopmostCodex(t *testing.T) {
	byPID := proc.ByPID([]proc.Process{
		{PID: 300, PPID: 200, Cmd: "-zsh"},
		{PID: 400, PPID: 300, Cmd: "node /x/codex.js"},
		{PID: 401, PPID: 400, Cmd: "/x/vendor/bin/codex -c a"},
		{PID: 402, PPID: 401, Cmd: "/x/vendor/bin/codex mcp-server"},
	})
	if got := topmostCodex(402, byPID); got != 400 {
		t.Errorf("topmostCodex(402) = %d", got)
	}
	if got := topmostCodex(300, byPID); got != 300 {
		t.Errorf("a non-codex pid stays itself, got %d", got)
	}
}

func TestOutputTextUnwraps(t *testing.T) {
	// A plain string result: text, no language.
	if body, lang := outputText(json.RawMessage(`"a\nb"`)); body != "a\nb" || lang != "" {
		t.Errorf("string result = %q, %q", body, lang)
	}

	// The shell shape: the object's `output` is what the command printed.
	raw := json.RawMessage(`{"exit_code":0,"output":"one\ntwo","wall_time_seconds":1.4}`)
	if body, lang := outputText(raw); body != "one\ntwo" || lang != "" {
		t.Errorf("shell result = %q, %q", body, lang)
	}

	// A list of content parts whose single text is itself a JSON document:
	// unwrapped, then indented, and marked as json.
	raw = json.RawMessage(`[{"type":"input_text","text":"[{\"name\":\"apply_patch\"}]"}]`)
	body, lang := outputText(raw)
	if lang != "json" {
		t.Errorf("lang = %q", lang)
	}
	if !strings.Contains(body, "\n    \"name\": \"apply_patch\"") {
		t.Errorf("not pretty printed: %q", body)
	}

	// Several parts stay several documents, separated by a blank line.
	raw = json.RawMessage(`[{"type":"input_text","text":"first"},{"type":"input_text","text":"{\"output\":\"second\"}"}]`)
	body, lang = outputText(raw)
	if body != "first\n\nsecond" || lang != "" {
		t.Errorf("two parts = %q, %q", body, lang)
	}

	// An object with no output field is shown as an indented document.
	raw = json.RawMessage(`{"chunk_id":"a6","exit_code":0}`)
	body, lang = outputText(raw)
	if lang != "json" || !strings.Contains(body, "\n  \"chunk_id\": \"a6\"") {
		t.Errorf("object = %q, %q", body, lang)
	}

	// Not JSON at all, and nothing at all.
	if body, _ := outputText(json.RawMessage(`plain text`)); body != "plain text" {
		t.Errorf("plain = %q", body)
	}
	if body, _ := outputText(nil); body != "" {
		t.Errorf("empty = %q", body)
	}
}
