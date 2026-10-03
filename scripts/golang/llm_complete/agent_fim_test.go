package main

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestFixtureInputsAndContext(t *testing.T) {
	for _, name := range []string{"claude", "codex"} {
		b, err := os.ReadFile("testdata/" + name + ".json")
		if err != nil {
			t.Fatal(err)
		}
		var s Screen
		if json.Unmarshal(b, &s) != nil {
			t.Fatal("fixture")
		}
		in, err := extract(s)
		if err != nil {
			t.Fatal(err)
		}
		want := "first row\nsecond = "
		if name == "codex" {
			want = "first row\nsecond ="
		}
		if in.Prefix != want || in.Suffix == "" {
			t.Fatalf("%s: %+v", name, in)
		}
		c := Config{AgentFIM: AgentConfig{PrefixChars: ptr(5), SuffixChars: ptr(4), ReplyChars: ptr(3)}}
		r, err := agentContext(c, AgentContextRequest{Screen: s, LastReply: "reply سلام", Source: "tmux"})
		if err != nil || !strings.Contains(r.Prefix, "لام\n\nCurrent prompt:\n") || !strings.HasSuffix(r.Prefix, lastChars(in.Prefix, 5)) || r.Suffix != firstChars(in.Suffix, 4) {
			t.Fatalf("%+v %v", r, err)
		}
		r, err = agentContext(c, AgentContextRequest{Screen: s})
		if err != nil || !strings.Contains(r.Prefix, lastChars(in.Above, 3)) {
			t.Fatal("screen fallback")
		}
		c.AgentFIM.PrefixChars = ptr(-1)
		if _, err = agentContext(c, AgentContextRequest{Screen: s}); err == nil {
			t.Fatal("negative budget")
		}
	}
}
func TestLogsExactRequestPrivacyAndRotation(t *testing.T) {
	dir := filepath.Join(t.TempDir(), "logs")
	t.Setenv("LLM_COMPLETE_LOG_DIR", dir)
	c := Config{DefaultProvider: "test", Providers: map[string]Provider{"test": {Name: "test", KeyVar: "test_key", Parameters: Parameters{Model: ptr("test-model")}}}, Logging: LogConfig{MaxBytes: 1024, Files: 2}}
	t.Setenv("test_key", "inert-secret-token")
	r := FIMRequest{Prefix: "count =\nسلام ", Suffix: " tail", Source: "kitty", Target: "window-7"}
	body := map[string]any{"model": "test-model", "prompt": r.Prefix, "suffix": r.Suffix, "max_tokens": 64, "stop": "\n", "temperature": 0}
	if err := writeFIMLog(c, r, body, time.Now(), " 0", "", 0); err != nil {
		t.Fatal(err)
	}
	b, err := os.ReadFile(filepath.Join(dir, "completion.log"))
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(b), r.Prefix+"⟦CURSOR⟧"+r.Suffix) || !strings.Contains(string(b), "window-7") || !strings.Contains(string(b), `"max_tokens": 64`) {
		t.Fatal(string(b))
	}
	r.Prefix = "inert-secret-token"
	if err := writeFIMLog(c, r, body, time.Now(), "inert-secret-token", "", 0); err != nil {
		t.Fatal(err)
	}
	for i := 0; i < 6; i++ {
		if err := writeFIMLog(c, r, body, time.Now(), " 0", "", 0); err != nil {
			t.Fatal(err)
		}
	}
	entries, _ := os.ReadDir(dir)
	for _, e := range entries {
		st, _ := e.Info()
		if st.Mode().Perm() != 0600 || st.Size() > 1024 {
			t.Fatalf("mode/size %s %v", e.Name(), st)
		}
		data, _ := os.ReadFile(filepath.Join(dir, e.Name()))
		if strings.Contains(string(data), "inert-secret-token") {
			t.Fatal("logged key")
		}
	}
	st, _ := os.Stat(dir)
	if st.Mode().Perm() != 0700 {
		t.Fatal("directory mode")
	}
	for _, src := range []string{"zsh", "emacs", "hammerspoon"} {
		r.Source = src
		if loggingEnabled(c, r) {
			t.Fatal("unexpected default logging")
		}
		r.Log = ptr(true)
		if !loggingEnabled(c, r) {
			t.Fatal("opt in")
		}
		r.Log = nil
	}
	r.Source = "tmux"
	c.AgentFIM.Log = ptr(false)
	if loggingEnabled(c, r) {
		t.Fatal("agent opt out")
	}
	c.Logging.Sources = map[string]bool{"tmux": true}
	if !loggingEnabled(c, r) {
		t.Fatal("source override")
	}
}
func TestStalenessAndLock(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	r := TerminalRequest{Source: "tmux", Target: "%1", Socket: "scratch"}
	release, err := targetLock(r)
	if err != nil {
		t.Fatal(err)
	}
	if _, err = targetLock(r); err == nil {
		t.Fatal("lock not exclusive")
	}
	release()
	release, err = targetLock(r)
	if err != nil {
		t.Fatal(err)
	}
	release()
	s := Screen{Agent: "codex", ProcessID: 1, X: 4, Lines: []Line{{Text: "› al"}}}
	if !sameInput(s, s) {
		t.Fatal("same input")
	}
	b := s
	b.ProcessID = 2
	if sameInput(s, b) {
		t.Fatal("process change")
	}
	b = s
	b.X = 3
	if sameInput(s, b) {
		t.Fatal("caret change")
	}
	if err = staleTicket(r); err != nil {
		t.Fatal(err)
	}
	data, _ := os.ReadFile(ticket(r))
	if !strings.Contains(string(data), "dabbrev-") {
		t.Fatal("ticket")
	}
}
func TestCaptureWrapsAndControlSafety(t *testing.T) {
	s := tmuxScreen("› abcdef\n  ghij\n\n", "› abcdef  ghij\n\n", "codex", 5, 1)
	if !s.Lines[0].Wrapped || s.Lines[1].Wrapped {
		t.Fatalf("%+v", s.Lines)
	}
	in, err := extract(s)
	if err != nil || in.Prefix != "abcdefghi" || in.Suffix != "j" {
		t.Fatalf("%+v %v", in, err)
	}
	s, err = kittyScreen("› abcdef\r  ghij\r\n\x1b[?25h\x1b[2;6H", "codex")
	if err != nil {
		t.Fatal(err)
	}
	if !s.Lines[0].Wrapped {
		t.Fatal("kitty wrap")
	}
	if len(s.Lines) != 2 || s.Lines[1].Wrapped {
		t.Fatal("kitty hard break", s.Lines)
	}
	if sanitise("a\n\r\x1b\x00\x7fb\u2028c") != "abc" {
		t.Fatal("controls")
	}
}

func TestCachedCorpusIdentityAndWrapPadding(t *testing.T) {
	t.Setenv("XDG_STATE_HOME", t.TempDir())
	path := filepath.Join(t.TempDir(), "fixture.jsonl")
	os.WriteFile(path, []byte("fabricated"), 0600)
	st, _ := os.Stat(path)
	r := TerminalRequest{Source: "kitty", Target: "1", Socket: "scratch", Screen: Screen{ProcessID: 7}}
	c := ContextCorpus{Corpus: []string{"knownWord"}, Transcript: path, Mtime: st.ModTime().UnixNano(), Size: st.Size(), ProcessID: 7}
	if err := writePrivate(contextFile(r), c); err != nil {
		t.Fatal(err)
	}
	if len(cachedContext(r).Corpus) != 1 {
		t.Fatal("fresh cache")
	}
	r.Screen.ProcessID = 8
	if len(cachedContext(r).Corpus) != 0 {
		t.Fatal("reused window cache")
	}
	s := tmuxScreen("› al    \n        \n", "› al\n\n", "codex", 4, 0)
	if s.Lines[0].Wrapped || s.Lines[1].Wrapped {
		t.Fatal("padding mistaken for wrap")
	}
}

func TestClaudeVimNoNormalLabel(t *testing.T) {
	s := Screen{Agent: "claude", Vim: true, X: 4, Lines: []Line{{Text: "❯ al"}, {Text: "─────"}, {Text: "manual mode on"}}}
	if _, err := extract(s); err == nil {
		t.Fatal("unlabelled NORMAL allowed")
	}
	s.Lines[2].Text = "-- INSERT -- manual mode on"
	if _, err := extract(s); err != nil {
		t.Fatal(err)
	}
	s.Vim = false
	s.Lines[2].Text = "manual mode on"
	if _, err := extract(s); err != nil {
		t.Fatal("non-vim refused", err)
	}
	t.Setenv("CLAUDE_CONFIG_DIR", t.TempDir())
	os.WriteFile(filepath.Join(os.Getenv("CLAUDE_CONFIG_DIR"), "settings.json"), []byte(`{"editorMode":"vim","ignored":"fabricated"}`), 0600)
	cwd := t.TempDir()
	if !claudeVim(cwd) {
		t.Fatal("user vim")
	}
	os.Mkdir(filepath.Join(cwd, ".claude"), 0700)
	os.WriteFile(filepath.Join(cwd, ".claude/settings.local.json"), []byte(`{"editorMode":"normal"}`), 0600)
	if claudeVim(cwd) {
		t.Fatal("local override")
	}
}

func TestInputBlankLinesAfterCaret(t *testing.T) {
	for _, agent := range []string{"claude", "codex"} {
		m, footer := "❯", "──────"
		if agent == "codex" {
			m, footer = "›", "  GPT-test · Context 0% used · weekly limit"
		}
		s := Screen{Agent: agent, X: 7, Lines: []Line{{Text: m + " first"}, {Text: "  "}, {Text: "  last"}, {Text: ""}, {Text: footer}}}
		if agent == "claude" {
			s.Lines = append(s.Lines, Line{Text: "-- INSERT --"})
			s.Lines = append(s.Lines[:3], s.Lines[4:]...)
		}
		in, err := extract(s)
		if err != nil || in.Prefix != "first" || in.Suffix != "\n\nlast" {
			t.Fatalf("%s: %+v %v", agent, in, err)
		}
	}
}

func TestEditorWrapWithoutTerminalFlag(t *testing.T) {
	s := Screen{Agent: "codex", Columns: 10, Y: 1, X: 4, Lines: []Line{{Text: "› abcdefgh"}, {Text: "  ijtail"}, {Text: ""}, {Text: "? for shortcuts"}}}
	in, err := extract(s)
	if err != nil || in.Prefix != "abcdefghij" || in.Suffix != "tail" {
		t.Fatal(in, err)
	}
}
