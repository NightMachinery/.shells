package main

import (
	"encoding/json"
	"os"
	"path/filepath"
	"testing"
)

func TestAgentCommand(t *testing.T) {
	cases := []struct {
		name string
		argv []string
		want string
	}{
		{"empty", nil, ""},
		{"empty executable", []string{""}, ""},
		{"native claude", []string{"/bin/claude", "codex"}, "claude"},
		{"claude profile", []string{"/bin/claude-work"}, "claude"},
		{"claude exe", []string{"claude.exe"}, "claude"},
		{"native codex", []string{"/bin/codex"}, "codex"},
		{"node claude", []string{"node", "/bin/claude"}, "claude"},
		{"node flag", []string{"node", "--no-warnings", "/bin/codex"}, "codex"},
		{"default wrapper", []string{"decset-rewrite", "-map", "1003=1002", "--", "/bin/claude"}, "claude"},
		{"wrapped node", []string{"/bin/decset-rewrite", "-trace", "claude", "--", "node", "/bin/codex"}, "codex"},
		{"legacy wrapper", []string{"decset-rewrite", "-map", "1003=1002", "/bin/codex"}, "codex"},
		{"legacy option value", []string{"decset-rewrite", "-trace", "claude", "/bin/codex"}, "codex"},
		{"inline option", []string{"decset-rewrite", "-map=1003=1002", "/bin/claude"}, "claude"},
		{"proxy alone", []string{"decset-rewrite"}, ""},
		{"separator alone", []string{"decset-rewrite", "--"}, ""},
		{"missing option value", []string{"decset-rewrite", "-map"}, ""},
		{"only agent-named value", []string{"decset-rewrite", "-trace", "claude"}, ""},
		{"value before empty command", []string{"decset-rewrite", "-map", "codex", "--"}, ""},
		{"unknown wrapped app", []string{"decset-rewrite", "--", "echo", "claude"}, ""},
		{"child separator", []string{"decset-rewrite", "echo", "--", "claude"}, ""},
		{"node eval child separator", []string{"decset-rewrite", "node", "-e", "0", "--", "codex"}, ""},
		{"shell prompt", []string{"decset-rewrite", "--", "bash", "-c", "codex"}, ""},
		{"node eval", []string{"decset-rewrite", "--", "node", "-e", "claude"}, ""},
		{"node require", []string{"decset-rewrite", "--", "node", "--require", "codex"}, ""},
		{"node other script", []string{"decset-rewrite", "--", "node", "/bin/other", "claude"}, ""},
		{"wrapped unsupported agy", []string{"decset-rewrite", "--", "agy"}, ""},
		{"unsupported agy", []string{"agy"}, ""},
		{"native argument mention", []string{"echo", "codex"}, ""},
		{"inline eval", []string{"node", "--eval=0", "claude"}, ""},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := agentCommand(tc.argv); got != tc.want {
				t.Fatalf("agentCommand(%q) = %q; want %q", tc.argv, got, tc.want)
			}
		})
	}
}

func TestKittyAgentWrappedForeground(t *testing.T) {
	dir := t.TempDir()
	listing := filepath.Join(dir, "windows.json")
	if err := os.WriteFile(filepath.Join(dir, "kitten"), []byte("#!/bin/sh\nexec /bin/cat \"$KITTY_TEST_LISTING\"\n"), 0700); err != nil {
		t.Fatal(err)
	}
	t.Setenv("PATH", dir+string(os.PathListSeparator)+os.Getenv("PATH"))
	t.Setenv("KITTY_TEST_LISTING", listing)
	for _, tc := range []struct {
		argv []string
		want string
	}{
		{[]string{"decset-rewrite", "-map", "1003=1002", "--", "claude"}, "claude"},
		{[]string{"decset-rewrite", "-map", "1003=1002", "--", "node", "codex"}, "codex"},
		{[]string{"decset-rewrite", "--", "bash", "-c", "codex"}, ""},
		{[]string{"decset-rewrite", "--", "agy"}, ""},
	} {
		command, _ := json.Marshal(tc.argv)
		data := `[{"tabs":[{"windows":[{"id":99,"columns":80,"foreground_processes":[{"pid":1,"cmdline":["codex"]}]},{"id":7,"columns":100,"foreground_processes":[{"pid":8,"cmdline":["echo","claude"]},{"pid":42,"cmdline":` + string(command) + `}]}]}]}]`
		if err := os.WriteFile(listing, []byte(data), 0600); err != nil {
			t.Fatal(err)
		}
		agent, pid, columns, err := kittyAgent(TerminalRequest{Target: "7", Socket: "inert-test-socket"})
		if tc.want == "" {
			if err == nil || agent != "" || pid != 0 {
				t.Fatalf("unsupported foreground accepted: %q %d %v", agent, pid, err)
			}
		} else if err != nil || agent != tc.want || pid != 42 || columns != 100 {
			t.Fatalf("wrong foreground identity: %q %d %d %v", agent, pid, columns, err)
		}
	}
}
