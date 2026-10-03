package main

import (
	"context"
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

type ContextCorpus struct {
	Corpus     []string `json:"corpus"`
	LastReply  string   `json:"last_reply"`
	Transcript string   `json:"transcript"`
	Mtime      int64    `json:"mtime"`
	Size       int64    `json:"size"`
}

func rootDir() string {
	if r := os.Getenv("NIGHTDIR"); r != "" {
		return r
	}
	h, _ := os.UserHomeDir()
	return filepath.Join(h, "scripts")
}
func contextFile(r TerminalRequest) string { return targetFile(r) + ".context" }
func cachedContext(r TerminalRequest) ContextCorpus {
	var c ContextCorpus
	b, err := os.ReadFile(contextFile(r))
	if err != nil || json.Unmarshal(b, &c) != nil {
		return ContextCorpus{}
	}
	st, err := os.Stat(c.Transcript)
	if err != nil || st.ModTime().UnixNano() != c.Mtime || st.Size() != c.Size {
		return ContextCorpus{}
	}
	return c
}
func loadContext(r TerminalRequest) ContextCorpus {
	var c ContextCorpus
	listing, err := kitty(r, "", "ls")
	if err != nil {
		return c
	}
	transcript, err := child(2*time.Second, listing, filepath.Join(rootDir(), "bin/agent-completion-session.zsh"), r.Target, fmtInt(r.KittyPID))
	if err != nil {
		return c
	}
	transcript = strings.TrimSpace(transcript)
	st, err := os.Stat(transcript)
	if err != nil {
		return c
	}
	result, err := child(120*time.Millisecond, "", "agent_session", r.Screen.Agent, "completion-context", transcript)
	if err != nil || json.Unmarshal([]byte(result), &c) != nil {
		return ContextCorpus{}
	}
	c.Transcript = transcript
	c.Mtime = st.ModTime().UnixNano()
	c.Size = st.Size()
	_ = writePrivate(contextFile(r), c)
	return c
}
func gitPaths(cwd string) string {
	// A corpus query, not repository mutation. Never inherit a vcsh GIT_DIR.
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Millisecond)
	defer cancel()
	cmd := exec.CommandContext(ctx, "git", "-C", cwd, "ls-files", "-z")
	cmd.Env = nil
	for _, e := range os.Environ() {
		if !strings.HasPrefix(e, "GIT_DIR=") && !strings.HasPrefix(e, "GIT_WORK_TREE=") {
			cmd.Env = append(cmd.Env, e)
		}
	}
	b, err := cmd.Output()
	if err != nil {
		return ""
	}
	return strings.ReplaceAll(string(b), "\x00", "\n")
}
func terminalCorpora(r TerminalRequest) []string {
	c := cachedContext(r)
	cs := append([]string{}, r.Others...)
	cs = append(cs, c.Corpus...)
	cs = append(cs, gitPaths(r.Cwd))
	return cs
}

func fmtInt(n int) string { return strconv.Itoa(n) }
