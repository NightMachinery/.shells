package main

import (
	"context"
	"crypto/sha256"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"
	"time"
)

type ContextCorpus struct {
	Corpus     []string `json:"corpus"`
	LastReply  string   `json:"last_reply"`
	Transcript string   `json:"transcript"`
	Mtime      int64    `json:"mtime"`
	Size       int64    `json:"size"`
	ProcessID  int      `json:"process_id"`
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
	if err != nil || json.Unmarshal(b, &c) != nil || c.ProcessID != r.Screen.ProcessID {
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
	var transcript string
	var err error
	if r.Source == "tmux" {
		identity, e := tmux(r, "", "display-message", "-p", "-t", r.Target, "#{@agent_session}")
		if e != nil {
			return c
		}
		f := strings.Split(strings.TrimSpace(identity), "\t")
		if len(f) != 3 || f[0] != r.Screen.Agent {
			return c
		}
		transcript = f[2]
	} else {
		listing, e := kitty(r, "", "ls")
		if e != nil {
			return c
		}
		transcript, err = child(2*time.Second, listing, filepath.Join(rootDir(), "bin/agent-completion-session.zsh"), r.Target, fmtInt(r.KittyPID))
		if err != nil {
			return c
		}
		transcript = strings.TrimSpace(transcript)
	}
	st, err := os.Stat(transcript)
	if err != nil {
		return c
	}
	result, err := child(120*time.Millisecond, "", "agent_session", r.Screen.Agent, "completion-context", transcript)
	if err != nil || json.Unmarshal([]byte(result), &c) != nil {
		return ContextCorpus{}
	}
	c.ProcessID = r.Screen.ProcessID
	c.Transcript = transcript
	c.Mtime = st.ModTime().UnixNano()
	c.Size = st.Size()
	_ = writePrivate(contextFile(r), c)
	return c
}
func gitPaths(cwd string) string {
	return gitPathsWithin(cwd, 15*time.Millisecond)
}
func gitCommand(ctx context.Context, cwd string, args ...string) *exec.Cmd {
	cmd := exec.CommandContext(ctx, "git", append([]string{"-C", cwd}, args...)...)
	for _, e := range os.Environ() {
		if !strings.HasPrefix(e, "GIT_DIR=") && !strings.HasPrefix(e, "GIT_WORK_TREE=") {
			cmd.Env = append(cmd.Env, e)
		}
	}
	return cmd
}
func gitPathsWithin(cwd string, timeout time.Duration) string {
	// A corpus query, not repository mutation. Never inherit a vcsh GIT_DIR.
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	cmd := gitCommand(ctx, cwd, "ls-files", "-z")
	b, err := cmd.Output()
	if err != nil {
		return ""
	}
	return strings.ReplaceAll(string(b), "\x00", "\n")
}

type GitCorpus struct {
	Cwd   string `json:"cwd"`
	Index string `json:"index"`
	Mtime int64  `json:"mtime"`
	Size  int64  `json:"size"`
	Paths string `json:"paths"`
}

func gitCorpusFile(cwd string) string {
	h := sha256.Sum256([]byte(cwd))
	return filepath.Join(stateDir(), fmt.Sprintf("git-%x.json", h[:12]))
}
func cachedGit(cwd string) (string, bool) {
	var c GitCorpus
	b, err := os.ReadFile(gitCorpusFile(cwd))
	if err != nil || json.Unmarshal(b, &c) != nil || c.Cwd != cwd {
		return "", false
	}
	st, err := os.Stat(c.Index)
	if err != nil || st.ModTime().UnixNano() != c.Mtime || st.Size() != c.Size {
		return "", false
	}
	return c.Paths, true
}
func warmGit(cwd string) {
	if _, ok := cachedGit(cwd); ok {
		return
	}
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()
	b, err := gitCommand(ctx, cwd, "rev-parse", "--git-path", "index").Output()
	if err != nil {
		return
	}
	index := strings.TrimSpace(string(b))
	if !filepath.IsAbs(index) {
		index = filepath.Join(cwd, index)
	}
	st, err := os.Stat(index)
	if err != nil {
		return
	}
	paths, err := gitCommand(ctx, cwd, "ls-files", "-z").Output()
	if err != nil {
		return
	}
	_ = writePrivate(gitCorpusFile(cwd), GitCorpus{Cwd: cwd, Index: index, Mtime: st.ModTime().UnixNano(), Size: st.Size(), Paths: strings.ReplaceAll(string(paths), "\x00", "\n")})
}
func terminalCorpora(r TerminalRequest) []string {
	c := cachedContext(r)
	cs := append([]string{}, r.Others...)
	cs = append(cs, c.Corpus...)
	paths, ok := cachedGit(r.Cwd)
	if !ok {
		paths = gitPaths(r.Cwd)
	}
	cs = append(cs, paths)
	return cs
}

func fmtInt(n int) string { return strconv.Itoa(n) }

// Cold or changed transcripts warm off the keypress path, including when the
// first press has no screen candidates. Pass only identity metadata on stdin.
func warmContext(r TerminalRequest) {
	_, gitReady := cachedGit(r.Cwd)
	if cachedContext(r).Transcript != "" && gitReady {
		return
	}
	if privateDir(stateDir()) != nil {
		return
	}
	f, err := os.CreateTemp(stateDir(), ".warm-*")
	if err != nil {
		return
	}
	defer f.Close()
	defer os.Remove(f.Name())
	r.Screen.Lines = nil
	r.Others = nil
	if json.NewEncoder(f).Encode(r) != nil {
		return
	}
	if _, err = f.Seek(0, 0); err != nil {
		return
	}
	exe, err := os.Executable()
	if err != nil {
		return
	}
	cmd := exec.Command(exe, "terminal", "warm")
	cmd.Stdin = f
	cmd.SysProcAttr = &syscall.SysProcAttr{Setsid: true}
	if cmd.Start() == nil {
		_ = cmd.Process.Release()
	}
}
