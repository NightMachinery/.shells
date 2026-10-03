package main

import (
	"errors"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

func tmux(r TerminalRequest, input string, args ...string) (string, error) {
	return child(2*time.Second, input, "tmux", append([]string{"-S", r.Socket}, args...)...)
}
func tmuxScreen(physical, joined, agent string, x, y int) Screen {
	s := Screen{Agent: agent, X: x, Y: y}
	ps := strings.Split(strings.TrimSuffix(physical, "\n"), "\n")
	js := strings.Split(strings.TrimSuffix(joined, "\n"), "\n")
	j, used := 0, 0
	for _, p := range ps {
		l := Line{Text: p}
		if j < len(js) {
			remain := js[j][min(used, len(js[j])):]
			if strings.HasPrefix(remain, p) && len(remain) > len(p) {
				l.Wrapped = true
				used += len(p)
				if used > len(js[j]) {
					used = len(js[j])
				}
			} else {
				j++
				used = 0
			}
		}
		s.Lines = append(s.Lines, l)
	}
	return s
}
func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}
func paneAgentIdentity(pid int) (string, int) {
	// Read process names only. Claude's full command line can contain credentials.
	raw, err := child(500*time.Millisecond, "", "ps", "-axo", "pid=,ppid=,comm=")
	if err != nil {
		return "", 0
	}
	type proc struct {
		pid, ppid int
		name      string
	}
	var procs []proc
	for _, line := range strings.Split(raw, "\n") {
		f := strings.Fields(line)
		if len(f) < 3 {
			continue
		}
		p, _ := strconv.Atoi(f[0])
		pp, _ := strconv.Atoi(f[1])
		procs = append(procs, proc{p, pp, filepath.Base(strings.Join(f[2:], " "))})
	}
	descendants := map[int]bool{pid: true}
	for round := 0; round < 16; round++ {
		changed := false
		for _, p := range procs {
			if descendants[p.ppid] && !descendants[p.pid] {
				descendants[p.pid] = true
				changed = true
			}
		}
		if !changed {
			break
		}
	}
	agent := ""
	identity := 0
	for _, p := range procs {
		if !descendants[p.pid] {
			continue
		}
		name := strings.TrimSuffix(p.name, ".exe")
		if name == "claude" || name == "codex" {
			if agent != "" && agent != name {
				return "", 0
			}
			agent = name
			identity = p.pid
		}
	}
	return agent, identity
}
func captureTmux(r TerminalRequest) (Screen, error) {
	meta, err := tmux(r, "", "display-message", "-p", "-t", r.Target, "#{cursor_x}\t#{cursor_y}\t#{pane_pid}\t#{pane_in_mode}\t#{pane_width}")
	if err != nil {
		return Screen{}, err
	}
	f := strings.Split(strings.TrimSpace(meta), "\t")
	if len(f) != 5 || f[3] != "0" {
		return Screen{}, errors.New("tmux pane is not in its input mode")
	}
	x, _ := strconv.Atoi(f[0])
	y, _ := strconv.Atoi(f[1])
	pid, _ := strconv.Atoi(f[2])
	agent, identity := paneAgentIdentity(pid)
	if agent == "" {
		return Screen{}, errors.New("completion requires a Claude Code or Codex pane")
	}
	physical, err := tmux(r, "", "capture-pane", "-p", "-N", "-t", r.Target)
	if err != nil {
		return Screen{}, err
	}
	joined, err := tmux(r, "", "capture-pane", "-p", "-J", "-t", r.Target)
	if err != nil {
		return Screen{}, err
	}
	s := tmuxScreen(physical, joined, agent, x, y)
	s.ProcessID = identity
	s.Columns, _ = strconv.Atoi(f[4])
	snapshot := r
	snapshot.Screen = s
	configureVim(&snapshot)
	return snapshot.Screen, nil
}
func prepareTmux(r *TerminalRequest) error {
	s, err := captureTmux(*r)
	if err != nil {
		return err
	}
	r.Screen = s
	cwd, _ := tmux(*r, "", "display-message", "-p", "-t", r.Target, "#{pane_current_path}")
	r.Cwd = strings.TrimSpace(cwd)
	panes, _ := tmux(*r, "", "list-panes", "-t", r.Target, "-F", "#{pane_id}")
	for _, id := range strings.Fields(panes) {
		if id != r.Target {
			text, _ := tmux(*r, "", "capture-pane", "-p", "-J", "-t", id)
			r.Others = append(r.Others, text)
		}
	}
	return nil
}
func insertTmux(r TerminalRequest, text string, backspaces int) error {
	if backspaces > 0 {
		args := []string{"send-keys", "-t", r.Target}
		for i := 0; i < backspaces; i++ {
			args = append(args, "BSpace")
		}
		if _, err := tmux(r, "", args...); err != nil {
			return err
		}
	}
	if text == "" {
		return nil
	}
	// Use a private tmux buffer, so prompt text never appears in send-keys argv.
	name := "llm-complete-" + fmtInt(int(time.Now().UnixNano()))
	if _, err := tmux(r, sanitise(text), "load-buffer", "-b", name, "-"); err != nil {
		return err
	}
	_, err := tmux(r, "", "paste-buffer", "-d", "-b", name, "-t", r.Target)
	return err
}
