package main

import (
	"context"
	"crypto/sha256"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"time"
)

type TerminalRequest struct {
	Others   []string `json:"others"`
	Screen   Screen   `json:"screen"`
	Socket   string   `json:"socket"`
	Target   string   `json:"target"`
	KittyPID int      `json:"kitty_pid"`
	Source   string   `json:"source"`
	Cwd      string   `json:"cwd"`
	Kitten   string   `json:"kitten"`
}

func child(timeout time.Duration, input string, name string, args ...string) (string, error) {
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	cmd := exec.CommandContext(ctx, name, args...)
	cmd.Stdin = strings.NewReader(input)
	var out, errout strings.Builder
	cmd.Stdout = &out
	cmd.Stderr = &errout
	if err := cmd.Run(); err != nil {
		if ctx.Err() != nil {
			return "", errors.New("completion helper timed out")
		}
		return "", fmt.Errorf("%s: %s", name, strings.TrimSpace(errout.String()))
	}
	return out.String(), nil
}
func kitty(r TerminalRequest, input string, args ...string) (string, error) {
	return child(3*time.Second, input, "kitten", append([]string{"@", "--to", r.Socket}, args...)...)
}

var cursorRE = regexp.MustCompile(`\x1b\[(\d+);(\d+)H`)

func kittyScreen(text, agent string) (Screen, error) {
	s := Screen{Agent: agent}
	ms := cursorRE.FindAllStringSubmatchIndex(text, -1)
	if len(ms) == 0 {
		return s, errors.New("kitty returned no caret")
	}
	m := ms[len(ms)-1]
	y, _ := strconv.Atoi(text[m[2]:m[3]])
	x, _ := strconv.Atoi(text[m[4]:m[5]])
	s.Y = y - 1
	s.X = x - 1
	text = text[:m[0]]
	if i := strings.Index(text, "\x1b[?25"); i >= 0 {
		text = text[:i]
	}
	// Kitty emits CR at each physical row, followed by LF only at hard
	// breaks. A soft wrap is a bare CR, not CR+LF. Preserve physical rows
	// so cursor coordinates agree with the native kitten's snapshot.
	if strings.Contains(text, "\r") {
		for len(text) > 0 {
			i := strings.IndexByte(text, '\r')
			if i < 0 {
				s.Lines = append(s.Lines, Line{Text: text})
				break
			}
			l := Line{Text: text[:i], Wrapped: true}
			text = text[i+1:]
			if strings.HasPrefix(text, "\n") {
				l.Wrapped = false
				text = text[1:]
			}
			s.Lines = append(s.Lines, l)
		}
	} else {
		for _, l := range strings.Split(strings.TrimSuffix(text, "\n"), "\n") {
			s.Lines = append(s.Lines, Line{Text: l})
		}
	}
	return s, nil
}
func capture(r TerminalRequest) (Screen, error) {
	if r.Source == "tmux" {
		return captureTmux(r)
	}
	raw, err := kitty(r, "", "get-text", "--match", "id:"+r.Target, "--add-cursor", "--add-wrap-markers")
	if err != nil {
		return Screen{}, err
	}
	agent, pid, columns, err := kittyAgent(r)
	if err != nil {
		return Screen{}, err
	}
	s, err := kittyScreen(raw, agent)
	s.ProcessID = pid
	s.Columns = columns
	snapshot := r
	snapshot.Screen = s
	configureVim(&snapshot)
	return snapshot.Screen, err
}
func notify(r TerminalRequest, msg string) {
	if r.Source == "tmux" {
		_, err := tmux(r, "", "display-message", "-t", r.Target, sanitise(msg))
		if err != nil {
			fmt.Fprintln(os.Stderr, "llm_complete:", sanitise(msg))
		}
		return
	}
	// The no-UI status handler opens kitty's existing error overlay.
	_, err := kitty(r, "", "kitten", "--match", "id:"+r.Target, r.Kitten, "status", sanitise(msg))
	if err != nil {
		fmt.Fprintln(os.Stderr, "llm_complete:", sanitise(msg))
	}
}
func privateDir(dir string) error {
	if err := os.MkdirAll(dir, 0700); err != nil {
		return err
	}
	return os.Chmod(dir, 0700)
}
func stateDir() string {
	base := os.Getenv("XDG_STATE_HOME")
	if base == "" {
		h, _ := os.UserHomeDir()
		base = filepath.Join(h, ".local/state")
	}
	return filepath.Join(base, "llm_complete")
}
func targetFile(r TerminalRequest) string {
	hash := sha256.Sum256([]byte(r.Source + "\x00" + r.Socket + "\x00" + r.Target))
	return filepath.Join(stateDir(), fmt.Sprintf("%x.json", hash[:12]))
}
func writePrivate(name string, v any) error {
	if err := privateDir(filepath.Dir(name)); err != nil {
		return err
	}
	data, err := json.Marshal(v)
	if err != nil {
		return err
	}
	f, err := os.CreateTemp(filepath.Dir(name), ".completion-*")
	if err != nil {
		return err
	}
	defer os.Remove(f.Name())
	if _, err = f.Write(data); err != nil {
		f.Close()
		return err
	}
	if err = f.Close(); err != nil {
		return err
	}
	return os.Rename(f.Name(), name)
}
func sameInput(a, b Screen) bool {
	ia, ea := extract(a)
	ib, eb := extract(b)
	return ea == nil && eb == nil && a.Agent == b.Agent && a.ProcessID == b.ProcessID && ia.Prefix == ib.Prefix && ia.Suffix == ib.Suffix
}
func insert(r TerminalRequest, text string, backspaces int) error {
	if r.Source == "tmux" {
		return insertTmux(r, text, backspaces)
	}
	if backspaces > 0 {
		keys := []string{"send-key", "--match", "id:" + r.Target}
		for i := 0; i < backspaces; i++ {
			keys = append(keys, "backspace")
		}
		if _, err := kitty(r, "", keys...); err != nil {
			return err
		}
	}
	if text == "" {
		return nil
	}
	_, err := kitty(r, sanitise(text), "send-text", "--match", "id:"+r.Target, "--stdin")
	return err
}
func terminalDabbrev(r TerminalRequest) error {
	defer warmContext(r)
	unlock, err := targetLock(r)
	if err != nil {
		return err
	}
	defer func() { unlock() }()
	filename := targetFile(r)
	var old Cycle
	if b, err := os.ReadFile(filename); err == nil {
		_ = json.Unmarshal(b, &old)
	}
	e, err := expand(r.Screen, terminalCorpora(r), old)
	if err != nil {
		return err
	}
	current, err := capture(r)
	if err != nil {
		return err
	}
	if !sameInput(r.Screen, current) {
		return errors.New("prompt changed, discarded expansion")
	}
	if err = staleTicket(r); err != nil {
		return err
	}
	if err = insert(r, e.Text, e.Backspaces); err != nil {
		return err
	}
	if err = writePrivate(filename, e.State); err != nil {
		return err
	}
	unlock()
	unlock = func() {}
	return nil
}

func kittyAgent(r TerminalRequest) (string, int, int, error) {
	raw, err := kitty(r, "", "ls")
	if err != nil {
		return "", 0, 0, err
	}
	var windows []struct {
		Tabs []struct {
			Windows []struct {
				ID         int `json:"id"`
				Columns    int `json:"columns"`
				Foreground []struct {
					PID     int      `json:"pid"`
					Cmdline []string `json:"cmdline"`
				} `json:"foreground_processes"`
			} `json:"windows"`
		} `json:"tabs"`
	}
	if json.Unmarshal([]byte(raw), &windows) != nil {
		return "", 0, 0, errors.New("cannot verify kitty foreground process")
	}
	for _, oswin := range windows {
		for _, tab := range oswin.Tabs {
			for _, w := range tab.Windows {
				if strconv.Itoa(w.ID) != r.Target {
					continue
				}
				for _, p := range w.Foreground {
					for i, arg := range p.Cmdline {
						if i >= 3 {
							break
						}
						name := filepath.Base(arg)
						if name == "codex" {
							return "codex", p.PID, w.Columns, nil
						}
						if name == "claude" || name == "claude.exe" || strings.HasPrefix(name, "claude-") {
							return "claude", p.PID, w.Columns, nil
						}
					}
				}
			}
		}
	}
	return "", 0, 0, errors.New("agent foreground process changed")
}
