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
	for _, l := range strings.Split(strings.TrimSuffix(text, "\n"), "\n") {
		s.Lines = append(s.Lines, Line{Text: strings.TrimSuffix(l, "\r"), Wrapped: strings.HasSuffix(l, "\r")})
	}
	return s, nil
}
func capture(r TerminalRequest) (Screen, error) {
	raw, err := kitty(r, "", "get-text", "--match", "id:"+r.Target, "--add-cursor", "--add-wrap-markers")
	if err != nil {
		return Screen{}, err
	}
	return kittyScreen(raw, r.Screen.Agent)
}
func notify(r TerminalRequest, msg string) {
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
	return ea == nil && eb == nil && a.Agent == b.Agent && ia.Prefix == ib.Prefix && ia.Suffix == ib.Suffix
}
func insert(r TerminalRequest, text string, backspaces int) error {
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
	if err = insert(r, e.Text, e.Backspaces); err != nil {
		return err
	}
	if err = writePrivate(filename, e.State); err != nil {
		return err
	}
	loadContext(r)
	return nil
}
