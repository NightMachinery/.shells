package main

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
)

// Recent Claude versions show INSERT but no NORMAL label. Read only the
// editorMode field (never credentials) to distinguish that from a non-vim CLI.
func claudeVim(cwd string) bool {
	home, _ := os.UserHomeDir()
	base := os.Getenv("CLAUDE_CONFIG_DIR")
	if base == "" {
		base = filepath.Join(home, ".claude")
	}
	paths := []string{filepath.Join(base, "settings.json")}
	var parents []string
	for dir := cwd; dir != "" && dir != filepath.Dir(dir); dir = filepath.Dir(dir) {
		parents = append(parents, dir)
	}
	for i := len(parents) - 1; i >= 0; i-- {
		paths = append(paths, filepath.Join(parents[i], ".claude/settings.json"), filepath.Join(parents[i], ".claude/settings.local.json"))
	}
	vim := false
	for _, path := range paths {
		f, err := os.Open(path)
		if err != nil {
			continue
		}
		var s struct {
			Mode string `json:"editorMode"`
		}
		err = json.NewDecoder(f).Decode(&s)
		f.Close()
		if err == nil && (s.Mode == "vim" || s.Mode == "normal") {
			vim = s.Mode == "vim"
		}
	}
	return vim
}
func configureVim(r *TerminalRequest) {
	if r.Screen.Agent != "claude" {
		return
	}
	r.Screen.Vim = claudeVim(r.Cwd)
	for _, line := range r.Screen.Lines {
		if strings.Contains(line.Text, "-- INSERT --") || strings.Contains(line.Text, "-- NORMAL --") || strings.Contains(line.Text, "-- VISUAL --") {
			r.Screen.Vim = true
		}
	}
}
