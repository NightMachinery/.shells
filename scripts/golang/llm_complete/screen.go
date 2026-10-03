package main

import (
	"errors"
	"strings"
	"unicode"
)

// Lines retain physical rows and the soft-wrap bit. Cursor coordinates are cells.
type Line struct {
	Text    string `json:"text"`
	Wrapped bool   `json:"wrapped"`
}
type Screen struct {
	Lines     []Line `json:"lines"`
	X         int    `json:"cursor_x"`
	Y         int    `json:"cursor_y"`
	Agent     string `json:"agent"`
	ProcessID int    `json:"process_id,omitempty"`
	Vim       bool   `json:"vim,omitempty"`
}
type Input struct {
	Prefix string `json:"prefix"`
	Suffix string `json:"suffix"`
	Above  string `json:"above"`
}

func sanitise(s string) string {
	return strings.Map(func(r rune) rune {
		if unicode.IsControl(r) || r == '\u2028' || r == '\u2029' {
			return -1
		}
		return r
	}, s)
}
func firstChars(s string, n int) string {
	r := []rune(s)
	if n < 0 {
		n = 0
	}
	if len(r) > n {
		r = r[:n]
	}
	return string(r)
}
func lastChars(s string, n int) string {
	r := []rune(s)
	if n < 0 {
		n = 0
	}
	if len(r) > n {
		r = r[len(r)-n:]
	}
	return string(r)
}
func cellWidth(r rune) int {
	if unicode.Is(unicode.Mn, r) || unicode.Is(unicode.Me, r) || r == '\u200d' {
		return 0
	}
	if r >= 0x1100 && (r <= 0x115f || r == 0x2329 || r == 0x232a || r >= 0x2e80 && r <= 0xa4cf || r >= 0xac00 && r <= 0xd7a3 || r >= 0xf900 && r <= 0xfaff || r >= 0xfe10 && r <= 0xfe6f || r >= 0xff00 && r <= 0xff60 || r >= 0x1f300 && r <= 0x1faff || r >= 0x20000) {
		return 2
	}
	return 1
}
func byteAtCell(s string, x int) (int, error) {
	if x < 0 {
		return 0, errors.New("caret is outside the input")
	}
	col := 0
	for i, r := range s {
		if col == x && cellWidth(r) > 0 {
			return i, nil
		}
		col += cellWidth(r)
		if col > x {
			return 0, errors.New("caret splits a display character")
		}
	}
	if col == x {
		return len(s), nil
	}
	return 0, errors.New("caret exceeds captured text")
}
func border(s string) bool {
	s = strings.TrimSpace(s)
	if s == "" {
		return false
	}
	for _, r := range s {
		if !strings.ContainsRune("─━╭╮╰╯┌┐└┘│┃ ", r) {
			return false
		}
	}
	return true
}
func marker(s string, agent string) int {
	// Offset in bytes, preserving indentation after the prompt's separator.
	t := strings.TrimLeft(s, " │┃")
	m := "❯"
	if agent == "codex" {
		m = "›"
	}
	if !strings.HasPrefix(t, m) {
		return -1
	}
	i := len(s) - len(t) + len(m)
	if strings.HasPrefix(s[i:], " ") {
		i++
	} else if strings.HasPrefix(s[i:], "\u00a0") {
		i += len("\u00a0")
	}
	return i
}
func extract(s Screen) (Input, error) {
	var out Input
	if s.Agent != "claude" && s.Agent != "codex" {
		return out, errors.New("completion requires Claude Code or Codex")
	}
	if s.Y < 0 || s.Y >= len(s.Lines) {
		return out, errors.New("caret is outside the captured screen")
	}
	// Only a mode indicator BELOW the caret counts, never an old assistant quote.
	if s.Agent == "claude" {
		insertMode := false
		for _, l := range s.Lines[s.Y+1:] {
			if strings.Contains(l.Text, "-- INSERT --") {
				insertMode = true
			}
			if strings.Contains(l.Text, "-- NORMAL --") || strings.Contains(l.Text, "-- VISUAL --") {
				return out, errors.New("Claude vim mode: enter INSERT mode first")
			}
		}
		if s.Vim && !insertMode {
			return out, errors.New("Claude vim mode: enter INSERT mode first")
		}
	}
	start := -1
	for i := s.Y; i >= 0; i-- {
		if marker(s.Lines[i].Text, s.Agent) >= 0 {
			start = i
			break
		}
		if border(s.Lines[i].Text) {
			break
		}
	}
	if start < 0 {
		return out, errors.New("cannot locate the CLI input area")
	}
	end := s.Y
	for end+1 < len(s.Lines) {
		l := s.Lines[end+1].Text
		if border(l) || strings.TrimSpace(l) == "" || strings.HasPrefix(strings.TrimSpace(l), "? for shortcuts") || strings.Contains(l, "context left") {
			break
		}
		end++
	}
	var full strings.Builder
	cursor := -1
	for i := start; i <= end; i++ {
		raw := s.Lines[i].Text
		off := 0
		if i == start {
			off = marker(raw, s.Agent)
		} else if strings.HasPrefix(raw, "  ") {
			off = 2
		}
		text := raw[off:]
		text = strings.TrimRight(text, " │┃")
		if i == s.Y {
			cut, err := byteAtCell(raw, s.X)
			if err != nil {
				return out, err
			}
			if cut < off {
				return out, errors.New("caret is before the prompt")
			}
			local := cut - off
			if local > len(text) {
				text = raw[off:cut]
			}
			cursor = full.Len() + local
		}
		full.WriteString(text)
		if i < end && !s.Lines[i].Wrapped {
			full.WriteByte('\n')
		}
	}
	if cursor < 0 || cursor > full.Len() {
		return out, errors.New("cannot locate the input caret")
	}
	out.Prefix = full.String()[:cursor]
	out.Suffix = full.String()[cursor:]
	var above []string
	for _, l := range s.Lines[:start] {
		if !border(l.Text) {
			above = append(above, l.Text)
		}
	}
	out.Above = strings.Join(above, "\n")
	return out, nil
}
