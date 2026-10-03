package main

import (
	"errors"
	"sort"
	"strings"
	"unicode"
)

func wordChar(r rune) bool {
	return unicode.IsLetter(r) || unicode.IsDigit(r) || unicode.IsMark(r) || strings.ContainsRune("/.-_~", r)
}
func fragment(s string) string {
	r := []rune(s)
	i := len(r)
	for i > 0 && wordChar(r[i-1]) {
		i--
	}
	return string(r[i:])
}

type word struct {
	text string
	at   int
}

func words(s string) []word {
	var out []word
	start := -1
	for i, r := range s {
		if wordChar(r) {
			if start < 0 {
				start = i
			}
		} else if start >= 0 {
			out = append(out, word{s[start:i], start})
			start = -1
		}
	}
	if start >= 0 {
		out = append(out, word{s[start:], start})
	}
	return out
}
func candidates(frag, screen string, caret int, corpora []string) []string {
	near := words(screen)
	sort.SliceStable(near, func(i, j int) bool { return abs(near[i].at-caret) < abs(near[j].at-caret) })
	var result []string
	seen := map[string]bool{}
	add := func(w string) {
		if len(w) > len(frag) && strings.HasPrefix(w, frag) && !seen[w] {
			seen[w] = true
			result = append(result, w)
		}
	}
	for _, w := range near {
		add(w.text)
	}
	for _, c := range corpora {
		for _, w := range words(c) {
			add(w.text)
		}
	}
	return result
}
func abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}
func ascii(s string) bool {
	for _, r := range s {
		if r > 127 {
			return false
		}
	}
	return true
}

type Cycle struct {
	Prefix     string   `json:"prefix"`
	Suffix     string   `json:"suffix"`
	Fragment   string   `json:"fragment"`
	Candidates []string `json:"candidates"`
	Index      int      `json:"index"`
	Inserted   string   `json:"inserted"`
	Agent      string   `json:"agent"`
	ProcessID  int      `json:"process_id,omitempty"`
}
type Expansion struct {
	Text       string `json:"text"`
	Backspaces int    `json:"backspaces"`
	State      Cycle  `json:"state"`
}

func validCycle(c Cycle, in Input, agent string) bool {
	return c.Agent == agent && c.Prefix+c.Inserted == in.Prefix && c.Suffix == in.Suffix && len(c.Candidates) > 0 && c.Index >= 0 && c.Index < len(c.Candidates) && c.Inserted == strings.TrimPrefix(c.Candidates[c.Index], c.Fragment) && ascii(c.Inserted) && c.Fragment == fragment(c.Prefix) && strings.HasPrefix(c.Candidates[c.Index], c.Fragment) && sanitise(c.Inserted) == c.Inserted
}
func expand(s Screen, corpora []string, old Cycle) (Expansion, error) {
	var e Expansion
	in, err := extract(s)
	if err != nil {
		return e, err
	}
	var c Cycle
	if validCycle(old, in, s.Agent) && old.ProcessID == s.ProcessID {
		c = old
		c.Index = (c.Index + 1) % len(c.Candidates)
		e.Backspaces = len(c.Inserted)
	} else {
		f := fragment(in.Prefix)
		if f == "" {
			return e, errors.New("no word fragment before the caret")
		}
		var text strings.Builder
		caret := 0
		for i, l := range s.Lines {
			if i == s.Y {
				cut, err := byteAtCell(l.Text, s.X)
				if err != nil {
					return e, err
				}
				caret = text.Len() + cut
			}
			text.WriteString(l.Text)
			text.WriteByte('\n')
		}
		cs := candidates(f, text.String(), caret, corpora)
		if len(cs) == 0 {
			return e, errors.New("no dabbrev candidates")
		}
		// A non-ASCII remainder can expand once, but must never be backspaced by cycling.
		c = Cycle{Prefix: in.Prefix, Suffix: in.Suffix, Fragment: f, Candidates: cs, Agent: s.Agent, ProcessID: s.ProcessID}
	}
	if e.Backspaces > 0 { // Skip candidates whose deletion count is unverified.
		for n := 0; n < len(c.Candidates) && !ascii(strings.TrimPrefix(c.Candidates[c.Index], c.Fragment)); n++ {
			c.Index = (c.Index + 1) % len(c.Candidates)
		}
	}
	c.Inserted = sanitise(strings.TrimPrefix(c.Candidates[c.Index], c.Fragment))
	e.Text = c.Inserted
	e.State = c
	return e, nil
}
