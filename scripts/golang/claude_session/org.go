package main

import (
	"fmt"
	"regexp"
	"strings"
)

// ** output primitives

func (r *renderer) heading(level int, text string) {
	mark := "#"
	if r.org {
		mark = "*"
	}
	level += r.base
	if level > 6 && !r.org {
		// Markdown stops at six; org does not care.
		level = 6
	}
	r.ensureBlank()
	r.out.WriteString(strings.Repeat(mark, level) + " " + text + "\n\n")
}

// A `**key:**` line introducing the block that follows.
func (r *renderer) label(key string) {
	r.ensureBlank()
	if r.org {
		r.out.WriteString("*" + key + ":*\n")
		return
	}
	r.out.WriteString("**" + key + ":**\n")
}

// Ends the current line and leaves exactly one blank line behind it, so the
// next construct starts its own markdown block.
func (r *renderer) ensureBlank() {
	s := r.out.String()
	switch {
	case s == "" || strings.HasSuffix(s, "\n\n"):
	case strings.HasSuffix(s, "\n"):
		r.out.WriteString("\n")
	default:
		r.out.WriteString("\n\n")
	}
}

func (r *renderer) bullet(key, val string) {
	if r.org {
		r.out.WriteString("- " + key + " :: =" + val + "=\n")
		return
	}
	if strings.ContainsAny(val, "`\n") {
		r.out.WriteString("- **" + key + "**: " + oneLine(val) + "\n")
		return
	}
	r.out.WriteString("- **" + key + "**: `" + val + "`\n")
}

// Markdown prose. Its own headings are shifted so they nest strictly under
// the enclosing heading instead of escaping it. The org writer has no
// markdown to convert, so it only defuses lines that org would misread.
func (r *renderer) prose(s string, parentLevel int) {
	if s == "" {
		return
	}
	r.ensureBlank()
	if r.org {
		r.out.WriteString(escOrgText(s) + "\n")
		return
	}
	r.out.WriteString(shiftHeadings(s, parentLevel+r.base+1) + "\n")
}

func (r *renderer) block(lang, body string) {
	// The closing fence supplies the final newline; keeping the body's would
	// leave a blank line that pandoc drops, splitting the two paths.
	body = strings.TrimRight(r.elide(body), "\n")
	// A fence glued to a preceding bullet would be swallowed by the list.
	r.ensureBlank()

	if r.org {
		r.out.WriteString(orgBlock(lang, body) + "\n")
		return
	}

	fence := fenceFor(body)
	r.out.WriteString(fence + lang + "\n" + body + "\n" + fence + "\n\n")
}

// Matches what pandoc's org writer emits for a fenced code block, including
// its comma-escaping of lines org would otherwise read as structure.
func orgBlock(lang, body string) string {
	if lang == "" {
		return "#+begin_example\n" + escOrgBlock(body) + "\n#+end_example"
	}
	return "#+begin_src " + lang + "\n" + escOrgBlock(body) + "\n#+end_src"
}

func (r *renderer) elide(body string) string {
	if r.maxBlock <= 0 {
		return body
	}
	lines := strings.Split(body, "\n")
	if len(lines) <= r.maxBlock {
		return body
	}
	return strings.Join(lines[:r.maxBlock], "\n") +
		fmt.Sprintf("\n… [%d lines elided]", len(lines)-r.maxBlock)
}

// A fence longer than any backtick run inside the body.
func fenceFor(body string) string {
	longest, run := 0, 0
	for _, c := range body {
		if c == '`' {
			run++
			if run > longest {
				longest = run
			}
		} else {
			run = 0
		}
	}
	n := longest + 1
	if n < 3 {
		n = 3
	}
	return strings.Repeat("`", n)
}

// Org would read `*` or `#+` at the start of a block line as structure, so it
// is escaped with a comma. The comma goes after any indentation, which is
// where pandoc's org writer puts it.
func escOrgBlock(s string) string {
	lines := strings.Split(s, "\n")
	for i, ln := range lines {
		indent := len(ln) - len(strings.TrimLeft(ln, " \t"))
		rest := ln[indent:]
		if strings.HasPrefix(rest, "*") || strings.HasPrefix(rest, "#+") {
			lines[i] = ln[:indent] + "," + rest
		}
	}
	return strings.Join(lines, "\n")
}

func escOrgText(s string) string {
	lines := strings.Split(s, "\n")
	for i, ln := range lines {
		if strings.HasPrefix(ln, "*") || strings.HasPrefix(ln, "#+") {
			lines[i] = " " + ln
		}
	}
	return strings.Join(lines, "\n")
}

// ** markdown heading levels

var fenceRe = regexp.MustCompile("^ {0,3}(`{3,}|~{3,})")
var headingRe = regexp.MustCompile(`^(#{1,6})(\s|$)`)

// Renumbers the headings of a markdown fragment so its shallowest heading
// sits at minLevel, keeping their relative depths. Mirrors
// [agfi:org-header-rm-shared-level] followed by [agfi:org-header-indent],
// but skips fenced code, where a `# comment` is not a heading.
func shiftHeadings(text string, minLevel int) string {
	lines := strings.Split(text, "\n")

	found := 0
	forEachMarkdownLine(lines, func(_ int, ln string) {
		if m := headingRe.FindStringSubmatch(ln); m != nil {
			if lv := len(m[1]); found == 0 || lv < found {
				found = lv
			}
		}
	})
	if found == 0 {
		return text
	}

	delta := minLevel - found
	if delta == 0 {
		return text
	}

	forEachMarkdownLine(lines, func(i int, ln string) {
		m := headingRe.FindStringSubmatch(ln)
		if m == nil {
			return
		}
		lv := len(m[1]) + delta
		if lv < 1 {
			lv = 1
		}
		if lv > 6 {
			lv = 6
		}
		lines[i] = strings.Repeat("#", lv) + ln[len(m[1]):]
	})

	return strings.Join(lines, "\n")
}

// Calls fn for every line outside a fenced code block.
func forEachMarkdownLine(lines []string, fn func(i int, ln string)) {
	inFence := false
	var fenceChar byte
	fenceLen := 0

	for i, ln := range lines {
		if m := fenceRe.FindStringSubmatch(ln); m != nil {
			mark := m[1]
			if !inFence {
				inFence, fenceChar, fenceLen = true, mark[0], len(mark)
			} else if mark[0] == fenceChar && len(mark) >= fenceLen {
				inFence = false
			}
			continue
		}
		if inFence {
			continue
		}
		fn(i, ln)
	}
}
