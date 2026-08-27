package main

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

// ** output primitives

func (r *renderer) heading(level int, text string) {
	r.taggedHeading(tagSub, level, text)
}

// The turn's own heading. Message text nests below the sub-headings this file
// emits for the turn's parts, so the tag records which heading is the turn.
func (r *renderer) turnHeading(level int, text string) {
	r.taggedHeading(tagTurn, level, text)
}

func (r *renderer) taggedHeading(kind byte, level int, text string) {
	mark := "#"
	if r.org {
		mark = "*"
	}
	level += r.base

	marks := level
	if marks > 6 && !r.org {
		// Markdown stops at six; org does not care.
		marks = 6
	}
	if r.tag {
		// The tag carries the unclamped level, so a heading deeper than markdown
		// can express is restored rather than lost.
		text = levelTag(kind, level) + " " + stripTags(text)
	}

	r.ensureBlank()
	r.out.WriteString(strings.Repeat(mark, marks) + " " + text + "\n\n")
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
	s = closeOpenFence(repairMidLineFences(s))
	if r.tag {
		// [normalizeOrgLevels] places these once pandoc has said which of them
		// are headings at all. Shifting here would only re-clamp them at six and
		// lose their relative depth, and would still miss every heading this
		// file's markdown cannot see.
		r.out.WriteString(stripTags(s) + "\n")
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
	n := longestBacktickRun(body) + 1
	if n < 3 {
		n = 3
	}
	return strings.Repeat("`", n)
}

func longestBacktickRun(s string) int {
	longest, run := 0, 0
	for _, c := range s {
		if c == '`' {
			run++
			if run > longest {
				longest = run
			}
		} else {
			run = 0
		}
	}
	return longest
}

// The body as an inline code span, delimited by a run longer than any inside it.
func inlineCode(s string) string {
	d := strings.Repeat("`", longestBacktickRun(s)+1)
	if strings.HasPrefix(s, "`") || strings.HasSuffix(s, "`") {
		// Otherwise delimiter and body run together into one longer run.
		s = " " + s + " "
	}
	return d + s + d
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

// A message that opens a fenced code block and never closes it swallows
// everything after it — the next turn's headings included — into one code
// block. It also breaks the promise every chunk is a self-contained markdown
// document, so where the seams fell would change the conversion. Close
// whatever the message left open.
func closeOpenFence(s string) string {
	open := ""
	for _, ln := range strings.Split(s, "\n") {
		open = fenceStep(open, ln)
	}
	if open == "" {
		return s
	}
	// The run alone, never the info string with it.
	return s + "\n" + open
}

// The fence a line leaves open, given the one it found open. Returns "" outside
// a fenced block and the opening run inside one.
//
// Only a bare run closes a fence: ```` ```sh ```` inside a ```` ``` ```` block
// is content, not the end of it, and reading it as the end walks the rest of
// the message one block out of step.
func fenceStep(open, ln string) string {
	m := fenceRe.FindStringSubmatch(ln)
	if m == nil {
		return open
	}
	if open == "" {
		return m[1]
	}
	if m[1][0] == open[0] && len(m[1]) >= len(open) &&
		strings.TrimSpace(ln[len(m[0]):]) == "" {
		return ""
	}
	return open
}

// A fence has to start its own line, so `I want to print ` + "```" is not one.
// CommonMark reads it as an inline code span, keeps the paragraph going, and
// then takes the *closing* run — which does start its own line — as an opening
// fence, swallowing the rest of the sentence. Code and prose come out swapped.
//
// This is what `pasteBlockified` produces whenever the paste lands anywhere but
// column 0, so it is common in the transcripts. Re-cut the block around what
// was meant: CommonMark was already going to read this text as a code block, so
// moving its boundaries cannot break a message that would otherwise have worked.
//
// A body of one line was a quotation inside a sentence, and becomes an inline
// span so the sentence survives; anything longer becomes the block it looks
// like. A run with no matching closer later in the message is left alone rather
// than guessed at.
func repairMidLineFences(s string) string {
	if !strings.Contains(s, "```") && !strings.Contains(s, "~~~") {
		return s
	}

	lines := strings.Split(s, "\n")
	out := make([]string, 0, len(lines)+2)
	open := ""

	for i := 0; i < len(lines); i++ {
		ln := lines[i]
		if open != "" || fenceRe.MatchString(ln) {
			// A well-formed fence, or a line inside one: nothing to repair.
			open = fenceStep(open, ln)
			out = append(out, ln)
			continue
		}

		run, at := trailingFenceRun(ln)
		if run == "" {
			out = append(out, ln)
			continue
		}
		prefix := strings.TrimRight(ln[:at], " \t")
		if prefix == "" {
			out = append(out, ln)
			continue
		}
		end, rest, ok := findFenceCloser(lines, i+1, run)
		if !ok {
			out = append(out, ln)
			continue
		}

		if body := oneLineBody(lines, i, end); body != "" {
			// The sentence usually resumes on the line after the closer. Pulling
			// it up too is what keeps pandoc from rendering the soft break as a
			// space, which would strand one before a leading `,`.
			tail, last := rest, end
			if strings.TrimSpace(tail) == "" && end+1 < len(lines) &&
				continuesParagraph(lines[end+1]) {
				tail, last = lines[end+1], end+1
			}
			// Fed back through rather than emitted, so a second fence opened on
			// the text just pulled up is repaired as well.
			lines[last] = prefix + " " + inlineCode(body) + tail
			i = last - 1
			continue
		}

		out = append(out, prefix, run)
		out = append(out, lines[i+1:end]...)
		out = append(out, run)
		if strings.TrimSpace(rest) == "" {
			i = end
			continue
		}
		lines[end] = rest
		i = end - 1
	}

	return strings.Join(out, "\n")
}

// The run of fence characters a line ends with, and the index it starts at.
// Returns "" unless the line ends in a run of three or more.
func trailingFenceRun(ln string) (string, int) {
	s := strings.TrimRight(ln, " \t")
	if s == "" {
		return "", -1
	}
	c := s[len(s)-1]
	if c != '`' && c != '~' {
		return "", -1
	}
	i := len(s)
	for i > 0 && s[i-1] == c {
		i--
	}
	if len(s)-i < 3 {
		return "", -1
	}
	return s[i:], i
}

// The first line from `from` on that opens with a run able to close `run`, and
// whatever it carries after that run.
func findFenceCloser(lines []string, from int, run string) (int, string, bool) {
	for j := from; j < len(lines); j++ {
		m := fenceRe.FindStringSubmatch(lines[j])
		if m != nil && m[1][0] == run[0] && len(m[1]) >= len(run) {
			return j, lines[j][len(m[0]):], true
		}
	}
	return 0, "", false
}

// The single line between the fences, or "" if there is not exactly one.
func oneLineBody(lines []string, open, end int) string {
	if end != open+2 {
		return ""
	}
	return strings.TrimSpace(lines[open+1])
}

// A line that starts a block of its own — list item, heading, quote, fence,
// thematic break, setext underline — is not a paragraph continuation and must
// not be pulled up into an inlined sentence.
var blockStartRe = regexp.MustCompile(
	"^ {0,3}(?:" +
		"[-*+](?:[ \t]|$)" +
		"|[0-9]{1,9}[.)](?:[ \t]|$)" +
		"|#{1,6}(?:[ \t]|$)" +
		"|>" +
		"|`{3,}|~{3,}" +
		"|(?:=+|-{3,}|\\*{3,}|_{3,})[ \t]*$" +
		")")

func continuesParagraph(ln string) bool {
	return strings.TrimSpace(ln) != "" && !blockStartRe.MatchString(ln)
}

// Calls fn for every line outside a fenced code block.
func forEachMarkdownLine(lines []string, fn func(i int, ln string)) {
	open := ""
	for i, ln := range lines {
		// A fence line, or any line inside a block: neither is a heading.
		if open != "" || fenceRe.MatchString(ln) {
			open = fenceStep(open, ln)
			continue
		}
		fn(i, ln)
	}
}

// ** heading tags
//
// On the org-pandoc path the skeleton is emitted as markdown and *pandoc*
// decides what a heading is, so by the time the document is org there is
// nothing left to tell this program's headings from a message's. Each
// structural heading therefore carries a tag: two private-use runes around a
// kind letter and the heading's true level. They mean nothing to markdown,
// survive pandoc untouched, and [normalizeOrgLevels] strips them again.
//
// The alternative — teaching the markdown side every construct pandoc's reader
// promotes to a heading — is the reimplementation `readme.org` argues against
// under Performance, and it had already gone wrong: setext underlines
// (`Title` over `====`) were invisible to [shiftHeadings], so a message full of
// them escaped the outline entirely.
const (
	tagOpen  = ''
	tagClose = ''

	tagTurn = 'T'
	tagSub  = 'S'
)

var tagStripper = strings.NewReplacer(string(tagOpen), "", string(tagClose), "")

func levelTag(kind byte, level int) string {
	return string(tagOpen) + string(kind) + strconv.Itoa(level) + string(tagClose)
}

// Keeps a message from forging a tag of its own.
func stripTags(s string) string {
	if !strings.ContainsRune(s, tagOpen) && !strings.ContainsRune(s, tagClose) {
		return s
	}
	return tagStripper.Replace(s)
}

var orgHeadingRe = regexp.MustCompile(`^(\*+) `)
var orgBlockRe = regexp.MustCompile(`(?i)^[ \t]*#\+(begin|end)_`)
var headingTagRe = regexp.MustCompile("^([TS])([0-9]+) ?")

// Where message headings start, relative to their turn: below the sub-headings
// the turn is made of (`Tool Use`, `Thinking`, `Recap` …), which sit one level
// under it. So a message can never be mistaken for the transcript's own
// structure.
const contentDepth = 2

type orgHeading struct {
	line  int
	stars int
	text  string
	// The tag's kind letter, or 0 for a heading that came out of a message.
	kind byte
	// The tagged heading's true level.
	level int
}

// Restores heading levels in pandoc's org output: tagged headings go back to
// the level they were emitted at, and everything else — which by definition
// came out of a message — is demoted to sit under its turn.
//
// This is only possible on the org side. A `*` at the start of a line in
// pandoc's org output is always a heading: the writer zero-width-space-escapes
// prose that org would misread (`#+TITLE:`, a literal `* ` line) and
// comma-escapes block interiors, so there are no false positives to sort out.
func normalizeOrgLevels(doc string) string {
	lines := strings.Split(doc, "\n")
	heads := scanOrgHeadings(lines)

	set := func(h orgHeading, level int) {
		if level < 1 {
			level = 1
		}
		lines[h.line] = strings.Repeat("*", level) + " " + h.text
	}

	contentBase := 0
	for i := 0; i < len(heads); {
		if h := heads[i]; h.kind != 0 {
			set(h, h.level)
			switch {
			case h.kind == tagTurn:
				contentBase = h.level + contentDepth
			case contentBase == 0:
				// The `* Subagents` skeleton, before any turn.
				contentBase = h.level + 1
			}
			i++
			continue
		}

		// This heading and every untagged one after it came out of the same
		// message, so they shift as a unit and keep their relative depths.
		j, min := i, 0
		for ; j < len(heads) && heads[j].kind == 0; j++ {
			if min == 0 || heads[j].stars < min {
				min = heads[j].stars
			}
		}
		base := contentBase
		if base == 0 {
			base = min
		}
		for ; i < j; i++ {
			set(heads[i], heads[i].stars+base-min)
		}
	}

	return strings.Join(lines, "\n")
}

func scanOrgHeadings(lines []string) []orgHeading {
	var out []orgHeading
	depth := 0

	for i, ln := range lines {
		if m := orgBlockRe.FindStringSubmatch(ln); m != nil {
			if strings.EqualFold(m[1], "begin") {
				depth++
			} else if depth > 0 {
				depth--
			}
			continue
		}
		if depth > 0 {
			continue
		}
		m := orgHeadingRe.FindStringSubmatch(ln)
		if m == nil {
			continue
		}

		h := orgHeading{line: i, stars: len(m[1]), text: ln[len(m[0]):]}
		if t := headingTagRe.FindStringSubmatch(h.text); t != nil {
			h.kind = t[1][0]
			h.level, _ = strconv.Atoi(t[2])
			if h.level < 1 {
				h.level = 1
			}
			h.text = h.text[len(t[0]):]
		}
		out = append(out, h)
	}

	return out
}

// A bare URL, as a link in whichever syntax is being written.
func (r *renderer) link(url string) {
	r.ensureBlank()
	if r.org {
		r.out.WriteString("[[" + url + "]]\n")
		return
	}
	r.out.WriteString("<" + url + ">\n")
}
