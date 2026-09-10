// Package turns is the agent-neutral half of the renderer: a document model of
// turns, blocks and tool results that every adapter (Claude Code, Codex,
// Antigravity) builds, and the markdown/org writer that turns it into a
// transcript. Nothing here knows how any agent stores its sessions; the
// adapters under internal/<agent> translate their records into a Document and
// hand it over.
package turns

import (
	"encoding/json"
	"fmt"
	"os/exec"
	"regexp"
	"strings"
	"sync"
	"time"
)

const (
	OrgStamp  = "[2006-01-02 Mon 15:04]"
	ListStamp = "2006-01-02 15:04"
)

// A Document is one session, ready to render: the conversation's turns, the
// tool results keyed by the call they answer, and the transcripts of the
// subagents it spawned.
type Document struct {
	Turns   []Turn
	Results map[string]ToolResult
	// The `* Subagents` section at the top of the document, in the order the
	// agents were launched.
	Subagents []Subdoc
}

// A Subdoc is a subagent's transcript, nested under its own heading.
type Subdoc struct {
	Title   string
	Turns   []Turn
	Results map[string]ToolResult
}

// One conversational turn: the consecutive records that share a role, flattened
// into the blocks they contain.
type Turn struct {
	Role   string
	TS     string
	Model  string
	Blocks []TimedBlock

	// Overrides the role-derived heading, for turns that are an event rather
	// than somebody speaking.
	Heading string
	// Trailing detail, after the timestamp.
	Note string
	// How long the turn took, when the transcript says so.
	Duration time.Duration
	// Open the turn closed in emacs: for a turn that is bulk rather than
	// conversation, such as the instructions a session was launched with.
	Folded bool
}

// A Block is one piece of a turn. The vocabulary is the renderer's, not any
// agent's: `text`, `thinking`, `tool_use` (Name + Input, with ID for its
// result), `tool_result` (an orphan whose call is not in the document),
// `notice` (Name is the kind, Text the body, rendered as prose), `event` (the
// same, with the body kept verbatim in a block), `command`, `pr`, `tasks` and
// `file-edit`. The JSON tags match the Anthropic content-block schema so the
// Claude adapter can decode into it directly; other adapters fill the fields
// by hand.
type Block struct {
	Type      string          `json:"type"`
	Text      string          `json:"text"`
	Thinking  string          `json:"thinking"`
	Name      string          `json:"name"`
	Input     json.RawMessage `json:"input"`
	Content   json.RawMessage `json:"content"`
	IsError   bool            `json:"is_error"`
	ID        string          `json:"id"`
	ToolUseID string          `json:"tool_use_id"`
}

// A block plus the timestamp of the record it arrived in, which within a turn
// is not necessarily the turn's own.
type TimedBlock struct {
	B  Block
	TS string
}

// A tool result, keyed elsewhere by the id of the call it answers.
type ToolResult struct {
	Body    string
	IsError bool
	TS      string
	// The body's language, when the adapter knows it: `json` for a result it
	// pretty-printed, empty for output whose syntax nobody can vouch for.
	Lang string
}

// Options for Render.
type Options struct {
	// `md`, `org` or `org-pandoc`. `md` is meant to be piped through pandoc;
	// `org-pandoc` does that here, in parallel chunks; `org` writes org
	// directly and leaves message bodies as markdown.
	Format string
	// Elide code blocks longer than this many lines; 0 never elides.
	MaxBlock int
	// Render Edit-style calls (old_string/new_string) as a unified diff.
	Diff bool
	// Worker count for rendering and for pandoc chunks.
	Jobs int
	// The pandoc binary, for org-pandoc.
	Pandoc string
}

// Style is how one run of turns is written: the syntax, the elision and diff
// settings, and the heading offset for a transcript nested inside another
// document. Exported for the adapters' tests, which render a handful of turns
// and look at the result.
type Style struct {
	Org      bool
	MaxBlock int
	Diff     bool
	// Tag every heading with its level, for [normalizeOrgLevels] to restore
	// once pandoc has converted the document. Only the org-pandoc path needs
	// it, because only there does something other than this program decide what
	// a heading is.
	Tag bool
	// Added to every heading level.
	Base int
}

type renderer struct {
	Style
	out *strings.Builder

	// Results, keyed by the id of the call they answer, so a call can render
	// its own result underneath itself.
	results map[string]ToolResult
	// The enclosing turn's timestamp; sub-headings only show theirs when it
	// differs.
	turnTS string
}

// Below this, splitting the document across pandoc processes costs more in
// process startup (~70ms each) than it saves.
const minPandocChunk = 96 << 10

// Render writes the document in the requested format.
func Render(doc *Document, o Options) (string, error) {
	switch o.Format {
	case "md", "org", "org-pandoc":
	default:
		return "", fmt.Errorf("unknown format: %s", o.Format)
	}
	jobs := o.Jobs
	if jobs < 1 {
		jobs = 1
	}

	st := Style{
		Org:      o.Format == "org",
		MaxBlock: o.MaxBlock,
		Diff:     o.Diff,
		Tag:      o.Format == "org-pandoc",
	}

	// The skeleton Go emits is in the *output* syntax, which for org-pandoc is
	// org even though the bodies it wraps are still markdown.
	orgOut := o.Format != "md"

	segs := subagentSegments(doc.Subagents, st, orgOut, jobs)
	for _, p := range RenderTurns(doc.Turns, doc.Results, st, jobs) {
		segs = append(segs, segment{text: p, body: true})
	}
	// Before pandoc rather than after, so no chunk carries a byte pandoc
	// would have to guess about, and before the org path returns, so every
	// format leaves a file that opens as text. See [ScrubText].
	for i := range segs {
		segs[i].text = ScrubText(segs[i].text)
	}

	var w strings.Builder
	if o.Format != "org-pandoc" {
		for _, s := range segs {
			w.WriteString(s.text)
		}
		return w.String(), nil
	}

	bin := o.Pandoc
	if bin == "" {
		bin = "pandoc"
	}
	converted, err := convertSegments(segs, jobs, bin)
	if err != nil {
		return "", err
	}
	var body strings.Builder
	for _, s := range converted {
		body.WriteString(s)
	}
	// Over the assembled document rather than per chunk, so how it was split
	// across pandoc processes cannot change the result.
	out := normalizeOrgLevels(body.String())
	return strings.TrimRight(out, "\n") + "\n", nil
}

// A piece of the output document. Message bodies are markdown and go through
// pandoc; the skeleton Go builds around them (the Subagents section and its
// headings) is already in its final syntax and must not, because pandoc has no
// way to express an org property drawer.
type segment struct {
	text string
	body bool
}

// The `* Subagents` section at the top of the document: every agent this
// session spawned. The section itself stays open so the roster is visible at a
// glance; each agent's own transcript is folded away.
func subagentSegments(subs []Subdoc, st Style, orgOut bool, jobs int) []segment {
	if len(subs) == 0 {
		return nil
	}

	mark := "#"
	if orgOut {
		mark = "*"
	}
	// Tagged like every other heading: this section is skeleton, so it never
	// meets pandoc, but [normalizeOrgLevels] still has to know it is not
	// something a message wrote.
	head := func(level int, text, trailer string) string {
		if st.Tag {
			text = levelTag(tagSub, level) + " " + stripTags(text)
		}
		return strings.Repeat(mark, level) + " " + text + "\n" + trailer + "\n"
	}

	segs := []segment{{text: head(1, "Subagents", "")}}

	// Under `* Subagents` / `** <agent>`, so the transcript starts at level 3.
	sub := st
	sub.Base = 2

	for _, s := range subs {
		trailer := ""
		if orgOut {
			// VISIBILITY is honoured at startup, so each agent opens folded.
			trailer = ":PROPERTIES:\n:VISIBILITY: folded\n:END:\n"
		}
		segs = append(segs, segment{text: head(2, s.Title, trailer)})
		for _, p := range RenderTurns(s.Turns, s.Results, sub, jobs) {
			segs = append(segs, segment{text: p, body: true})
		}
	}

	return segs
}

// RenderTurns writes each turn as its own piece of the document. Turns are
// independent, so they render concurrently and are reassembled in order.
func RenderTurns(turns []Turn, results map[string]ToolResult, st Style, jobs int) []string {
	parts := make([]string, len(turns))

	workers := jobs
	if workers > len(turns) {
		workers = len(turns)
	}
	if workers < 1 {
		return nil
	}

	var wg sync.WaitGroup
	idx := make(chan int)
	for w := 0; w < workers; w++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			r := &renderer{Style: st, results: results, out: &strings.Builder{}}
			for i := range idx {
				r.out.Reset()
				r.renderTurn(turns[i])
				parts[i] = r.out.String()
			}
		}()
	}
	for i := range turns {
		idx <- i
	}
	close(idx)
	wg.Wait()

	return parts
}

// Converts the markdown segments, leaving the skeleton ones alone. Contiguous
// runs of markdown are converted as units so pandoc never sees a document
// fragment that starts mid-structure.
func convertSegments(segs []segment, jobs int, bin string) ([]string, error) {
	out := make([]string, len(segs))

	for i := 0; i < len(segs); {
		if !segs[i].body {
			out[i] = segs[i].text
			i++
			continue
		}

		j := i
		var run []string
		for j < len(segs) && segs[j].body {
			run = append(run, segs[j].text)
			j++
		}

		chunks, err := pandocChunks(run, jobs, bin)
		if err != nil {
			return nil, err
		}
		// Trailing blank line so a skeleton heading after this run is its own
		// block; the caller trims whatever is left over at the very end.
		out[i] = strings.Join(chunks, "\n\n") + "\n\n"
		for k := i + 1; k < j; k++ {
			out[k] = ""
		}
		i = j
	}

	return out, nil
}

// Splits the rendered records into byte-balanced chunks and converts each with
// its own pandoc. Chunk seams fall on record boundaries, never inside a code
// block and never just after a heading, so each chunk is a self-contained
// markdown document and the result is identical to converting the whole thing
// at once.
func pandocChunks(parts []string, jobs int, bin string) ([]string, error) {
	total := 0
	for _, p := range parts {
		total += len(p)
	}

	n := total / minPandocChunk
	if n > jobs {
		n = jobs
	}
	if n < 1 {
		n = 1
	}

	chunks := make([]string, 0, n)
	var cur strings.Builder
	target := total / n
	for _, p := range parts {
		cur.WriteString(p)

		if cur.Len() < target || len(chunks) >= n-1 {
			continue
		}
		// A chunk must not end on a heading. An org headline is a container,
		// so pandoc writes whatever follows one with no blank line between the
		// two, while joining the chunks' outputs always puts one there --- and
		// that is the whole of the difference from a single run, measured over
		// every block type the renderer emits. Deciding it at the seam instead
		// would mean reimplementing pandoc's spacing rules, which is exactly
		// the trade the Performance section refuses; moving the seam needs to
		// know only about our own markdown.
		if endsWithMarkdownHeading(p) {
			continue
		}

		chunks = append(chunks, cur.String())
		cur.Reset()
	}
	if cur.Len() > 0 {
		chunks = append(chunks, cur.String())
	}

	out := make([]string, len(chunks))
	errs := make([]error, len(chunks))

	var wg sync.WaitGroup
	for i := range chunks {
		wg.Add(1)
		go func(i int) {
			defer wg.Done()
			out[i], errs[i] = runPandoc(bin, chunks[i])
		}(i)
	}
	wg.Wait()

	for i, err := range errs {
		if err != nil {
			return nil, fmt.Errorf("pandoc (chunk %d/%d): %v", i+1, len(chunks), err)
		}
	}

	// Only the trailing newlines are normalized, so that joining the chunks
	// leaves exactly one blank line at each seam. Leading ones are left alone:
	// they are part of what a single pandoc run would have produced.
	for i := range out {
		out[i] = strings.TrimRight(out[i], "\n")
	}
	return out, nil
}

// Whether this piece of markdown ends on an ATX heading, which is all the
// renderer emits. Trailing blank lines do not change the answer: a heading is
// written with one after it.
var mdHeadingRe = regexp.MustCompile(`^#{1,6} `)

func endsWithMarkdownHeading(md string) bool {
	md = strings.TrimRight(md, "\n \t")
	if i := strings.LastIndexByte(md, '\n'); i >= 0 {
		md = md[i+1:]
	}
	return mdHeadingRe.MatchString(md)
}

func runPandoc(bin, input string) (string, error) {
	// -gfm_auto_identifiers: otherwise every heading gets a
	// :PROPERTIES:/:CUSTOM_ID: drawer that nothing here links to.
	cmd := exec.Command(bin,
		"--from=gfm-gfm_auto_identifiers", "--to=org", "--wrap=none")
	cmd.Stdin = strings.NewReader(input)

	var stdout, stderr strings.Builder
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(stderr.String())
		if msg != "" {
			return "", fmt.Errorf("%v: %s", err, msg)
		}
		return "", err
	}
	return stdout.String(), nil
}

func (r *renderer) renderTurn(t Turn) {
	// Rendered first so a turn whose blocks are all empty (e.g. a bare
	// redacted-thinking turn) does not leave a dangling heading behind.
	body := &renderer{Style: r.Style, results: r.results, turnTS: t.TS, out: &strings.Builder{}}
	for _, tb := range t.Blocks {
		body.renderBlock(tb)
	}
	if strings.TrimSpace(body.out.String()) == "" && t.Heading == "" {
		return
	}

	title := t.Heading
	if title == "" && t.Role != "" {
		title = strings.ToUpper(t.Role[:1]) + t.Role[1:]
	}
	// The model leads the heading, the way a subagent's does (`** @Opus5
	// Explore · Find claude session name storage`): that is the convention
	// the readme already documents, and the turn heading was the one place
	// that disagreed. It earns the position by lining up in a column down a
	// long transcript where models alternate. A single space after the tag,
	// matching the subagent form, not a `·`.
	if m := ModelTag(t.Model); m != "" {
		if title == "" {
			title = m
		} else {
			title = m + " " + title
		}
	}
	if ts := HumanTimestamp(t.TS); ts != "" {
		title += " " + ts
	}
	if d := ShortDuration(t.Duration); d != "" {
		title += " · " + d
	}
	if t.Note != "" {
		title += " · " + t.Note
	}
	// VISIBILITY is honoured at startup, so a turn nobody reads twice -- the
	// instructions a session was launched with, say -- opens closed.
	r.turnHeading(1, title, t.Folded)
	r.out.WriteString(body.out.String())
	r.ensureBlank()
}

// A timestamp for a heading inside a turn, shown only when it says something
// the turn's own heading does not. Same minute means same moment here.
func (r *renderer) stamp(ts string) string {
	if ts == "" || r.turnTS == "" {
		return ""
	}

	t, err := time.Parse(time.RFC3339, ts)
	if err != nil {
		return ""
	}
	base, err := time.Parse(time.RFC3339, r.turnTS)
	if err != nil {
		return ""
	}

	t, base = t.Local(), base.Local()
	if t.Format(ListStamp) == base.Format(ListStamp) {
		return ""
	}
	if t.YearDay() != base.YearDay() || t.Year() != base.Year() {
		// A turn that crosses midnight needs the date to stay unambiguous.
		return " " + t.Format(OrgStamp)
	}
	return " [" + t.Format("15:04") + "]"
}

// A single-line result this short goes on its heading instead of into a block
// of its own.
const ResultInlineMax = 72

func (r *renderer) renderBlock(tb TimedBlock) {
	b := tb.B

	switch b.Type {
	case "text":
		if strings.TrimSpace(b.Text) == "" {
			return
		}
		r.prose(b.Text, 1)

	case "notice":
		r.heading(2, b.Name+r.stamp(tb.TS))
		r.prose(b.Text, 2)

	case "event":
		// Like a notice, but the body is output rather than prose: an agent
		// whose steps are an open enum renders the ones this file has no
		// vocabulary for through here, and their text is not markdown.
		r.heading(2, b.Name+r.stamp(tb.TS))
		if strings.TrimSpace(b.Text) != "" {
			r.block("", b.Text)
		}

	case "command":
		r.heading(2, "Command: "+b.Text+r.stamp(tb.TS))

	case "pr":
		r.heading(2, "Pull request "+b.Name+r.stamp(tb.TS))
		if b.Text != "" {
			r.link(b.Text)
		}

	case "tasks":
		r.heading(2, "Task reminder · "+b.Name+r.stamp(tb.TS))
		r.block("json", b.Text)

	case "file-edit":
		r.heading(2, "Edited outside the session · "+AbbrevHome(b.Name)+r.stamp(tb.TS))
		if strings.TrimSpace(b.Text) != "" {
			r.block("", b.Text)
		}

	case "thinking":
		if strings.TrimSpace(b.Thinking) == "" {
			return
		}
		// Prose, not a block. Every agent's reasoning summary is markdown --
		// `**Checking the CLI help**` paragraphs from Codex, the same from
		// Claude and Antigravity -- so a verbatim block showed the asterisks
		// and lost the paragraphs. Its headings nest under this one like any
		// other prose field's.
		r.heading(2, "Thinking"+r.stamp(tb.TS))
		r.prose(b.Thinking, 2)

	case "tool_use":
		name := b.Name
		if name == "" {
			name = "?"
		}
		in := decodeInput(b.Input)
		title := "Tool Use: " + name
		if head := toolHeadline(name, in); head != "" {
			title += " · " + head
		}
		r.heading(2, title+r.stamp(tb.TS))
		r.renderToolInput(name, in, b.Input)

		if res, ok := r.results[b.ID]; ok {
			r.renderResult(3, res)
		}

	case "tool_result":
		// Orphaned: the call it answers is not in this transcript.
		r.renderResult(2, ToolResult{
			Body:    FlattenResult(b.Content),
			IsError: b.IsError,
			TS:      tb.TS,
		})
	}
}

func (r *renderer) renderResult(level int, res ToolResult) {
	title := "Result"
	if res.IsError {
		title += " (error)"
	}
	stamp := r.stamp(res.TS)

	body := strings.TrimSpace(res.Body)
	switch {
	case body == "":
		r.heading(level, title+": (no output)"+stamp)
	case !strings.ContainsAny(body, "\n\r") && len([]rune(body)) <= ResultInlineMax:
		r.heading(level, title+": "+body+stamp)
	default:
		r.heading(level, title+stamp)
		r.block(res.Lang, res.Body)
	}
}

func decodeInput(raw json.RawMessage) map[string]json.RawMessage {
	if len(raw) == 0 {
		return nil
	}
	var in map[string]json.RawMessage
	if err := json.Unmarshal(raw, &in); err != nil {
		return nil
	}
	return in
}

// FlattenResult reads a tool result's content as text: a plain string, or a
// list of content items whose `text` fields are joined.
func FlattenResult(raw json.RawMessage) string {
	if len(raw) == 0 {
		return ""
	}

	var s string
	if err := json.Unmarshal(raw, &s); err == nil {
		return s
	}

	var items []json.RawMessage
	if err := json.Unmarshal(raw, &items); err == nil {
		parts := make([]string, 0, len(items))
		for _, it := range items {
			var obj struct {
				Text string `json:"text"`
			}
			if err := json.Unmarshal(it, &obj); err == nil && obj.Text != "" {
				parts = append(parts, obj.Text)
				continue
			}
			parts = append(parts, string(it))
		}
		return strings.Join(parts, "\n")
	}

	return string(raw)
}

// ** tool input rendering

// Keys rendered before the bulky ones, in this order. Anything unlisted is
// appended alphabetically.
var bulletOrder = []string{
	"file_path", "notebook_path", "path", "planFilePath", "url", "query", "pattern",
	"subagent_type", "skill", "description", "offset", "limit",
	"replace_all", "timeout", "run_in_background", "isolation",
}

var blockOrder = []string{"command", "old_string", "new_string", "content", "plan", "prompt"}

// Always rendered as a block, however short. A one-line `command` inlined as
// `=...=` breaks the moment it contains an `=`, and a command belongs in a
// source block anyway.
var alwaysBlock = map[string]bool{
	"command": true, "content": true, "old_string": true, "new_string": true,
	"plan": true, "prompt": true,
}

// Rendered as markdown prose (so pandoc turns them into real org markup)
// rather than as an inert code block.
var proseKeys = map[string]bool{"plan": true, "prompt": true}

// A string longer than this becomes its own block even when single-line.
const inlineMax = 100

// A bulky input value, emitted after the bullets.
type section struct {
	key   string
	lang  string
	body  string
	prose bool
}

func (r *renderer) renderToolInput(name string, in map[string]json.RawMessage, raw json.RawMessage) {
	if in == nil {
		if len(raw) > 0 {
			r.block("json", string(raw))
		}
		return
	}

	handled := map[string]bool{}
	lang := LangForPath(firstString(in, "file_path", "notebook_path", "path"))
	var sections []section

	// Whatever [toolHeadline] put in the heading must not be repeated.
	switch name {
	case "Read":
		handled["file_path"], handled["offset"], handled["limit"] = true, true, true
	case "Write", "Edit", "NotebookEdit":
		handled["file_path"], handled["notebook_path"] = true, true
	case "Glob", "Grep":
		handled["pattern"], handled["path"] = true, true
	case "WebFetch":
		handled["url"] = true
	case "WebSearch", "ToolSearch":
		handled["query"] = true
	case "Skill":
		handled["skill"] = true
	case "Bash", "Agent", "Task":
		handled["description"] = true
	}

	if name == "Edit" {
		oldS, hasOld := stringAt(in, "old_string")
		newS, hasNew := stringAt(in, "new_string")
		if r.Diff && hasOld && hasNew && oldS != "" {
			if d, ok := unifiedDiff(SplitLines(oldS), SplitLines(newS), 3); ok {
				sections = append(sections, section{key: "diff", lang: "diff", body: strings.Join(d, "\n")})
				handled["old_string"], handled["new_string"] = true, true
			}
		}
	}

	// Scalars and short strings become bullets.
	for _, k := range orderedKeys(in, bulletOrder) {
		if handled[k] || alwaysBlock[k] {
			continue
		}
		v := in[k]
		if s, ok := asString(v); ok {
			if strings.Contains(s, "\n") || len(s) > inlineMax {
				continue
			}
			if k == "file_path" || k == "notebook_path" || k == "path" || k == "planFilePath" {
				s = AbbrevHome(s)
			}
			r.bullet(k, s)
			handled[k] = true
			continue
		}
		if isScalar(v) {
			r.bullet(k, strings.TrimSpace(string(v)))
			handled[k] = true
		}
	}

	// Everything left is bulky: prose, code, or nested JSON.
	for _, k := range orderedKeys(in, blockOrder) {
		if handled[k] {
			continue
		}
		v := in[k]

		if s, ok := asString(v); ok {
			sections = append(sections, section{key: k, lang: langForKey(k, lang, s), body: s, prose: proseKeys[k]})
			continue
		}

		var pretty strings.Builder
		enc := json.NewEncoder(&pretty)
		enc.SetIndent("", "  ")
		if err := enc.Encode(json.RawMessage(v)); err != nil {
			sections = append(sections, section{key: k, lang: "json", body: string(v)})
			continue
		}
		sections = append(sections, section{key: k, lang: "json", body: strings.TrimRight(pretty.String(), "\n")})
	}

	// With a single section the heading already says what it is.
	for _, s := range sections {
		if len(sections) > 1 {
			r.label(s.key)
		}
		if s.prose {
			r.prose(s.body, 2)
		} else {
			r.block(s.lang, s.body)
		}
	}
}

func langForKey(key, pathLang, body string) string {
	switch key {
	case "command":
		return "zsh"
	case "content", "old_string", "new_string":
		return pathLang
	case "input":
		// Codex's apply_patch carries its patch here.
		if strings.HasPrefix(strings.TrimSpace(body), "*** Begin Patch") {
			return "diff"
		}
	}
	return ""
}

// A short, scannable summary for the tool-use heading. Keyed on the tool's
// name as the agent spells it; the names of every supported agent live here,
// since a display table is not worth an indirection per agent.
func toolHeadline(name string, in map[string]json.RawMessage) string {
	if in == nil {
		return ""
	}

	switch name {
	case "Bash":
		if d, ok := stringAt(in, "description"); ok && d != "" {
			return Truncate(OneLine(d), 80)
		}
		if c, ok := stringAt(in, "command"); ok {
			return Truncate(FirstLine(c), 80)
		}

	case "Read":
		if p, ok := stringAt(in, "file_path"); ok {
			head := AbbrevHome(p)
			off, hasOff := intAt(in, "offset")
			lim, hasLim := intAt(in, "limit")
			switch {
			case hasOff && hasLim:
				head += fmt.Sprintf(" (lines %d-%d)", off, off+lim-1)
			case hasOff:
				head += fmt.Sprintf(" (from line %d)", off)
			case hasLim:
				head += fmt.Sprintf(" (first %d lines)", lim)
			}
			return head
		}

	case "Write", "Edit", "NotebookEdit":
		if p := firstString(in, "file_path", "notebook_path"); p != "" {
			return AbbrevHome(p)
		}

	case "Glob", "Grep":
		p, _ := stringAt(in, "pattern")
		if dir, ok := stringAt(in, "path"); ok && dir != "" {
			return Truncate(p+" in "+AbbrevHome(dir), 80)
		}
		return Truncate(p, 80)

	case "WebFetch":
		if u, ok := stringAt(in, "url"); ok {
			return Truncate(u, 80)
		}

	case "WebSearch", "ToolSearch":
		if q, ok := stringAt(in, "query"); ok {
			return Truncate(OneLine(q), 80)
		}

	case "Skill":
		if s, ok := stringAt(in, "skill"); ok {
			return s
		}

	case "Agent", "Task":
		if d, ok := stringAt(in, "description"); ok {
			return Truncate(OneLine(d), 80)
		}

	// Codex.
	case "shell", "exec_command", "container.exec":
		if c := FirstString(in, "command", "cmd"); c != "" {
			return Truncate(FirstLine(c), 80)
		}

	case "apply_patch":
		if p, ok := stringAt(in, "input"); ok {
			return Truncate(patchFiles(p), 80)
		}

	case "update_plan":
		if e, ok := stringAt(in, "explanation"); ok && e != "" {
			return Truncate(OneLine(e), 80)
		}
		var plan []struct {
			Step string `json:"step"`
		}
		if json.Unmarshal(in["plan"], &plan) == nil && len(plan) > 0 {
			return Truncate(OneLine(plan[0].Step), 80)
		}

	case "web_search":
		if q, ok := stringAt(in, "query"); ok {
			return Truncate(OneLine(q), 80)
		}
	}

	return ""
}

// ShortDuration is a turn's wall-clock length, as `4m2s`, for its heading.
func ShortDuration(d time.Duration) string {
	switch {
	case d <= 0:
		return ""
	case d < time.Minute:
		return fmt.Sprintf("%ds", int(d.Seconds()))
	case d < time.Hour:
		return fmt.Sprintf("%dm%ds", int(d.Minutes()), int(d.Seconds())%60)
	default:
		return fmt.Sprintf("%dh%dm", int(d.Hours()), int(d.Minutes())%60)
	}
}

// The files an apply_patch input touches, for its heading: `*** Update File:
// a.go`, `*** Add File: b.go`, `*** Delete File: c.go` lines, in order.
func patchFiles(patch string) string {
	var files []string
	for _, ln := range strings.Split(patch, "\n") {
		for _, prefix := range []string{"*** Update File: ", "*** Add File: ", "*** Delete File: "} {
			if strings.HasPrefix(ln, prefix) {
				files = append(files, AbbrevHome(strings.TrimSpace(strings.TrimPrefix(ln, prefix))))
			}
		}
	}
	return strings.Join(files, ", ")
}
