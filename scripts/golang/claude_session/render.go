package main

import (
	"bufio"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"runtime"
	"strings"
	"sync"
	"time"
)

// ** render

type renderer struct {
	org      bool
	maxBlock int
	diff     bool
	out      *strings.Builder

	// Results, keyed by the id of the call they answer, so a call can render
	// its own result underneath itself.
	results map[string]toolResult
	// The enclosing turn's timestamp; sub-headings only show theirs when it
	// differs.
	turnTS string
	// Heading offset, for a transcript nested inside another document.
	base int
}

// Below this, splitting the document across pandoc processes costs more in
// process startup (~70ms each) than it saves.
const minPandocChunk = 96 << 10

func cmdRender(argv []string) {
	fs := flag.NewFlagSet("render", flag.ExitOnError)
	format := fs.String("format", "md", "output syntax: md, org or org-pandoc")
	maxBlock := fs.Int("max-block-lines", 0, "elide code blocks longer than N lines (0 = never)")
	diff := fs.Bool("diff", true, "render Edit tool calls as a unified diff")
	jobs := fs.Int("jobs", runtime.NumCPU(), "worker count")
	pandocBin := fs.String("pandoc", "pandoc", "pandoc binary, for -format=org-pandoc")
	subagentsP := fs.Bool("subagents", true, "inline the transcripts of spawned subagents")
	fs.Parse(argv)

	input := fs.Arg(0)
	if input == "" {
		fatal("render: no input file given")
	}
	switch *format {
	case "md", "org", "org-pandoc":
	default:
		fatal("render: unknown format: " + *format)
	}
	if *jobs < 1 {
		*jobs = 1
	}

	fh, err := os.Open(input)
	if err != nil {
		fatal(err.Error())
	}
	defer fh.Close()

	records := conversationRecords(readRecords(fh))

	// Decoded once: the result index, the turn grouping and the rendering all
	// need the blocks.
	blocks := make([][]block, len(records))
	for i := range records {
		blocks[i] = decodeBlocks(records[i].Message)
	}

	results := indexResults(records, blocks)
	turns := buildTurns(records, blocks, results)

	opts := renderOpts{org: *format == "org", maxBlock: *maxBlock, diff: *diff}

	// The skeleton Go emits is in the *output* syntax, which for org-pandoc is
	// org even though the bodies it wraps are still markdown.
	orgOut := *format != "md"

	var segs []segment
	if *subagentsP {
		segs = append(segs, subagentSegments(input, blocks, opts, orgOut, *jobs)...)
	}
	for _, p := range renderTurns(turns, results, opts, *jobs) {
		segs = append(segs, segment{text: p, body: true})
	}

	w := bufio.NewWriter(os.Stdout)
	defer w.Flush()

	if *format != "org-pandoc" {
		for _, s := range segs {
			w.WriteString(s.text)
		}
		return
	}

	var doc strings.Builder
	for _, s := range convertSegments(segs, *jobs, *pandocBin) {
		doc.WriteString(s)
	}
	w.WriteString(strings.TrimRight(doc.String(), "\n") + "\n")
}

type renderOpts struct {
	org      bool
	maxBlock int
	diff     bool
	// Added to every heading level, so a transcript can be nested inside
	// another document.
	base int
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
func subagentSegments(input string, blocks [][]block, opts renderOpts, orgOut bool, jobs int) []segment {
	subs := loadSubagents(input, toolCallOrder(blocks))
	if len(subs) == 0 {
		return nil
	}

	mark := "#"
	if orgOut {
		mark = "*"
	}
	head := func(level int, text string) string {
		return strings.Repeat(mark, level) + " " + text + "\n\n"
	}

	segs := []segment{{text: head(1, "Subagents")}}

	for _, s := range subs {
		title := head(2, s.title())
		if orgOut {
			// VISIBILITY is honoured at startup, so each agent opens folded.
			title = strings.Repeat(mark, 2) + " " + s.title() +
				"\n:PROPERTIES:\n:VISIBILITY: folded\n:END:\n\n"
		}
		segs = append(segs, segment{text: title})

		for _, p := range renderSubagent(s, opts, jobs) {
			segs = append(segs, segment{text: p, body: true})
		}
	}

	return segs
}

// One conversational turn: the consecutive records that share a role, flattened
// into the blocks they contain. Claude Code writes one record per content
// block, so without this an assistant turn becomes a run of near-identical
// headings.
type turn struct {
	role   string
	ts     string
	model  string
	blocks []timedBlock

	// Overrides the role-derived heading, for turns that are an event rather
	// than somebody speaking.
	heading string
	// Trailing detail, after the timestamp.
	note string
	// How long the turn took, when the transcript says so.
	duration time.Duration
}

// The records that make up the conversation: the messages, plus the events
// that punctuate them.
//
// Everything else in a transcript is bookkeeping and stays out: `mode`,
// `permission-mode`, `agent-name`/`agent-color`/`agent-setting`,
// `bridge-session`, `file-history-snapshot`/`-delta`, and `last-prompt`, which
// only repeats the message next to it. `queue-operation` is left out for a
// subtler reason: half of what gets enqueued is delivered and so already shows
// up as an ordinary user message, and the other half was withdrawn before it
// was ever sent. Most `attachment` payloads are harness internals
// (`task_reminder`, `skill_listing`, `deferred_tools_delta`); only a file you
// edited yourself says anything about the conversation.
func conversationRecords(all []record) []record {
	var out []record
	for _, rec := range all {
		switch rec.Type {
		case "user", "assistant":
			if rec.IsMeta {
				continue
			}
		case "system":
			switch rec.Subtype {
			case subtypeRecap, subtypeCompact, subtypeCommand, subtypeInfo,
				subtypeFallback, subtypeDuration:
			default:
				continue
			}
		case "pr-link":
		case "attachment":
			if rec.Attachment == nil || rec.Attachment.Type != "edited_text_file" {
				continue
			}
		default:
			continue
		}
		out = append(out, rec)
	}
	return out
}

// A block plus the timestamp of the record it arrived in, which within a turn
// is not necessarily the turn's own.
type timedBlock struct {
	b  block
	ts string
}

// A tool result, keyed elsewhere by the id of the call it answers.
type toolResult struct {
	body    string
	isError bool
	ts      string
}

// Results arrive as user turns, because that is how they are sent back to the
// model. Indexing them by the call they answer lets them be rendered under it
// instead of as a message nobody wrote.
func indexResults(records []record, blocks [][]block) map[string]toolResult {
	calls := map[string]bool{}
	for _, bs := range blocks {
		for _, b := range bs {
			if b.Type == "tool_use" && b.ID != "" {
				calls[b.ID] = true
			}
		}
	}

	out := map[string]toolResult{}
	for i, bs := range blocks {
		for _, b := range bs {
			if b.Type != "tool_result" || !calls[b.ToolUseID] {
				continue
			}
			out[b.ToolUseID] = toolResult{
				body:    flattenResult(b.Content),
				isError: b.IsError,
				ts:      records[i].Timestamp,
			}
		}
	}
	return out
}

func buildTurns(records []record, blocks [][]block, results map[string]toolResult) []turn {
	var turns []turn

	for i, rec := range records {
		if rec.Type != "user" && rec.Type != "assistant" {
			turns = appendEvent(turns, rec)
			continue
		}

		model := ""
		if rec.Message != nil {
			model = rec.Message.Model
		}

		var keep []timedBlock
		for _, b := range blocks[i] {
			// Nested under its call; an orphan with no matching call still
			// gets rendered where it sits.
			if b.Type == "tool_result" {
				if _, nested := results[b.ToolUseID]; nested {
					continue
				}
			}
			keep = append(keep, timedBlock{b: b, ts: rec.Timestamp})
		}
		if len(keep) == 0 {
			continue
		}

		// Merging stops at a model change, so a switch mid-answer starts a
		// new heading rather than hiding inside one. Annotating sub-headings
		// instead would miss a switch that lands on a plain text block, which
		// has no heading to annotate.
		if n := len(turns); n > 0 && turns[n-1].role == rec.Type && turns[n-1].model == model {
			turns[n-1].blocks = append(turns[n-1].blocks, keep...)
			continue
		}
		turns = append(turns, turn{role: rec.Type, ts: rec.Timestamp, model: model, blocks: keep})
	}

	return turns
}

// Turns are independent, so they render concurrently and are reassembled in
// order.
func renderTurns(turns []turn, results map[string]toolResult, opts renderOpts, jobs int) []string {
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
			r := &renderer{
				org:      opts.org,
				maxBlock: opts.maxBlock,
				diff:     opts.diff,
				base:     opts.base,
				results:  results,
				out:      &strings.Builder{},
			}
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
func convertSegments(segs []segment, jobs int, bin string) []string {
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

		// Trailing blank line so a skeleton heading after this run is its own
		// block; the caller trims whatever is left over at the very end.
		out[i] = strings.Join(pandocChunks(run, jobs, bin), "\n\n") + "\n\n"
		for k := i + 1; k < j; k++ {
			out[k] = ""
		}
		i = j
	}

	return out
}

// Splits the rendered records into byte-balanced chunks and converts each with
// its own pandoc. Chunk seams fall on record boundaries, never inside a code
// block, so each chunk is a self-contained markdown document and the result is
// identical to converting the whole thing at once.
func pandocChunks(parts []string, jobs int, bin string) []string {
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
		if cur.Len() >= target && len(chunks) < n-1 {
			chunks = append(chunks, cur.String())
			cur.Reset()
		}
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
			fatal(fmt.Sprintf("pandoc (chunk %d/%d): %v", i+1, len(chunks), err))
		}
	}

	// Only the trailing newlines are normalized, so that joining the chunks
	// leaves exactly one blank line at each seam. Leading ones are left alone:
	// they are part of what a single pandoc run would have produced.
	for i := range out {
		out[i] = strings.TrimRight(out[i], "\n")
	}
	return out
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

func readRecords(fh *os.File) []record {
	var out []record
	sc := bufio.NewScanner(fh)
	sc.Buffer(make([]byte, 0, 64<<10), maxLineBytes)
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if len(line) == 0 || line[0] != '{' {
			continue
		}
		var rec record
		if err := json.Unmarshal([]byte(line), &rec); err != nil {
			// A truncated or malformed line loses one message, not the file.
			continue
		}
		out = append(out, rec)
	}
	return out
}

func (r *renderer) renderTurn(t turn) {
	// Rendered first so a turn whose blocks are all empty (e.g. a bare
	// redacted-thinking turn) does not leave a dangling heading behind.
	body := &renderer{
		org:      r.org,
		maxBlock: r.maxBlock,
		diff:     r.diff,
		base:     r.base,
		results:  r.results,
		turnTS:   t.ts,
		out:      &strings.Builder{},
	}
	for _, tb := range t.blocks {
		body.renderBlock(tb)
	}
	if strings.TrimSpace(body.out.String()) == "" && t.heading == "" {
		return
	}

	title := t.heading
	if title == "" {
		title = strings.ToUpper(t.role[:1]) + t.role[1:]
	}
	if ts := humanTimestamp(t.ts); ts != "" {
		title += " " + ts
	}
	if m := shortModel(t.model); m != "" {
		title += " · " + m
	}
	if d := shortDuration(t.duration); d != "" {
		title += " · " + d
	}
	if t.note != "" {
		title += " · " + t.note
	}
	r.heading(1, title)
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
	if t.Format(listStamp) == base.Format(listStamp) {
		return ""
	}
	if t.YearDay() != base.YearDay() || t.Year() != base.Year() {
		// A turn that crosses midnight needs the date to stay unambiguous.
		return " " + t.Format(orgStamp)
	}
	return " [" + t.Format("15:04") + "]"
}

func decodeBlocks(m *message) []block {
	if m == nil || len(m.Content) == 0 {
		return nil
	}

	var s string
	if err := json.Unmarshal(m.Content, &s); err == nil {
		return []block{{Type: "text", Text: s}}
	}

	var blocks []block
	if err := json.Unmarshal(m.Content, &blocks); err != nil {
		return nil
	}
	return blocks
}

// A single-line result this short goes on its heading instead of into a block
// of its own.
const resultInlineMax = 72

func (r *renderer) renderBlock(tb timedBlock) {
	b := tb.b

	switch b.Type {
	case "text":
		if strings.TrimSpace(b.Text) == "" {
			return
		}
		r.prose(b.Text, 1)

	case "notice":
		r.heading(2, b.Name+r.stamp(tb.ts))
		r.prose(b.Text, 2)

	case "command":
		r.heading(2, "Command: "+b.Text+r.stamp(tb.ts))

	case "pr":
		r.heading(2, "Pull request "+b.Name+r.stamp(tb.ts))
		if b.Text != "" {
			r.link(b.Text)
		}

	case "file-edit":
		r.heading(2, "Edited outside the session · "+abbrevHome(b.Name)+r.stamp(tb.ts))
		if strings.TrimSpace(b.Text) != "" {
			r.block("", b.Text)
		}

	case "thinking":
		if strings.TrimSpace(b.Thinking) == "" {
			return
		}
		r.heading(2, "Thinking"+r.stamp(tb.ts))
		r.block("", b.Thinking)

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
		r.heading(2, title+r.stamp(tb.ts))
		r.renderToolInput(name, in, b.Input)

		if res, ok := r.results[b.ID]; ok {
			r.renderResult(3, res)
		}

	case "tool_result":
		// Orphaned: the call it answers is not in this transcript.
		r.renderResult(2, toolResult{
			body:    flattenResult(b.Content),
			isError: b.IsError,
			ts:      tb.ts,
		})
	}
}

func (r *renderer) renderResult(level int, res toolResult) {
	title := "Result"
	if res.isError {
		title += " (error)"
	}
	stamp := r.stamp(res.ts)

	body := strings.TrimSpace(res.body)
	switch {
	case body == "":
		r.heading(level, title+": (no output)"+stamp)
	case !strings.ContainsAny(body, "\n\r") && len([]rune(body)) <= resultInlineMax:
		r.heading(level, title+": "+body+stamp)
	default:
		r.heading(level, title+stamp)
		r.block("", res.body)
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

func flattenResult(raw json.RawMessage) string {
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
	lang := langForPath(firstString(in, "file_path", "notebook_path", "path"))
	var sections []section

	// Whatever [agfi:toolHeadline] put in the heading must not be repeated.
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
		if r.diff && hasOld && hasNew && oldS != "" {
			if d, ok := unifiedDiff(splitLines(oldS), splitLines(newS), 3); ok {
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
				s = abbrevHome(s)
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
			sections = append(sections, section{key: k, lang: langForKey(k, lang), body: s, prose: proseKeys[k]})
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

func langForKey(key, pathLang string) string {
	switch key {
	case "command":
		return "zsh"
	case "content", "old_string", "new_string":
		return pathLang
	}
	return ""
}

// A short, scannable summary for the tool-use heading.
func toolHeadline(name string, in map[string]json.RawMessage) string {
	if in == nil {
		return ""
	}

	switch name {
	case "Bash":
		if d, ok := stringAt(in, "description"); ok && d != "" {
			return truncate(oneLine(d), 80)
		}
		if c, ok := stringAt(in, "command"); ok {
			return truncate(firstLine(c), 80)
		}

	case "Read":
		if p, ok := stringAt(in, "file_path"); ok {
			head := abbrevHome(p)
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
			return abbrevHome(p)
		}

	case "Glob", "Grep":
		p, _ := stringAt(in, "pattern")
		if dir, ok := stringAt(in, "path"); ok && dir != "" {
			return truncate(p+" in "+abbrevHome(dir), 80)
		}
		return truncate(p, 80)

	case "WebFetch":
		if u, ok := stringAt(in, "url"); ok {
			return truncate(u, 80)
		}

	case "WebSearch", "ToolSearch":
		if q, ok := stringAt(in, "query"); ok {
			return truncate(oneLine(q), 80)
		}

	case "Skill":
		if s, ok := stringAt(in, "skill"); ok {
			return s
		}

	case "Agent", "Task":
		if d, ok := stringAt(in, "description"); ok {
			return truncate(oneLine(d), 80)
		}
	}

	return ""
}

// Everything in a transcript that is neither a message nor bookkeeping: the
// recaps, notices, slash commands, compaction boundaries, pull requests and
// externally edited files. Each attaches to the turn it interrupts, except a
// compaction, which separates two phases of the conversation and so stands on
// its own.
func appendEvent(turns []turn, rec record) []turn {
	var text string
	json.Unmarshal(rec.Content, &text)
	text = strings.TrimSpace(text)

	attach := func(b block) []turn {
		tb := timedBlock{b: b, ts: rec.Timestamp}
		if n := len(turns); n > 0 {
			turns[n-1].blocks = append(turns[n-1].blocks, tb)
			return turns
		}
		return append(turns, turn{role: "assistant", ts: rec.Timestamp, blocks: []timedBlock{tb}})
	}

	switch {
	case rec.Type == "pr-link":
		label := rec.PRRepository
		if label == "" {
			label = "Pull request"
		}
		return attach(block{Type: "pr", Name: fmt.Sprintf("%s#%d", label, rec.PRNumber), Text: rec.PRUrl})

	case rec.Type == "attachment":
		path := rec.Attachment.DisplayPath
		if path == "" {
			path = rec.Attachment.Filename
		}
		return attach(block{Type: "file-edit", Name: path, Text: rec.Attachment.Snippet})

	case rec.Subtype == subtypeDuration:
		// Belongs to the turn it measures, in its heading.
		if n := len(turns); n > 0 && rec.DurationMs > 0 {
			turns[n-1].duration = time.Duration(rec.DurationMs) * time.Millisecond
		}
		return turns

	case rec.Subtype == subtypeCompact:
		note := ""
		if m := rec.CompactMetadata; m != nil {
			if m.Trigger != "" {
				note = m.Trigger
			}
			if m.PreTokens > 0 {
				if note != "" {
					note += " · "
				}
				note += fmt.Sprintf("%d → %d tokens", m.PreTokens, m.PostTokens)
			}
		}
		return append(turns, turn{
			role: "system", heading: "Context compacted", note: note, ts: rec.Timestamp,
		})

	case rec.Subtype == subtypeCommand:
		// The payload is the same `<command-name>` scaffolding the picker
		// strips, so it reduces to the command that was run.
		if s := snippetText(text); s != "" {
			return attach(block{Type: "command", Text: s})
		}
		return turns

	case text != "":
		// away_summary, informational, model_consent_fallback.
		kind := "Notice"
		switch rec.Subtype {
		case subtypeRecap:
			kind = "Recap"
		case subtypeFallback:
			kind = "Model fallback"
		}
		return attach(block{Type: "notice", Name: kind, Text: text})
	}

	return turns
}

// A turn's wall-clock length, as `4m2s`, for its heading.
func shortDuration(d time.Duration) string {
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
