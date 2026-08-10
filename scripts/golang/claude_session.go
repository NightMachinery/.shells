#!/usr/bin/env scriptisto

package main

// scriptisto-begin
// script_src: main.go
// build_cmd: go build -o script -trimpath
// target_bin: ./script
// replace_shebang_with: //
// files:
//  - path: go.mod
//    content: |
//      module night/claude_session
//      go 1.21
// scriptisto-end

// Renders Claude Code session `.jsonl` transcripts, and lists them for a
// fuzzy picker. Called from [agfi:h-claude-code-session-to-md],
// [agfi:h-claude-code-session-to-org-pandoc] and
// [agfi:h-claude-code-session-select-fz].
//
// Stdlib only, so `go build` needs no network and works on hosts without a
// module cache.

import (
	"bufio"
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"sync"
	"time"
)

// Session lines carry whole files inline, so the default 64KiB scanner
// buffer is nowhere near enough.
const maxLineBytes = 64 << 20

const (
	orgStamp  = "[2006-01-02 Mon 15:04]"
	listStamp = "2006-01-02 15:04"
)

type record struct {
	Type      string   `json:"type"`
	IsMeta    bool     `json:"isMeta"`
	Timestamp string   `json:"timestamp"`
	Message   *message `json:"message"`
}

type message struct {
	Content json.RawMessage `json:"content"`
}

type block struct {
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

func main() {
	if len(os.Args) < 2 {
		usage()
	}

	switch os.Args[1] {
	case "render":
		cmdRender(os.Args[2:])
	case "list":
		cmdList(os.Args[2:])
	case "-h", "--help", "help":
		usage()
	default:
		fmt.Fprintf(os.Stderr, "claude_session: unknown subcommand: %s\n", os.Args[1])
		usage()
	}
}

func usage() {
	fmt.Fprint(os.Stderr, `usage:
  claude_session.go render [flags] <session.jsonl>   #: transcript -> markdown/org on stdout
  claude_session.go list   [flags] <sessions-dir>    #: TSV of sessions, newest first

render flags:
  -format md|org|org-pandoc   output syntax (default md). org-pandoc pipes the
                              markdown through pandoc, in parallel chunks
  -max-block-lines N          elide code blocks longer than N lines (0 = never)
  -diff                       render Edit as a unified diff (default true)
  -jobs N                     worker count (default: CPU count)
  -pandoc PATH                pandoc binary for org-pandoc (default "pandoc")

list flags:
  -snippet-len N        max snippet width (default 120)
  -jobs N               worker count (default: CPU count)

list emits: epoch <TAB> path <TAB> local time <TAB> relative path <TAB> snippet
`)
	os.Exit(2)
}

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

	var records []record
	for _, rec := range readRecords(fh) {
		if rec.Type != "user" && rec.Type != "assistant" {
			continue
		}
		if rec.IsMeta {
			continue
		}
		records = append(records, rec)
	}

	// Decoded once: the result index, the turn grouping and the rendering all
	// need the blocks.
	blocks := make([][]block, len(records))
	for i := range records {
		blocks[i] = decodeBlocks(records[i].Message)
	}

	results := indexResults(records, blocks)
	turns := buildTurns(records, blocks, results)

	opts := renderOpts{org: *format == "org", maxBlock: *maxBlock, diff: *diff}
	parts := renderTurns(turns, results, opts, *jobs)

	w := bufio.NewWriter(os.Stdout)
	defer w.Flush()

	if *format != "org-pandoc" {
		for _, p := range parts {
			w.WriteString(p)
		}
		return
	}

	for i, chunk := range pandocChunks(parts, *jobs, *pandocBin) {
		if i > 0 {
			w.WriteString("\n\n")
		}
		w.WriteString(chunk)
	}
	w.WriteString("\n")
}

type renderOpts struct {
	org      bool
	maxBlock int
	diff     bool
}

// One conversational turn: the consecutive records that share a role, flattened
// into the blocks they contain. Claude Code writes one record per content
// block, so without this an assistant turn becomes a run of near-identical
// headings.
type turn struct {
	role   string
	ts     string
	blocks []timedBlock
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

		if n := len(turns); n > 0 && turns[n-1].role == rec.Type {
			turns[n-1].blocks = append(turns[n-1].blocks, keep...)
			continue
		}
		turns = append(turns, turn{role: rec.Type, ts: rec.Timestamp, blocks: keep})
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
		results:  r.results,
		turnTS:   t.ts,
		out:      &strings.Builder{},
	}
	for _, tb := range t.blocks {
		body.renderBlock(tb)
	}
	if strings.TrimSpace(body.out.String()) == "" {
		return
	}

	title := strings.ToUpper(t.role[:1]) + t.role[1:]
	if ts := humanTimestamp(t.ts); ts != "" {
		title += " " + ts
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

// ** output primitives

func (r *renderer) heading(level int, text string) {
	mark := "#"
	if r.org {
		mark = "*"
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
	r.out.WriteString(shiftHeadings(s, parentLevel+1) + "\n")
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

// ** diff

type diffOp struct {
	kind byte //: ' ', '-', '+'
	text string
}

// Line-based unified diff via an LCS table. Reports false when the inputs
// are big enough that the quadratic table is not worth it; the caller then
// falls back to printing both sides.
func unifiedDiff(a, b []string, ctx int) ([]string, bool) {
	n, m := len(a), len(b)
	if n*m > 4_000_000 {
		return nil, false
	}

	// dp[i*(m+1)+j] = length of the LCS of a[i:] and b[j:].
	dp := make([]uint32, (n+1)*(m+1))
	for i := n - 1; i >= 0; i-- {
		for j := m - 1; j >= 0; j-- {
			if a[i] == b[j] {
				dp[i*(m+1)+j] = dp[(i+1)*(m+1)+j+1] + 1
			} else if dp[(i+1)*(m+1)+j] >= dp[i*(m+1)+j+1] {
				dp[i*(m+1)+j] = dp[(i+1)*(m+1)+j]
			} else {
				dp[i*(m+1)+j] = dp[i*(m+1)+j+1]
			}
		}
	}

	var ops []diffOp
	i, j := 0, 0
	for i < n && j < m {
		switch {
		case a[i] == b[j]:
			ops = append(ops, diffOp{' ', a[i]})
			i, j = i+1, j+1
		case dp[(i+1)*(m+1)+j] >= dp[i*(m+1)+j+1]:
			ops = append(ops, diffOp{'-', a[i]})
			i++
		default:
			ops = append(ops, diffOp{'+', b[j]})
			j++
		}
	}
	for ; i < n; i++ {
		ops = append(ops, diffOp{'-', a[i]})
	}
	for ; j < m; j++ {
		ops = append(ops, diffOp{'+', b[j]})
	}

	return formatHunks(ops, ctx), true
}

func formatHunks(ops []diffOp, ctx int) []string {
	var out []string

	for start := 0; start < len(ops); {
		if ops[start].kind == ' ' {
			start++
			continue
		}

		// Grow the hunk while the next change is close enough that its
		// leading context would touch this one's trailing context, which is
		// where diff(1) merges them too.
		lo := start - ctx
		if lo < 0 {
			lo = 0
		}
		hi := start
		for hi < len(ops) {
			next := nextChange(ops, hi+1)
			if next >= 0 && next-hi-1 <= 2*ctx {
				hi = next
				continue
			}
			break
		}
		hi += ctx
		if hi > len(ops)-1 {
			hi = len(ops) - 1
		}

		aStart, bStart := 0, 0
		for _, op := range ops[:lo] {
			if op.kind != '+' {
				aStart++
			}
			if op.kind != '-' {
				bStart++
			}
		}

		aCount, bCount := 0, 0
		for _, op := range ops[lo : hi+1] {
			if op.kind != '+' {
				aCount++
			}
			if op.kind != '-' {
				bCount++
			}
		}

		out = append(out, fmt.Sprintf("@@ -%d,%d +%d,%d @@", aStart+1, aCount, bStart+1, bCount))
		for _, op := range ops[lo : hi+1] {
			out = append(out, string(op.kind)+op.text)
		}

		start = hi + 1
	}

	return out
}

func nextChange(ops []diffOp, from int) int {
	for i := from; i < len(ops); i++ {
		if ops[i].kind != ' ' {
			return i
		}
	}
	return -1
}

// ** list

type sessionInfo struct {
	path    string
	rel     string
	epoch   int64
	stamp   string
	snippet string
}

func cmdList(argv []string) {
	fs := flag.NewFlagSet("list", flag.ExitOnError)
	snippetLen := fs.Int("snippet-len", 120, "max snippet width, in runes")
	jobs := fs.Int("jobs", runtime.NumCPU(), "worker count")
	fs.Parse(argv)

	dir := fs.Arg(0)
	if dir == "" {
		fatal("list: no sessions directory given")
	}

	var files []string
	err := filepath.WalkDir(dir, func(p string, d os.DirEntry, err error) error {
		if err != nil {
			return nil
		}
		if !d.IsDir() && strings.HasSuffix(p, ".jsonl") {
			files = append(files, p)
		}
		return nil
	})
	if err != nil {
		fatal(err.Error())
	}
	if len(files) == 0 {
		fatal("list: no session files found in: " + dir)
	}

	infos := make([]sessionInfo, len(files))
	workers := *jobs
	if workers > len(files) {
		workers = len(files)
	}
	if workers < 1 {
		workers = 1
	}

	var wg sync.WaitGroup
	queue := make(chan int)
	for w := 0; w < workers; w++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for idx := range queue {
				infos[idx] = scanSession(files[idx], dir, *snippetLen)
			}
		}()
	}
	for i := range files {
		queue <- i
	}
	close(queue)
	wg.Wait()

	sort.SliceStable(infos, func(i, j int) bool {
		if infos[i].epoch != infos[j].epoch {
			return infos[i].epoch > infos[j].epoch
		}
		return infos[i].path < infos[j].path
	})

	w := bufio.NewWriter(os.Stdout)
	defer w.Flush()
	for _, s := range infos {
		fmt.Fprintf(w, "%d\t%s\t%s\t%s\t%s\n", s.epoch, s.path, s.stamp, s.rel, s.snippet)
	}
}

// How much of the file's end is searched for the last message's timestamp,
// and how far into the start the first user message is looked for. Both grow
// on demand, so these only decide how much is read in the common case.
const (
	tailWindow = 64 << 10
	headWindow = 4 << 20
)

// The session's time is that of its last user/assistant message. The file's
// mtime is not usable: Claude Code appends bookkeeping records (e.g.
// `bridge-session`) long after the conversation ends, which can put mtime
// hours or days past the last message.
//
// Only the two ends of the file are read. Reading all of it would make the
// picker cost grow with total transcript volume rather than with the number
// of sessions.
func scanSession(path, root string, snippetLen int) sessionInfo {
	info := sessionInfo{path: path}
	if rel, err := filepath.Rel(root, path); err == nil {
		info.rel = rel
	} else {
		info.rel = filepath.Base(path)
	}

	var last time.Time
	var snippet string

	if fh, err := os.Open(path); err == nil {
		defer fh.Close()
		if st, err := fh.Stat(); err == nil {
			last = lastMessageTime(fh, st.Size())
		}
		snippet = firstUserText(fh)
	}

	if last.IsZero() {
		// No timestamped message at all (empty or unreadable file).
		if st, err := os.Stat(path); err == nil {
			last = st.ModTime()
		}
	}

	info.epoch = last.Unix()
	info.stamp = last.Local().Format(listStamp)
	info.snippet = truncate(oneLine(snippet), snippetLen)
	return info
}

// How many timestamped messages to look back over. Records are written in
// order, so the last one almost always wins; the slack only has to cover the
// millisecond-scale reordering that does occur in practice.
const tailRecords = 25

// Only the type and timestamp are needed to date a session. Decoding into the
// full record would copy every message body in the window for nothing.
type stampOnly struct {
	Type      string `json:"type"`
	Timestamp string `json:"timestamp"`
}

// Newest user/assistant timestamp, found by walking backwards from the end of
// the file and widening the window until something turns up.
func lastMessageTime(fh *os.File, size int64) time.Time {
	for window := int64(tailWindow); ; window *= 4 {
		if window > size {
			window = size
		}

		buf := make([]byte, window)
		if _, err := fh.ReadAt(buf, size-window); err != nil {
			return time.Time{}
		}

		var last time.Time
		seen := 0

		// Backwards, line by line, so a long transcript costs the same as a
		// short one.
		end := len(buf)
		for end > 0 && seen < tailRecords {
			start := bytes.LastIndexByte(buf[:end], '\n') + 1
			if start == 0 && window < size {
				// The window cut this line in half; it is not parseable.
				break
			}

			line := strings.TrimSpace(string(buf[start:end]))
			end = start - 1

			if len(line) == 0 || line[0] != '{' {
				continue
			}
			var rec stampOnly
			if err := json.Unmarshal([]byte(line), &rec); err != nil {
				continue
			}
			if rec.Type != "user" && rec.Type != "assistant" {
				continue
			}
			if t, err := time.Parse(time.RFC3339, rec.Timestamp); err == nil {
				seen++
				if t.After(last) {
					last = t
				}
			}
		}

		if !last.IsZero() || window >= size {
			return last
		}
	}
}

// First non-meta user message with text, read from the start and abandoned
// once the file stops being worth scanning for one.
func firstUserText(fh *os.File) string {
	if _, err := fh.Seek(0, 0); err != nil {
		return ""
	}

	sc := bufio.NewScanner(fh)
	sc.Buffer(make([]byte, 0, 64<<10), maxLineBytes)

	read := 0
	for sc.Scan() {
		line := sc.Text()
		read += len(line) + 1
		if read > headWindow {
			return ""
		}

		rec, ok := parseRecord(line)
		if !ok || rec.Type != "user" || rec.IsMeta {
			continue
		}
		for _, b := range decodeBlocks(rec.Message) {
			if b.Type == "text" && strings.TrimSpace(b.Text) != "" {
				return b.Text
			}
		}
	}
	return ""
}

func parseRecord(line string) (record, bool) {
	line = strings.TrimSpace(line)
	if len(line) == 0 || line[0] != '{' {
		return record{}, false
	}
	var rec record
	if err := json.Unmarshal([]byte(line), &rec); err != nil {
		return record{}, false
	}
	return rec, true
}

// ** helpers

func humanTimestamp(ts string) string {
	if ts == "" {
		return ""
	}
	t, err := time.Parse(time.RFC3339, ts)
	if err != nil {
		return ts
	}
	return t.Local().Format(orgStamp)
}

func orderedKeys(in map[string]json.RawMessage, preferred []string) []string {
	seen := map[string]bool{}
	var out []string

	for _, k := range preferred {
		if _, ok := in[k]; ok {
			out = append(out, k)
			seen[k] = true
		}
	}

	var rest []string
	for k := range in {
		if !seen[k] {
			rest = append(rest, k)
		}
	}
	sort.Strings(rest)

	return append(out, rest...)
}

func asString(raw json.RawMessage) (string, bool) {
	var s string
	if err := json.Unmarshal(raw, &s); err != nil {
		return "", false
	}
	return s, true
}

func isScalar(raw json.RawMessage) bool {
	s := strings.TrimSpace(string(raw))
	if s == "" {
		return false
	}
	return s[0] != '{' && s[0] != '['
}

func stringAt(in map[string]json.RawMessage, key string) (string, bool) {
	raw, ok := in[key]
	if !ok {
		return "", false
	}
	return asString(raw)
}

func intAt(in map[string]json.RawMessage, key string) (int, bool) {
	raw, ok := in[key]
	if !ok {
		return 0, false
	}
	n, err := strconv.Atoi(strings.TrimSpace(string(raw)))
	if err != nil {
		return 0, false
	}
	return n, true
}

func firstString(in map[string]json.RawMessage, keys ...string) string {
	for _, k := range keys {
		if s, ok := stringAt(in, k); ok && s != "" {
			return s
		}
	}
	return ""
}

var langByExt = map[string]string{
	".c": "c", ".cc": "cpp", ".cpp": "cpp", ".css": "css", ".el": "emacs-lisp",
	".go": "go", ".h": "c", ".hs": "haskell", ".html": "html", ".java": "java",
	".jl": "julia", ".js": "javascript", ".json": "json", ".jsx": "jsx",
	".lua": "lua", ".md": "markdown", ".org": "org", ".pl": "perl",
	".py": "python", ".rb": "ruby", ".rs": "rust", ".scm": "scheme",
	".sh": "sh", ".sql": "sql", ".svelte": "svelte", ".toml": "toml",
	".ts": "typescript", ".tsx": "tsx", ".vim": "vim", ".yaml": "yaml",
	".yml": "yaml", ".zsh": "zsh",
}

func langForPath(p string) string {
	if p == "" {
		return ""
	}
	return langByExt[strings.ToLower(filepath.Ext(p))]
}

func abbrevHome(p string) string {
	home, err := os.UserHomeDir()
	if err != nil || home == "" {
		return p
	}
	if p == home {
		return "~"
	}
	if strings.HasPrefix(p, home+"/") {
		return "~" + p[len(home):]
	}
	return p
}

func splitLines(s string) []string {
	s = strings.TrimSuffix(s, "\n")
	if s == "" {
		return nil
	}
	return strings.Split(s, "\n")
}

func firstLine(s string) string {
	if i := strings.IndexByte(s, '\n'); i >= 0 {
		return strings.TrimSpace(s[:i])
	}
	return strings.TrimSpace(s)
}

var wsRe = regexp.MustCompile(`\s+`)

func oneLine(s string) string {
	return strings.TrimSpace(wsRe.ReplaceAllString(s, " "))
}

func truncate(s string, n int) string {
	runes := []rune(s)
	if len(runes) <= n {
		return s
	}
	return string(runes[:n])
}

func fatal(msg string) {
	fmt.Fprintln(os.Stderr, "claude_session: "+msg)
	os.Exit(1)
}
