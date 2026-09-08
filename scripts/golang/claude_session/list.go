package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"sort"
	"strings"
	"sync"
	"time"
)

// ** list

type sessionInfo struct {
	path    string
	rel     string
	epoch   int64
	stamp   string
	name    string
	snippet string
}

func cmdList(argv []string) {
	fs := flag.NewFlagSet("list", flag.ExitOnError)
	snippetLen := fs.Int("snippet-len", 120, "max snippet width, in runes")
	nameLen := fs.Int("name-len", 40, "max session-name width, in runes")
	subagentsP := fs.Bool("subagents", false, "also list subagent transcripts")
	jobs := fs.Int("jobs", runtime.NumCPU(), "worker count")
	fs.Parse(guardPathArgs(fs, argv))

	// Several roots, because Claude Code keeps one projects directory per
	// config home and `claude-work` runs a second one. Merged here rather than
	// by the caller, so the sort below is over all of them at once.
	roots := fs.Args()
	if len(roots) == 0 {
		fatal("list: no sessions directory given")
	}
	labels := profileLabels(roots)

	// Which root a file came from travels with it: `rel` is relative to that
	// root, and carries its label when there is more than one.
	type found struct{ path, root string }
	var files []found

	for _, root := range roots {
		err := filepath.WalkDir(root, func(p string, d os.DirEntry, err error) error {
			if err != nil {
				return nil
			}
			if d.IsDir() || !strings.HasSuffix(p, ".jsonl") {
				return nil
			}
			// Subagent transcripts live under `<session>/subagents/`. render
			// inlines them into their parent, so listing them next to real
			// sessions is just noise -- they were a third of the list.
			sep := string(filepath.Separator)
			if !*subagentsP && strings.Contains(p, sep+"subagents"+sep) {
				return nil
			}
			files = append(files, found{path: p, root: root})
			return nil
		})
		if err != nil {
			fatal(err.Error())
		}
	}
	if len(files) == 0 {
		fatal("list: no session files found in: " + strings.Join(roots, " "))
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
				f := files[idx]
				info := scanSession(f.path, f.root, *snippetLen, *nameLen)
				if l := labels[f.root]; l != "" {
					info.rel = filepath.Join(l, info.rel)
				}
				infos[idx] = info
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
		fmt.Fprintf(w, "%d\t%s\t%s\t%s\t%s\t%s\n",
			s.epoch, s.path, s.stamp, s.name, s.rel, s.snippet)
	}
}

// How each root is labelled in the listing, keyed by root. The relative path
// alone is ambiguous the moment more than one root is listed: every profile
// has a -Users-evar-scripts/ under it, and in project scope the roots end in
// that same component.
//
// So the label is the first path component in which the roots actually
// differ. For ~/.claude/projects and ~/.claude-work/projects that is .claude
// and .claude-work, and it stays right when the caller scopes the roots down
// to one project apiece -- which taking a fixed component would not: their
// parent is then "projects" for both.
//
// Empty for a single root, so single-root output is unchanged.
func profileLabels(roots []string) map[string]string {
	labels := make(map[string]string, len(roots))
	if len(roots) < 2 {
		return labels
	}

	split := make([][]string, len(roots))
	shortest := -1
	for i, r := range roots {
		split[i] = strings.Split(filepath.Clean(r), string(filepath.Separator))
		if shortest < 0 || len(split[i]) < shortest {
			shortest = len(split[i])
		}
	}

	at := 0
	for ; at < shortest; at++ {
		same := true
		for i := 1; i < len(split); i++ {
			if split[i][at] != split[0][at] {
				same = false
				break
			}
		}
		if !same {
			break
		}
	}

	// One root is a prefix of another, so there is no differing component for
	// the shorter one; its own last component is the best it has.
	for i, r := range roots {
		idx := at
		if idx >= len(split[i]) {
			idx = len(split[i]) - 1
		}
		labels[r] = split[i][idx]
	}
	return labels
}

// How much of the file's end is searched for the last message's timestamp and
// the session's name, how far back the name search will widen at most, and how
// far into the start the first user message is looked for. The tail window
// grows on demand, so these only decide how much is read in the common case.
//
// The name search alone is capped. Missing a timestamp means the session gets
// dated by mtime, which is wrong in a way worth reading the whole file to
// avoid; missing a title only means falling back to the slug, and a session
// that has no title anywhere would otherwise pull a 26MB transcript through
// here to establish that.
const (
	tailWindow = 64 << 10
	nameWindow = 1 << 20
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
func scanSession(path, root string, snippetLen, nameLen int) sessionInfo {
	info := sessionInfo{path: path}
	if rel, err := filepath.Rel(root, path); err == nil {
		info.rel = rel
	} else {
		info.rel = filepath.Base(path)
	}

	var last time.Time
	var name nameParts
	var snippet string

	if fh, err := os.Open(path); err == nil {
		defer fh.Close()
		if st, err := fh.Stat(); err == nil {
			last, name = scanTail(fh, st.Size())
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
	info.name = truncate(oneLine(name.resolve()), nameLen)
	info.snippet = truncate(oneLine(snippet), snippetLen)
	return info
}

// How many timestamped messages to look back over. Records are written in
// order, so the last one almost always wins; the slack only has to cover the
// millisecond-scale reordering that does occur in practice.
const tailRecords = 25

// Only the type, the timestamp and the name fields are needed here. Decoding
// into the full record would copy every message body in the window for
// nothing.
type tailRecord struct {
	Type      string `json:"type"`
	Timestamp string `json:"timestamp"`

	nameFields
}

// Newest user/assistant timestamp and the session's name, found by walking
// backwards from the end of the file and widening the window until both turn
// up.
//
// Both come out of the one buffer. The timestamp walk already visits every
// line in the window and has already paid to decode it, so noticing the name
// records it passes costs nothing beyond the four extra fields of
// `nameFields`.
func scanTail(fh *os.File, size int64) (time.Time, nameParts) {
	for window := int64(tailWindow); ; window *= 4 {
		if window > size {
			window = size
		}

		buf := make([]byte, window)
		if _, err := fh.ReadAt(buf, size-window); err != nil {
			return time.Time{}, nameParts{}
		}

		var last time.Time
		var name nameParts
		seen := 0

		// Backwards, line by line, so a long transcript costs the same as a
		// short one.
		end := len(buf)
		for end > 0 {
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
			var rec tailRecord
			if err := json.Unmarshal([]byte(line), &rec); err != nil {
				continue
			}

			// Reading backwards, the first name of each kind we meet is the
			// last one written, which is the rule the titles want.
			name.observe(rec.nameFields, true)

			if rec.Type != "user" && rec.Type != "assistant" {
				continue
			}
			// Records are written in order, so the newest timestamp is within
			// the first few messages met; the rest of the window is walked
			// only for the name.
			if seen >= tailRecords {
				continue
			}
			if t, err := time.Parse(time.RFC3339, rec.Timestamp); err == nil {
				seen++
				if t.After(last) {
					last = t
				}
			}
		}

		switch {
		case !last.IsZero() && name.hasTitle():
			return last, name
		case window >= size:
			return last, name
		case window >= nameWindow && !last.IsZero():
			// Only the title is still missing, and it is not worth widening
			// any further for; `resolve` falls back to the slug.
			return last, name
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
			if b.Type != "text" {
				continue
			}
			if s := snippetText(b.Text); s != "" {
				return s
			}
		}
	}
	return ""
}

var commandNameRe = regexp.MustCompile(`(?s)<command-name>\s*(.*?)\s*</command-name>`)

// Harness scaffolding that carries no meaning in a one-line preview. RE2 has
// no backreferences, so each pair is spelled out.
var scaffoldTags = []string{
	"command-name", "command-message", "command-args", "command-contents",
	"local-command-stdout", "local-command-stderr", "system-reminder",
}

var tagRe = func() *regexp.Regexp {
	alts := make([]string, len(scaffoldTags))
	for i, t := range scaffoldTags {
		alts[i] = "<" + t + ">.*?</" + t + ">"
	}
	return regexp.MustCompile(`(?s)` + strings.Join(alts, "|"))
}()

// What to show for a session in the picker. A message that is only harness
// scaffolding is worth nothing there: a third of the list read
// `<command-name>/clear</command-name> <command-message>clear</command-message>`
// until this reduced it to `/clear`, and a message that is nothing but a
// system reminder is skipped so the next real one can be shown instead.
func snippetText(text string) string {
	name := ""
	if m := commandNameRe.FindStringSubmatch(text); m != nil {
		name = m[1]
	}

	stripped := strings.TrimSpace(tagRe.ReplaceAllString(text, ""))
	if strings.HasPrefix(stripped, "Caveat:") {
		// The local-command caveat preamble, which is boilerplate.
		_, rest, _ := strings.Cut(stripped, "\n")
		stripped = strings.TrimSpace(rest)
	}

	switch {
	case stripped != "" && name != "":
		return name + " " + stripped
	case stripped != "":
		return stripped
	case name != "":
		return name
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

// Claude Code names a session several ways, in increasing order of authority:
// a generated slug like `sharded-bouncing-clarke`, an `ai-title` summarising
// the work, and a `custom-title` the user set. `agent-name` is the name Claude
// Code itself resolved from those, so it wins when present — it agrees with the
// rule below on every local session that has one, and preferring it means a
// future title source is picked up without changing this code. Only 19 of 28
// named sessions carry one, though, so the explicit precedence has to stay.
//
// Sessions predating all of it have no name, and the caller falls back to the
// uuid.
func cmdName(argv []string) {
	if len(argv) == 0 {
		fatal("name: no input file given")
	}

	fh, err := os.Open(argv[0])
	if err != nil {
		fatal(err.Error())
	}
	defer fh.Close()

	if s := sessionName(fh); s != "" {
		fmt.Println(s)
	}
}

func sessionName(fh *os.File) string {
	sc := bufio.NewScanner(fh)
	sc.Buffer(make([]byte, 0, 64<<10), maxLineBytes)

	var name nameParts
	for sc.Scan() {
		rec, ok := parseRecord(sc.Text())
		if !ok {
			continue
		}
		name.observe(rec.nameFields, false)
	}
	return name.resolve()
}

// What a transcript said about its own name, gathered as it is read. Shared by
// the full scan above and the tail scan in `list`, so the picker's column and
// the name a viewer writes into a filename cannot disagree.
type nameParts struct {
	slug        string
	aiTitle     string
	customTitle string
	agentName   string
}

// Each record names the session in at most one of these ways -- the titles
// have record types of their own, and only ordinary message records carry a
// slug -- so all four are taken independently rather than in precedence order.
//
// `keepFirst` says which occurrence to keep. Names can be revised during a
// session, so the last one written wins: that is the last occurrence when
// reading forwards and the first when reading backwards. A slug never changes,
// so either end of the file gives the same one.
func (n *nameParts) observe(f nameFields, keepFirst bool) {
	set := func(dst *string, v string) {
		if v == "" || (keepFirst && *dst != "") {
			return
		}
		*dst = v
	}

	set(&n.agentName, f.AgentName)
	set(&n.customTitle, f.CustomTitle)
	set(&n.aiTitle, f.AITitle)
	set(&n.slug, f.Slug)
}

// Whether a title was seen, as opposed to only a slug. A slug rides on nearly
// every message record, so finding one proves nothing about how far back the
// scan has looked -- which is what the widening in `scanTail` needs to know.
func (n nameParts) hasTitle() bool {
	return n.agentName != "" || n.customTitle != "" || n.aiTitle != ""
}

func (n nameParts) resolve() string {
	for _, s := range []string{n.agentName, n.customTitle, n.aiTitle, n.slug} {
		if s != "" {
			return s
		}
	}
	return ""
}
