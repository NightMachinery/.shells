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
	snippet string
}

func cmdList(argv []string) {
	fs := flag.NewFlagSet("list", flag.ExitOnError)
	snippetLen := fs.Int("snippet-len", 120, "max snippet width, in runes")
	subagentsP := fs.Bool("subagents", false, "also list subagent transcripts")
	jobs := fs.Int("jobs", runtime.NumCPU(), "worker count")
	fs.Parse(guardPathArgs(fs, argv))

	dir := fs.Arg(0)
	if dir == "" {
		fatal("list: no sessions directory given")
	}

	var files []string
	err := filepath.WalkDir(dir, func(p string, d os.DirEntry, err error) error {
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
		files = append(files, p)
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

	var slug, aiTitle, customTitle, agentName string
	for sc.Scan() {
		rec, ok := parseRecord(sc.Text())
		if !ok {
			continue
		}
		// Names can be revised during a session, so the last one wins; a slug
		// never changes, so the first is as good as any.
		switch {
		case rec.AgentName != "":
			agentName = rec.AgentName
		case rec.CustomTitle != "":
			customTitle = rec.CustomTitle
		case rec.AITitle != "":
			aiTitle = rec.AITitle
		case rec.Slug != "" && slug == "":
			slug = rec.Slug
		}
	}

	for _, s := range []string{agentName, customTitle, aiTitle, slug} {
		if s != "" {
			return s
		}
	}
	return ""
}
