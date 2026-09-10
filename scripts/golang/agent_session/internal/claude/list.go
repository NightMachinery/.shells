package claude

import (
	"bufio"
	"bytes"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	"agent_session/internal/session"
	"agent_session/internal/turns"
)

// ** list

// List walks the roots for transcripts. Several roots, because Claude Code
// keeps one projects directory per config home and `claude-work` runs a second
// one; they are merged here rather than by the caller so the sort is over all
// of them at once.
//
// With a cwd, only that project's directory under each root is walked, and
// paths are relative to it -- the same rows the caller used to get by scoping
// the roots down itself.
func (Adapter) List(roots []string, o session.ListOpts) ([]session.Info, error) {
	if len(roots) == 0 {
		return nil, errors.New("list: no sessions directory given")
	}
	labels := session.ProfileLabels(roots)

	slug := ""
	if o.Cwd != "" {
		slug = projectSlug(o.Cwd)
	}

	// Which root a file came from travels with it: `rel` is relative to that
	// root (or its project directory), and carries its label when there is
	// more than one.
	type found struct{ path, base, root string }
	var files []found

	sep := string(filepath.Separator)
	if len(o.Only) > 0 {
		// The paths are already known, so nothing is walked. `base` is the
		// project directory when the caller scoped to a cwd and the root
		// otherwise, exactly as the walk below sets it.
		for _, pair := range session.Under(o.Only, roots) {
			base := pair[1]
			if slug != "" {
				base = filepath.Join(pair[1], slug)
			}
			files = append(files, found{path: pair[0], base: base, root: pair[1]})
		}
	}
	for _, root := range roots {
		if len(o.Only) > 0 {
			break
		}
		base := root
		if slug != "" {
			base = filepath.Join(root, slug)
			if st, err := os.Stat(base); err != nil || !st.IsDir() {
				continue
			}
		}
		err := filepath.WalkDir(base, func(p string, d os.DirEntry, err error) error {
			if err != nil {
				return nil
			}
			if d.IsDir() || !strings.HasSuffix(p, ".jsonl") {
				return nil
			}
			// Subagent transcripts live under `<session>/subagents/`. render
			// inlines them into their parent, so listing them next to real
			// sessions is just noise -- they were a third of the list.
			if !o.Subagents && strings.Contains(p, sep+"subagents"+sep) {
				return nil
			}
			files = append(files, found{path: p, base: base, root: root})
			return nil
		})
		if err != nil {
			return nil, err
		}
	}
	if len(files) == 0 {
		return nil, errors.New("list: no session files found in: " + strings.Join(roots, " "))
	}

	infos := make([]session.Info, len(files))
	session.ForEach(len(files), o.Jobs, func(i int) {
		f := files[i]
		info := scanSession(f.path, f.base, o.SnippetLen, o.NameLen)
		if l := labels[f.root]; l != "" {
			info.Rel = filepath.Join(l, info.Rel)
		}
		infos[i] = info
	})
	return infos, nil
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
func scanSession(path, root string, snippetLen, nameLen int) session.Info {
	info := session.Info{Path: path}
	if rel, err := filepath.Rel(root, path); err == nil {
		info.Rel = rel
	} else {
		info.Rel = filepath.Base(path)
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

	info.Epoch = last.Unix()
	info.Stamp = last.Local().Format(turns.ListStamp)
	info.Name = turns.Truncate(turns.OneLine(name.resolve()), nameLen)
	info.Snippet = turns.Truncate(turns.OneLine(snippet), snippetLen)
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
	sc.Buffer(make([]byte, 0, 64<<10), session.MaxLineBytes)

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

// ** name
//
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
func sessionName(fh *os.File) string {
	sc := bufio.NewScanner(fh)
	sc.Buffer(make([]byte, 0, 64<<10), session.MaxLineBytes)

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
