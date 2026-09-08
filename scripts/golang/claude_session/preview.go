package main

// ** preview
//
// `claude_session preview <session.jsonl>` writes the fzf preview body for a
// session: what it is called, what it was running as, where, when it last
// moved, and what was last asked of it.
//
// This exists to make the preview cheap enough to run on every cursor move.
// The shell version tail-scanned with `tail -c | jq` and coloured its output
// with the repository's `colorfg'/`Bold'/`resetcolor', each of which forks for
// [agfi:h-color-p-override] -- about 25 of those at ~5ms apiece, 122ms of the
// 277ms it took. On top of that it reached fzf through the brish garden, whose
// round trip alone measured ~380ms. Total, about 600ms per keystroke.
//
// As one static binary fzf can exec directly there is no garden hop, no jq and
// no fork per colour: 15.4ms through fzf's dash, of which 10.7ms is this
// binary's own startup and 4.3ms the dash spawn, so the scan is about 5ms and
// does not grow with the transcript (26MB measures 16.1ms). The name precedence
// also stops being implemented twice -- `nameParts' now serves the preview as
// well as `name' and `list'.

import (
	"bytes"
	"encoding/json"
	"flag"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

// How much of the transcript's end is read. Everything worth previewing is
// written repeatedly, so the newest of each is near the end; a title older than
// this window falls back to the slug, which is the same trade `list` makes.
const previewWindow = 400 << 10

// The width the labels are padded to, so the values line up in a column.
const previewLabelWidth = 14

// What a session's name is coloured by: the config home it lives under. Which
// account a session belongs to is the thing worth seeing at a glance, since the
// whole point of [agfi:claude-code-session-resume] is that the two are
// otherwise interchangeable. A profile with no entry gets no colour rather than
// a wrong one.
//
// Not Claude Code's own `agent-color': it writes that record once, wherever in
// the session the name got settled, which is nowhere near either end of the
// file -- 210MB into a 398MB transcript in one measured case -- so a tail scan
// cannot reach it, and only 5 of 123 local sessions have one at all.
var profileColors = map[string]string{
	".claude":      "90;150;240",
	".claude-work": "235;145;60",
}

// Only the fields the preview shows. A narrow struct on purpose: decoding the
// full record would copy every message body in the window, and the only thing
// wanted out of `message` is the model.
//
// No `type` field, because none of these needs gating on one: each rides only
// on the records that mean it. That is not merely equivalent to keying on the
// type, it is better for `permissionMode`, which Claude Code writes on every
// `user` record as well as on its own `permission-mode` events -- 286 of the
// former locally -- so the newest of the two is more current than the last
// change event alone.
type previewRecord struct {
	Timestamp string `json:"timestamp"`

	nameFields

	LastPrompt     string `json:"lastPrompt"`
	PermissionMode string `json:"permissionMode"`
	Mode           string `json:"mode"`
	Cwd            string `json:"cwd"`
	GitBranch      string `json:"gitBranch"`
	Version        string `json:"version"`
	Effort         string `json:"effort"`

	Message *struct {
		Model string `json:"model"`
	} `json:"message"`
}

type previewData struct {
	name     nameParts
	prompt   string
	stamp    string
	permMode string
	mode     string
	cwd      string
	branch   string
	version  string
	effort   string
	model    string
}

func cmdPreview(argv []string) {
	fs := flag.NewFlagSet("preview", flag.ExitOnError)
	window := fs.Int("bytes", previewWindow,
		"how much of the transcript's tail to read; 0 or less reads all of it")
	colorP := fs.Bool("color", true, "emit ANSI colour")
	fs.Parse(guardPathArgs(fs, argv))

	if fs.NArg() == 0 {
		fatal("preview: no session file given")
	}
	path := fs.Arg(0)

	// NO_COLOR is the cross-tool convention, and costs one lookup to honour.
	// Its presence is what counts, whatever the value.
	_, noColor := os.LookupEnv("NO_COLOR")
	c := painter{on: *colorP && !noColor}

	data := scanPreview(path, int64(*window))

	w := &strings.Builder{}
	id := strings.TrimSuffix(filepath.Base(path), ".jsonl")
	profile := profileOf(path)

	name := data.name.resolve()
	if name == "" {
		name = "Claude Code session " + id
	}
	w.WriteString(c.boldFg(profileColors[profile], name) + "\n")

	w.WriteString(c.gray(joinParts(" · ", id, profile, versionLabel(data.version))) + "\n\n")

	// Every row is skipped when it has nothing to say, so a session predating a
	// field does not get a line of blanks for it.
	when, aside := "", ""
	if t, err := time.Parse(time.RFC3339, data.stamp); err == nil {
		when = t.Local().Format(listStamp)
		aside = "(" + humanAge(time.Since(t)) + " ago)"
	}

	// Two shapes of row, and the difference matters when a field is missing.
	// The permission mode and the interaction mode are peers, so a row with
	// only one of them still reads correctly. The effort and the branch are
	// annotations on the model and the cwd, and read as nonsense without them:
	// `cwd  topic' claims a branch name is a directory.
	effort := ""
	if data.effort != "" {
		effort = data.effort + " effort"
	}
	model := annotate(strings.TrimPrefix(data.model, "claude-"), " · ", effort)
	mode := joinParts(" · ", data.permMode, data.mode)
	where := annotate(abbrevHome(data.cwd), " @ ", data.branch)

	row(w, c, "last activity", when, aside)
	row(w, c, "model", model, "")
	row(w, c, "mode", mode, "")
	row(w, c, "cwd", where, "")

	w.WriteString("\n" + c.bold("last prompt") + "\n")
	if data.prompt == "" {
		w.WriteString("(none in the scanned tail)\n")
	} else {
		w.WriteString(data.prompt + "\n")
	}

	os.Stdout.WriteString(w.String())
}

// The non-empty parts, joined. Every field the preview shows is one Claude Code
// may simply not have written, and a row built by concatenation would then read
// as a stray separator or vanish because its first half was the missing one.
func joinParts(sep string, parts ...string) string {
	kept := make([]string, 0, len(parts))
	for _, p := range parts {
		if p != "" {
			kept = append(kept, p)
		}
	}
	return strings.Join(kept, sep)
}

// A principal with a note appended, or nothing at all: a note that cannot
// stand on its own is not worth a row of its own either.
func annotate(principal, sep, note string) string {
	if principal == "" {
		return ""
	}
	if note == "" {
		return principal
	}
	return principal + sep + note
}

func versionLabel(v string) string {
	if v == "" {
		return ""
	}
	return "v" + v
}

// One `<label>  <value>  <aside>' line, or nothing when there is no value.
func row(w *strings.Builder, c painter, label, value, aside string) {
	if value == "" {
		return
	}

	pad := label
	if n := previewLabelWidth - len(label); n > 0 {
		pad += strings.Repeat(" ", n)
	}

	w.WriteString(c.gray(pad) + value)
	if aside != "" {
		w.WriteString(" " + c.gray(aside))
	}
	w.WriteString("\n")
}

// The config home the transcript sits under -- .claude, .claude-work -- which
// is the same label `list` puts on its own relative paths.
func profileOf(path string) string {
	abs, err := filepath.Abs(path)
	if err != nil {
		abs = path
	}
	sep := string(filepath.Separator)
	if i := strings.Index(abs, sep+"projects"+sep); i >= 0 {
		return filepath.Base(abs[:i+1])
	}
	return ""
}

// A duration as its single largest unit: `44s', `6m', `3h', `2d'. What a
// preview answers is how stale the session is, so one unit is enough and a
// second would only be noise.
func humanAge(d time.Duration) string {
	secs := int64(d.Seconds())
	if secs < 0 {
		secs = 0
	}

	switch {
	case secs < 60:
		return strconv.FormatInt(secs, 10) + "s"
	case secs < 3600:
		return strconv.FormatInt(secs/60, 10) + "m"
	case secs < 86400:
		return strconv.FormatInt(secs/3600, 10) + "h"
	default:
		return strconv.FormatInt(secs/86400, 10) + "d"
	}
}

// Reads the tail once and keeps the newest of each field. Forward over the
// window, so "last one wins" needs no special handling: names get revised
// during a session and the last written is the current one, and a slug never
// changes so either end gives the same value.
func scanPreview(path string, window int64) previewData {
	var data previewData

	fh, err := os.Open(path)
	if err != nil {
		return data
	}
	defer fh.Close()

	st, err := fh.Stat()
	if err != nil {
		return data
	}

	size := st.Size()
	// A window of 0 or less means the whole file: the escape hatch for a
	// session whose title was written further back than the tail reaches.
	// Deliberately opt-in --- the largest transcript here is 398MB, and this
	// reads the window into memory in one piece.
	if window > size || window <= 0 {
		window = size
	}

	buf := make([]byte, window)
	if _, err := fh.ReadAt(buf, size-window); err != nil {
		return data
	}

	// A byte-tail necessarily starts on a partial line, so drop it -- but only
	// when the tail really is a tail. On a transcript smaller than the window
	// there is no partial line, and for a session of one or two records that
	// would throw away the only thing there was to read.
	if window < size {
		if i := bytes.IndexByte(buf, '\n'); i >= 0 {
			buf = buf[i+1:]
		} else {
			buf = nil
		}
	}

	set := func(dst *string, v string) {
		if v != "" {
			*dst = v
		}
	}

	for len(buf) > 0 {
		line := buf
		if i := bytes.IndexByte(buf, '\n'); i >= 0 {
			line, buf = buf[:i], buf[i+1:]
		} else {
			buf = nil
		}

		line = bytes.TrimSpace(line)
		if len(line) == 0 || line[0] != '{' {
			continue
		}

		var rec previewRecord
		if err := json.Unmarshal(line, &rec); err != nil {
			continue
		}

		data.name.observe(rec.nameFields, false)

		set(&data.prompt, rec.LastPrompt)
		set(&data.stamp, rec.Timestamp)
		set(&data.permMode, rec.PermissionMode)
		set(&data.mode, rec.Mode)
		set(&data.cwd, rec.Cwd)
		set(&data.branch, rec.GitBranch)
		set(&data.version, rec.Version)
		set(&data.effort, rec.Effort)
		if rec.Message != nil {
			set(&data.model, rec.Message.Model)
		}
	}

	data.prompt = oneLine(data.prompt)
	return data
}

// Wraps text in ANSI escapes, or does not. A value rather than a package flag
// so nothing can colour output the caller asked to be plain.
type painter struct{ on bool }

const grayRGB = "170;170;170"

func (c painter) fg(rgb, s string) string {
	if !c.on || rgb == "" || s == "" {
		return s
	}
	return "\x1b[38;2;" + rgb + "m" + s + "\x1b[0m"
}

func (c painter) gray(s string) string { return c.fg(grayRGB, s) }

// Bold and coloured in one escape. Nesting `bold(fg(...))` would work, but the
// inner reset ends the bold too, leaving two resets to say one thing.
func (c painter) boldFg(rgb, s string) string {
	if !c.on || s == "" {
		return s
	}
	if rgb == "" {
		return c.bold(s)
	}
	return "\x1b[1;38;2;" + rgb + "m" + s + "\x1b[0m"
}

func (c painter) bold(s string) string {
	if !c.on || s == "" {
		return s
	}
	return "\x1b[1m" + s + "\x1b[0m"
}
