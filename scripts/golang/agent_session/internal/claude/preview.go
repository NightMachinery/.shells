package claude

// ** preview
//
// `agent_session claude preview <session.jsonl>` writes the fzf preview body
// for a session: what it is called, whether it is still running, what it was
// running as, where, when it last moved, and the last exchange of the
// conversation -- the last thing the user typed and the last thing the model
// said back.
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
	"os"
	"path/filepath"
	"strings"
	"time"

	"agent_session/internal/preview"
	"agent_session/internal/profiles"
	"agent_session/internal/session"
	"agent_session/internal/turns"
)

// How much of the transcript's end is read. Everything worth previewing is
// written repeatedly, so the newest of each is near the end; a title older than
// this window falls back to the slug, which is the same trade `list` makes.
const previewWindow = 400 << 10

// What a session's name is coloured by: the config home it lives under. Which
// account a session belongs to is the thing worth seeing at a glance, since the
// whole point of [agfi:claude-code-session-resume] is that the two are
// otherwise interchangeable. A profile with no entry gets no colour rather than
// a wrong one.
//
// The table itself is [profiles.PickerColors], generated from
// =configFiles/claude-code/profiles.yaml= so that the zsh launcher, the status
// line and this binary cannot disagree about which seat is which colour; the
// reasoning behind each value lives there, beside the value.
//
// Not Claude Code's own `agent-color': it writes that record once, wherever in
// the session the name got settled, which is nowhere near either end of the
// file -- 210MB into a 398MB transcript in one measured case -- so a tail scan
// cannot reach it, and only 5 of 123 local sessions have one at all.

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

	msgRecord
	nameFields

	LastPrompt     string `json:"lastPrompt"`
	PermissionMode string `json:"permissionMode"`
	Mode           string `json:"mode"`
	Cwd            string `json:"cwd"`
	GitBranch      string `json:"gitBranch"`
	Version        string `json:"version"`
	Effort         string `json:"effort"`
}

type previewData struct {
	name nameParts
	// What the user last typed, and what the assistant last said, both found
	// in the tail window; `record` is the `last-prompt` bookkeeping record,
	// which is only a fallback. See [scanPreview].
	typed    string
	reply    string
	record   string
	stamp    string
	permMode string
	mode     string
	cwd      string
	branch   string
	version  string
	effort   string
	model    string
}

func (Adapter) Preview(path string, o session.PreviewOpts) (string, error) {
	c := preview.Painter{On: o.Color}
	l := preview.Layout{Compact: o.Compact}

	// Started before the tail read and usually abandoned. The head scan is
	// only wanted when the tail turns out to hold no typed prompt, which is
	// rare, and it is bounded by `headWindow` and satisfied within the first
	// kilobyte in the ordinary case -- so running it beside the tail read
	// costs a goroutine and no wall clock, where running it afterwards would
	// add a second pass to the one preview that already had its answer.
	// Nothing waits on it when the tail answered: `main` returns and the
	// goroutine dies with the process.
	first := firstPromptAsync(path)

	data := scanPreview(path, o.Bytes)
	status := statusOf(path)

	w := &strings.Builder{}
	id := strings.TrimSuffix(filepath.Base(path), ".jsonl")
	profile := profileOf(path)

	name := data.name.resolve()
	if name == "" {
		name = "Claude Code session " + id
	}
	// The status emoji goes outside the colour escape: it is not part of the
	// name, and a bold hourglass is no more legible than a plain one.
	w.WriteString(statusEmoji(status) + c.BoldFg(profiles.PickerColors[profile], name) + "\n")

	// The uuid goes first when the pane is narrow: it is the longest thing on
	// the line by far, and the least worth reading of the three -- the profile
	// says which seat this is, and the row it was picked from carried the id.
	subtitle := preview.JoinParts(" · ", id, profile, preview.VersionLabel(data.version))
	if o.Compact {
		subtitle = preview.JoinParts(" · ", profile, preview.VersionLabel(data.version))
	}
	w.WriteString(c.Gray(subtitle) + "\n")
	l.Gap(w)

	// Every row is skipped when it has nothing to say, so a session predating a
	// field does not get a line of blanks for it.
	when, aside := "", ""
	if t, err := time.Parse(time.RFC3339, data.stamp); err == nil {
		when = t.Local().Format(turns.ListStamp)
		aside = "(" + preview.HumanAge(time.Since(t)) + " ago)"
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
	model := preview.Annotate(strings.TrimPrefix(data.model, "claude-"), " · ", effort)
	mode := preview.JoinParts(" · ", data.permMode, data.mode)
	where := preview.Annotate(turns.AbbrevHome(data.cwd), " @ ", data.branch)

	// Only in the ordinary layout: a compact pane reads the status off the
	// emoji on the name line, and cannot spare a row to spell it out.
	if !o.Compact {
		l.Row(w, c, "status", status, "")
	}
	l.Row(w, c, "last activity", when, aside)
	l.Row(w, c, "model", model, "")
	l.Row(w, c, "mode", mode, "")
	l.Row(w, c, "cwd", where, "")

	l.Gap(w)
	l.Section(w, c, promptLabel(data), promptText(data, first), "nothing asked yet")
	l.Section(w, c, "last reply", turns.OneLine(data.reply), "no answer in the scanned tail")

	return w.String(), nil
}

// Which prompt the preview is about to show, in the order [promptText] finds
// them.
func promptLabel(data previewData) string {
	if data.typed == "" && data.record == "" {
		return "first prompt"
	}
	return "last prompt"
}

// What was last asked, in decreasing order of authority:
//
//   - the last message the user typed inside the tail window, which is what
//     the section claims to show and is present in nearly every transcript;
//   - the `last-prompt` bookkeeping record, which repeats one prompt but is
//     written rarely -- of 77 local transcripts every one has such a record
//     and only 2 have one inside a 400KB tail, which is why it cannot be the
//     primary source;
//   - the *first* thing the session was asked, from the head scan, for a tail
//     that is all tool traffic. A session identifies itself better by what it
//     was started for than by an empty section; the heading says which it is.
func promptText(data previewData, first <-chan string) string {
	switch {
	case data.typed != "":
		return data.typed
	case data.record != "":
		return data.record
	}
	return <-first
}

// The live status as one character on the name line: an hourglass for a
// session working, a sleep sign for one waiting on its user, nothing at all
// for a transcript with no live session behind it.
func statusEmoji(status string) string {
	switch status {
	case "busy":
		return "⏳ "
	case "idle":
		return "💤 "
	}
	return ""
}

// The status Claude Code records for the session this transcript belongs to,
// or "" when it is not running.
//
// The record is `<config-home>/sessions/<pid>.json`, the same file `live`
// reads, matched on its `sessionId`: a session writes one while it runs and
// removes it on exit, so a transcript with no matching record is a finished
// session. A crash leaves the file behind, so [recordAlive] checks the pid --
// with no process table, because the point of doing this here is that it costs
// a handful of small reads. On a platform where the kernel cannot be asked
// directly that means trusting the record, which is the same trade `live`
// makes when the table cannot be read.
func statusOf(path string) string {
	home := configHome(path)
	if home == "" {
		return ""
	}
	id := strings.TrimSuffix(filepath.Base(path), ".jsonl")

	for _, rec := range readSessionRecords(home) {
		if rec.SessionID != id || rec.PID == 0 || !recordAlive(rec, nil) {
			continue
		}
		return rec.Status
	}
	return ""
}

// firstPromptAsync starts the head scan and hands back where its answer will
// arrive. The channel is buffered, so a caller that never reads it leaks
// nothing.
func firstPromptAsync(path string) <-chan string {
	out := make(chan string, 1)
	go func() {
		fh, err := os.Open(path)
		if err != nil {
			out <- ""
			return
		}
		defer fh.Close()
		out <- turns.OneLine(firstUserText(fh))
	}()
	return out
}

// The config home the transcript sits under -- .claude, .claude-work -- which
// is the same label `list` puts on its own relative paths.
func profileOf(path string) string {
	home := configHome(path)
	if home == "" {
		return ""
	}
	return filepath.Base(home)
}

// The config home directory itself: everything above the `projects` directory
// a transcript lives in. `~/.claude` for
// `~/.claude/projects/-Users-e-scripts/<uuid>.jsonl`. That is where the live
// session records sit, so the same rule that names the profile also finds
// them -- a work session must not be matched against the personal seat's
// records, and vice versa.
func configHome(path string) string {
	abs, err := filepath.Abs(path)
	if err != nil {
		abs = path
	}
	sep := string(filepath.Separator)
	if i := strings.Index(abs, sep+"projects"+sep); i > 0 {
		return abs[:i]
	}
	return ""
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

		// The conversation itself, as opposed to the bookkeeping around it.
		// Both are "last one wins" like every other field here, and both are
		// allowed to find nothing: a message that is only a system reminder
		// or only a tool call reduces to "" and leaves the previous one
		// standing, which is what makes the sections show something a person
		// would recognise rather than the newest record's scaffolding.
		switch {
		case rec.typed():
			set(&data.typed, snippetText(rec.text()))
		case rec.Type == "assistant" && !rec.IsMeta:
			set(&data.reply, rec.text())
		}

		set(&data.record, rec.LastPrompt)
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

	data.typed = turns.OneLine(data.typed)
	data.record = turns.OneLine(data.record)
	return data
}
