package agy

import (
	"errors"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	"agent_session/internal/session"
	"agent_session/internal/turns"
)

// Adapter implements [session.Adapter] for Antigravity.
type Adapter struct{}

var _ session.Adapter = Adapter{}

func (Adapter) Name(path string) (string, error) {
	if _, err := os.Stat(path); err != nil {
		return "", err
	}
	return nameOf(homeOf(path), idOf(path)), nil
}

func (Adapter) Meta(path string) (session.Meta, error) {
	if _, err := os.Stat(path); err != nil {
		return session.Meta{}, err
	}
	home, id := homeOf(path), idOf(path)
	return session.Meta{ID: id, Name: nameOf(home, id), Cwd: cwdOf(home, id)}, nil
}

// Document turns the steps into the renderer's model. What the user typed and
// what the model answered are turns; every other step is an event under the
// turn it followed, titled by its own kind, so a step type this code has never
// heard of still reads correctly.
func (a Adapter) Document(path string, o session.DocOpts) (*turns.Document, error) {
	if _, err := os.Stat(path); err != nil {
		return nil, err
	}
	steps := readSteps(path)

	doc := &turns.Document{Turns: buildTurns(steps), Results: map[string]turns.ToolResult{}}
	if o.Subagents {
		brain := filepath.Dir(filepath.Dir(filepath.Dir(filepath.Dir(path))))
		for _, sub := range subConversations(brain, steps) {
			doc.Subagents = append(doc.Subagents, subdoc(sub))
		}
	}
	return doc, nil
}

func buildTurns(steps []step) []turns.Turn {
	var out []turns.Turn

	attach := func(role, ts string, b turns.Block) {
		tb := turns.TimedBlock{B: b, TS: ts}
		if n := len(out); n > 0 && out[n-1].Role == role && out[n-1].Heading == "" {
			out[n-1].Blocks = append(out[n-1].Blocks, tb)
			return
		}
		out = append(out, turns.Turn{Role: role, TS: ts, Blocks: []turns.TimedBlock{tb}})
	}
	// An event belongs to whatever turn it followed, except a user's: the
	// harness is speaking for the agent, so a step that follows a prompt opens
	// the agent's turn rather than being filed under the person who asked.
	event := func(ts string, b turns.Block) {
		tb := turns.TimedBlock{B: b, TS: ts}
		if n := len(out); n > 0 && out[n-1].Role != "user" {
			out[n-1].Blocks = append(out[n-1].Blocks, tb)
			return
		}
		out = append(out, turns.Turn{Role: "assistant", TS: ts, Blocks: []turns.TimedBlock{tb}})
	}

	for _, s := range steps {
		switch {
		case s.Source == sourceUser:
			text := userText(s.Content)
			if text == "" {
				continue
			}
			attach("user", s.CreatedAt, turns.Block{Type: "text", Text: text})
			continue

		case s.Type == typeConversationLog:
			// A summary of the conversation so far, which is Antigravity's
			// compaction: its own turn, so the phases either side of it read
			// as separate.
			out = append(out, turns.Turn{
				Role: "system", Heading: "Conversation history", TS: s.CreatedAt,
				Blocks: []turns.TimedBlock{{B: turns.Block{Type: "notice", Name: "Summary", Text: s.Content}, TS: s.CreatedAt}},
			})
			continue
		}

		if s.Thinking != "" {
			attach("assistant", s.CreatedAt, turns.Block{Type: "thinking", Thinking: s.Thinking})
		}

		switch {
		case s.Type == typePlanner && s.Content != "":
			// The agent talking: markdown, like any other message.
			attach("assistant", s.CreatedAt, turns.Block{Type: "text", Text: s.Content})

		case s.Content != "" || len(s.ToolCalls) == 0:
			// Anything else with something to show: a labelled event, whose
			// body is output rather than prose.
			name := typeLabel(s.Type)
			if st := statusNote(s.Status); st != "" {
				name += " · " + st
			}
			if s.Content == "" && s.Thinking != "" {
				// The thinking above was the whole step.
				break
			}
			// A search summary is markdown and a command's output is not, so
			// the block type follows the step rather than being fixed. See
			// [proseStep].
			kind := "event"
			if proseStep(s.Type, s.Content) {
				kind = "notice"
			}
			event(s.CreatedAt, turns.Block{Type: kind, Name: name, Text: s.Content})
		}

		for _, c := range s.ToolCalls {
			b := turns.Block{Type: "tool_use", Name: c.Name, Input: callArgs(c.Args)}
			if b.Name == "" {
				b.Name = typeLabel(s.Type)
			}
			attach("assistant", s.CreatedAt, b)
		}
	}

	return out
}

// A step's status, when it says something a reader needs. DONE is the ordinary
// case and would be noise on every heading.
func statusNote(status string) string {
	switch strings.ToUpper(status) {
	case "", "DONE":
		return ""
	case "RUNNING":
		return "running"
	case "ERROR":
		return "error"
	default:
		return strings.ToLower(status)
	}
}

var uuidRe = regexp.MustCompile(`[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}`)

// The conversations this one spawned: the ids named by its subagent steps that
// have a transcript of their own under the same brain directory. Read out of
// the step rather than from a documented field, because which key carries the
// child's id is not documented -- and inlining only ever happens when the
// directory it names is really there.
func subConversations(brain string, steps []step) []string {
	seen := map[string]bool{}
	var out []string

	for _, s := range steps {
		if s.Type != typeSubagent {
			continue
		}
		hay := s.Content
		for _, c := range s.ToolCalls {
			hay += " " + string(c.Args)
		}
		for _, id := range uuidRe.FindAllString(hay, -1) {
			if seen[id] {
				continue
			}
			seen[id] = true
			if t := transcriptOf(filepath.Join(brain, id)); t != "" {
				out = append(out, t)
			}
		}
	}
	return out
}

func subdoc(path string) turns.Subdoc {
	home, id := homeOf(path), idOf(path)

	title := "Subagent " + id
	if name := nameOf(home, id); name != "" {
		title = name
	}
	if agent := summaries(home)[id].AgentName; agent != "" {
		title = agent + " · " + title
	}

	return turns.Subdoc{Title: title, Turns: buildTurns(readSteps(path)), Results: map[string]turns.ToolResult{}}
}

// List walks `<root>/*/` for conversations that have written a transcript. A
// brain directory without one is not a session: there is nothing to show.
func (Adapter) List(roots []string, o session.ListOpts) ([]session.Info, error) {
	if len(roots) == 0 {
		return nil, errors.New("list: no brain directory given")
	}
	labels := session.ProfileLabels(roots)

	cwd := ""
	if o.Cwd != "" {
		cwd = filepath.Clean(o.Cwd)
	}

	type found struct{ path, root string }
	var files []found
	if len(o.Only) > 0 {
		// The paths are known, so the brain directory is not read. See
		// [session.ListOpts.Only].
		for _, pair := range session.Under(o.Only, roots) {
			files = append(files, found{path: pair[0], root: pair[1]})
		}
	} else {
		for _, root := range roots {
			entries, err := os.ReadDir(root)
			if err != nil {
				continue
			}
			for _, e := range entries {
				if !e.IsDir() {
					continue
				}
				if t := transcriptOf(filepath.Join(root, e.Name())); t != "" {
					files = append(files, found{path: t, root: root})
				}
			}
		}
	}
	if len(files) == 0 {
		return nil, errors.New("list: no conversation transcripts under: " + strings.Join(roots, " "))
	}

	infos := make([]session.Info, len(files))
	keep := make([]bool, len(files))
	session.ForEach(len(files), o.Jobs, func(i int) {
		f := files[i]
		home, id := homeOf(f.path), idOf(f.path)

		if cwd != "" && filepath.Clean(cwdOf(home, id)) != cwd {
			return
		}

		info := session.Info{Path: f.path}
		if rel, err := filepath.Rel(f.root, f.path); err == nil {
			info.Rel = rel
		} else {
			info.Rel = id
		}
		if l := labels[f.root]; l != "" {
			info.Rel = filepath.Join(l, info.Rel)
		}

		steps := readSteps(f.path)
		last := lastStamp(steps)
		if last.IsZero() {
			if st, err := os.Stat(f.path); err == nil {
				last = st.ModTime()
			}
		}
		info.Epoch = last.Unix()
		info.Stamp = last.Local().Format(turns.ListStamp)
		info.Name = turns.Truncate(turns.OneLine(nameOf(home, id)), o.NameLen)
		info.Snippet = turns.Truncate(turns.OneLine(firstUserText(steps)), o.SnippetLen)

		infos[i] = info
		keep[i] = true
	})

	out := make([]session.Info, 0, len(infos))
	for i, k := range keep {
		if k {
			out = append(out, infos[i])
		}
	}
	if len(out) == 0 && cwd != "" {
		return nil, errors.New("list: no conversations ran in " + cwd)
	}
	return out, nil
}

func lastStamp(steps []step) time.Time {
	var last time.Time
	for i := len(steps) - 1; i >= 0; i-- {
		if t, err := time.Parse(time.RFC3339, steps[i].CreatedAt); err == nil {
			if t.After(last) {
				last = t
			}
			break
		}
	}
	return last
}

func firstUserText(steps []step) string {
	for _, s := range steps {
		if s.Source != sourceUser {
			continue
		}
		if text := userText(s.Content); text != "" {
			return text
		}
	}
	return ""
}
