package agy

import (
	"os"
	"strings"
	"time"

	"agent_session/internal/preview"
	"agent_session/internal/session"
	"agent_session/internal/turns"
)

// The colour of an Antigravity conversation's name in the preview.
const nameRGB = "150;130;240"

// Preview writes the fzf preview body: what the conversation is called, which
// agent ran it, where, when it last moved, and what was last asked of it.
//
// The whole transcript is read rather than a tail of it. Unlike the other
// agents' stores these files are small -- a conversation is steps, not message
// bodies -- and the fields the preview wants (the agent's name, the workspace)
// come from the JSON mirror beside them, not from the transcript at all. The
// `-bytes` window is honoured anyway, for a conversation that grows past that.
func (Adapter) Preview(path string, o session.PreviewOpts) (string, error) {
	if _, err := os.Stat(path); err != nil {
		return "", err
	}
	c := preview.Painter{On: o.Color}
	home, id := homeOf(path), idOf(path)
	sum := summaries(home)[id]

	steps := readSteps(path)
	if o.Bytes > 0 {
		steps = withinBytes(path, steps, o.Bytes)
	}

	w := &strings.Builder{}
	name := nameOf(home, id)
	if name == "" {
		name = "Antigravity conversation " + id
	}
	w.WriteString(c.BoldFg(nameRGB, name) + "\n")
	w.WriteString(c.Gray(preview.JoinParts(" · ", id, "agy", sum.AgentName)) + "\n\n")

	when, aside := "", ""
	if t := lastStamp(steps); !t.IsZero() {
		when = t.Local().Format(turns.ListStamp)
		aside = "(" + preview.HumanAge(time.Since(t)) + " ago)"
	}

	steps_ := ""
	if n := len(steps); n > 0 {
		steps_ = itoa(n) + " steps"
	}

	preview.Row(w, c, "last activity", when, aside)
	preview.Row(w, c, "steps", steps_, "")
	preview.Row(w, c, "cwd", turns.AbbrevHome(cwdOf(home, id)), "")
	preview.Row(w, c, "last step", preview.JoinParts(" · ", typeLabel(lastType(steps)), statusNote(lastStatus(steps))), "")

	w.WriteString("\n" + c.Bold("last prompt") + "\n")
	if p := turns.OneLine(lastUserText(steps)); p == "" {
		w.WriteString("(none in this transcript)\n")
	} else {
		w.WriteString(p + "\n")
	}
	return w.String(), nil
}

// Keeps the steps whose lines fall within the last `window` bytes of the file,
// so `-bytes` means the same thing here as for the other agents.
func withinBytes(path string, steps []step, window int64) []step {
	st, err := os.Stat(path)
	if err != nil || st.Size() <= window {
		return steps
	}
	fh, err := os.Open(path)
	if err != nil {
		return steps
	}
	defer fh.Close()

	// Count the steps in the tail by counting its lines: the steps are in file
	// order, so the last n lines are the last n steps.
	buf := make([]byte, window)
	n, err := fh.ReadAt(buf, st.Size()-window)
	if err != nil && n == 0 {
		return steps
	}
	lines := 0
	for _, b := range buf[:n] {
		if b == '\n' {
			lines++
		}
	}
	if lines >= len(steps) || lines == 0 {
		return steps
	}
	return steps[len(steps)-lines:]
}

func lastType(steps []step) string {
	if len(steps) == 0 {
		return ""
	}
	return steps[len(steps)-1].Type
}

func lastStatus(steps []step) string {
	if len(steps) == 0 {
		return ""
	}
	return steps[len(steps)-1].Status
}

func lastUserText(steps []step) string {
	for i := len(steps) - 1; i >= 0; i-- {
		if steps[i].Source != sourceUser {
			continue
		}
		if text := userText(steps[i].Content); text != "" {
			return text
		}
	}
	return ""
}

func itoa(n int) string {
	if n == 0 {
		return "0"
	}
	neg := n < 0
	if neg {
		n = -n
	}
	var b [20]byte
	i := len(b)
	for n > 0 {
		i--
		b[i] = byte('0' + n%10)
		n /= 10
	}
	if neg {
		i--
		b[i] = '-'
	}
	return string(b[i:])
}
