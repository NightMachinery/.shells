package codex

import (
	"bytes"
	"encoding/json"
	"os"
	"strings"
	"time"

	"agent_session/internal/preview"
	"agent_session/internal/session"
	"agent_session/internal/turns"
)

// The colour of a Codex thread's name in the preview.
const nameRGB = "120;200;120"

type previewData struct {
	meta   sessionMeta
	stamp  string
	model  string
	effort string
	prompt string
	reply  string
}

// Preview writes the fzf preview body: the thread's name, where and as what
// it ran, when it last moved, and what was last asked of it. The head gives
// the meta, the tail the rest.
func (Adapter) Preview(path string, o session.PreviewOpts) (string, error) {
	if _, err := os.Stat(path); err != nil {
		return "", err
	}
	c := preview.Painter{On: o.Color}
	l := preview.Layout{Compact: o.Compact}
	data := scanPreview(path, o.Bytes)
	id := data.meta.ID
	if id == "" {
		id = idOf(path)
	}

	w := &strings.Builder{}
	name := names(homeOf(path))[id]
	if name == "" {
		name = "Codex thread " + id
	}
	w.WriteString(c.BoldFg(nameRGB, name) + "\n")
	// A narrow pane cannot afford the uuid; the agent and the version are what
	// the line is for. See the same choice in the Claude adapter.
	subtitle := preview.JoinParts(" · ", id, "codex", preview.VersionLabel(data.meta.CLIVersion))
	if o.Compact {
		subtitle = preview.JoinParts(" · ", "codex", preview.VersionLabel(data.meta.CLIVersion))
	}
	w.WriteString(c.Gray(subtitle) + "\n")
	l.Gap(w)

	when, aside := "", ""
	if t, err := time.Parse(time.RFC3339, data.stamp); err == nil {
		when = t.Local().Format(turns.ListStamp)
		aside = "(" + preview.HumanAge(time.Since(t)) + " ago)"
	}

	effort := ""
	if data.effort != "" {
		effort = data.effort + " effort"
	}
	l.Row(w, c, "last activity", when, aside)
	l.Row(w, c, "model", preview.Annotate(data.model, " · ", effort), "")
	l.Row(w, c, "cwd", turns.AbbrevHome(data.meta.Cwd), "")

	l.Gap(w)
	l.Section(w, c, "last prompt", data.prompt, "none in the scanned tail")
	l.Section(w, c, "last reply", data.reply, "no answer in the scanned tail")
	return w.String(), nil
}

func scanPreview(path string, window int64) previewData {
	var data previewData
	data.meta, _ = readMeta(path)

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
	if window > size || window <= 0 {
		window = size
	}

	buf := make([]byte, window)
	if _, err := fh.ReadAt(buf, size-window); err != nil {
		return data
	}
	// A byte-tail starts mid-line; drop the partial line when it is a tail.
	if window < size {
		if i := bytes.IndexByte(buf, '\n'); i >= 0 {
			buf = buf[i+1:]
		} else {
			buf = nil
		}
	}

	for _, raw := range bytes.Split(buf, []byte("\n")) {
		raw = bytes.TrimSpace(raw)
		if len(raw) == 0 || raw[0] != '{' {
			continue
		}
		var l line
		if json.Unmarshal(raw, &l) != nil {
			continue
		}
		if l.Timestamp != "" {
			data.stamp = l.Timestamp
		}
		switch l.Type {
		case "turn_context":
			var tc turnContext
			if json.Unmarshal(l.Payload, &tc) == nil {
				if tc.Model != "" {
					data.model = tc.Model
				}
				if tc.Effort != "" {
					data.effort = tc.Effort
				}
			}
		case "response_item":
			// Both sides of the conversation, since the tail is already being
			// read and decoded: the user's last prompt and the model's last
			// answer. No status row goes with them -- see the note on `live`
			// -- but what was last said is in hand either way.
			var it responseItem
			if json.Unmarshal(l.Payload, &it) != nil || it.Type != "message" {
				continue
			}
			text := partsText(it.Content)
			if strings.TrimSpace(text) == "" {
				continue
			}
			switch {
			case it.Role == "user" && !scaffoldText(text):
				data.prompt = text
			case it.Role == "assistant":
				data.reply = text
			}
		}
	}

	data.prompt = turns.OneLine(data.prompt)
	data.reply = turns.OneLine(data.reply)
	return data
}
