package session

import (
	"bytes"
	"io"
	"os"
)

// Completion is bounded, plain conversation text for external completers.
// Newest message first in Corpus; LastReply retains real newlines.
type Completion struct {
	Corpus    []string `json:"corpus"`
	LastReply string   `json:"last_reply"`
}

func CompletionLines(path string) ([][]byte, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	st, err := f.Stat()
	if err != nil {
		return nil, err
	}
	n := st.Size()
	const window = 512 << 10
	start := int64(0)
	if n > window {
		start = n - window
	}
	b := make([]byte, n-start)
	if _, err = f.ReadAt(b, start); err != nil && err != io.EOF {
		return nil, err
	}
	if start > 0 {
		if i := bytes.IndexByte(b, '\n'); i >= 0 {
			b = b[i+1:]
		} else {
			b = nil
		}
	}
	lines := bytes.Split(b, []byte("\n"))
	for i, j := 0, len(lines)-1; i < j; i, j = i+1, j-1 {
		lines[i], lines[j] = lines[j], lines[i]
	}
	return lines, nil
}
