package main

import (
	"os"
	"sync"
)

// tempFiles are the temp files of this run, removed when it ends, as
// brishzq.zsh removes its own. The first one also has SIGHUP, SIGINT and
// SIGTERM remove them (see sigHandler), and then kill us with the same
// signal, so our parent still sees it. A run without temp files keeps the
// default signal handling, unless it streams.
type tempFiles struct {
	mu    sync.Mutex
	paths []string
	trap  sync.Once
}

var temps tempFiles

// create makes a new temp file in $TMPDIR, as mktemp does, only readable by
// us (and so by a local garden, which runs as us).
func (t *tempFiles) create(pattern string) (*os.File, error) {
	t.trap.Do(func() { sigs.add(t.removeAll) })
	// Held across the creation, so a signal cannot slip in between it
	// and the bookkeeping.
	t.mu.Lock()
	defer t.mu.Unlock()
	f, err := os.CreateTemp("", pattern)
	if err != nil {
		return nil, err
	}
	t.paths = append(t.paths, f.Name())
	return f, nil
}

func (t *tempFiles) removeAll() {
	t.mu.Lock()
	defer t.mu.Unlock()
	for _, p := range t.paths {
		os.Remove(p)
	}
	t.paths = nil
}
