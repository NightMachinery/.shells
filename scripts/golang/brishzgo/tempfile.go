package main

import (
	"os"
	"os/signal"
	"sync"
	"syscall"
)

// tempFiles are the temp files of this run, removed when it ends, as
// brishzq.zsh removes its own. The first one also traps SIGHUP, SIGINT and
// SIGTERM: such a signal removes them, then kills us with the same signal,
// so our parent still sees it. A run without temp files keeps the default
// signal handling.
type tempFiles struct {
	mu    sync.Mutex
	paths []string
	trap  sync.Once
}

var temps tempFiles

// create makes a new temp file in $TMPDIR, as mktemp does, only readable by
// us (and so by a local garden, which runs as us).
func (t *tempFiles) create(pattern string) (*os.File, error) {
	t.trap.Do(t.trapSignals)
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

func (t *tempFiles) trapSignals() {
	ch := make(chan os.Signal, 1)
	signal.Notify(ch, syscall.SIGHUP, syscall.SIGINT, syscall.SIGTERM)
	go func() {
		sig := <-ch
		t.removeAll()
		signal.Reset()
		syscall.Kill(os.Getpid(), sig.(syscall.Signal))
	}()
}
