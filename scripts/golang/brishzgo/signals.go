package main

import (
	"os"
	"os/signal"
	"sync"
	"syscall"
	"time"
)

// sigHandler runs cleanups on SIGHUP, SIGINT and SIGTERM, then dies of the
// same signal, so our parent still sees it (a shell reports 129, 130 and
// 143). Two things register cleanups: the temp files (removed) and a
// streamed command (its connection closed, so the garden kills it).
//
// It is installed on first use, so a run that needs neither keeps Go's
// default handling. A signal that was ignored when we started stays
// ignored: a background job of a non-interactive shell starts with SIGINT
// ignored, so that a Ctrl-C meant for the script leaves it running, and
// asking to be told of SIGINT would undo that.
type sigHandler struct {
	once     sync.Once
	mu       sync.Mutex
	next     int
	cleanups map[int]func()
}

var sigs sigHandler

var trappedSignals = []os.Signal{syscall.SIGHUP, syscall.SIGINT, syscall.SIGTERM}

// add registers f to run when one of the signals arrives; the returned
// function unregisters it.
func (h *sigHandler) add(f func()) (remove func()) {
	h.once.Do(h.install)
	h.mu.Lock()
	defer h.mu.Unlock()
	if h.cleanups == nil {
		h.cleanups = map[int]func(){}
	}
	id := h.next
	h.next++
	h.cleanups[id] = f
	return func() {
		h.mu.Lock()
		delete(h.cleanups, id)
		h.mu.Unlock()
	}
}

func (h *sigHandler) install() {
	var trap []os.Signal
	for _, s := range trappedSignals {
		if !signal.Ignored(s) {
			trap = append(trap, s)
		}
	}
	if len(trap) == 0 {
		return
	}
	ch := make(chan os.Signal, 1)
	signal.Notify(ch, trap...)
	go func() {
		sig := <-ch
		h.mu.Lock()
		fs := make([]func(), 0, len(h.cleanups))
		for id := 0; id < h.next; id++ {
			if f, ok := h.cleanups[id]; ok {
				fs = append(fs, f)
			}
		}
		h.mu.Unlock()
		for _, f := range fs {
			f()
		}
		signal.Reset(trap...)
		s := sig.(syscall.Signal)
		syscall.Kill(os.Getpid(), s)
		// Still alive (the signal is blocked?): exit as a shell reports it.
		time.Sleep(time.Second)
		os.Exit(128 + int(s))
	}()
}

// awaitSignalDeath is for a goroutine whose work a signal's cleanup cut
// short: the signal handler is about to end the process with that signal,
// and returning an exit status of our own would race with it.
func awaitSignalDeath() {
	for {
		time.Sleep(time.Hour)
	}
}
