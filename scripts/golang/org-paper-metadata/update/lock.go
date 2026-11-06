package update

import (
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"syscall"
)

// WriteLock serialises --write runs on one machine.
//
// It is a flock(2) lock on a fixed file. The kernel drops it when the holding
// process exits for any reason, SIGKILL and crashes included, so the lock can
// never go stale and there is nothing to clean up: a file left behind by a
// dead run is not a lock, and the next run simply takes it. A live holder,
// even a hung one, keeps it, because it may still write; kill that process.
// Never delete the file while a run holds it: the next run would lock a fresh
// file and both would proceed.
//
// The file descriptor is close-on-exec, so children the updater spawns do not
// inherit the lock and cannot keep it alive after the updater itself dies.
type WriteLock struct{ f *os.File }

// LockedError reports that another live process holds the lock.
type LockedError struct{ Path, Holder string }

func (e *LockedError) Error() string {
	holder := e.Holder
	if holder == "" {
		holder = "holder not recorded yet"
	}
	return fmt.Sprintf("another updater holds the write lock %s (%s); it is released when that process exits", e.Path, holder)
}

// LockPathEnv overrides DefaultLockPath, mainly for tests.
const LockPathEnv = "org_paper_metadata_updater_lock"

// DefaultLockPath is one lock per machine, not per root: overlapping roots
// such as ~/notes and ~/notes/public edit the same files, and Semantic
// Scholar's rate limit is shared by every run on the machine.
func DefaultLockPath() (string, error) {
	if p := os.Getenv(LockPathEnv); p != "" {
		return p, nil
	}
	state := os.Getenv("XDG_STATE_HOME")
	if state == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			return "", err
		}
		state = filepath.Join(home, ".local", "state")
	}
	return filepath.Join(state, "org-paper-metadata", "updater-write.lock"), nil
}

// AcquireWriteLock takes the lock without waiting. It returns *LockedError
// when a live process holds it. holder is recorded in the file so that a
// refused run can say who is in the way.
func AcquireWriteLock(path, holder string) (*WriteLock, error) {
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return nil, err
	}
	f, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE, 0o600)
	if err != nil {
		return nil, err
	}
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		defer f.Close()
		if errors.Is(err, syscall.EWOULDBLOCK) {
			note, _ := io.ReadAll(f)
			return nil, &LockedError{Path: path, Holder: strings.TrimSpace(string(note))}
		}
		return nil, fmt.Errorf("locking %s: %w", path, err)
	}
	if err := f.Truncate(0); err == nil {
		_, _ = f.WriteAt([]byte(holder+"\n"), 0)
	}
	return &WriteLock{f: f}, nil
}

// Release clears the holder note and drops the lock.
func (l *WriteLock) Release() error {
	if l == nil || l.f == nil {
		return nil
	}
	_ = l.f.Truncate(0)
	err := l.f.Close()
	l.f = nil
	return err
}
