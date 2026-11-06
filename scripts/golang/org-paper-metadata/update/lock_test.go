package update

import (
	"context"
	"errors"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestWriteLockRefusesSecondHolder(t *testing.T) {
	path := filepath.Join(t.TempDir(), "sub", "updater-write.lock")
	first, err := AcquireWriteLock(path, "pid 1 first")
	if err != nil {
		t.Fatal(err)
	}
	_, err = AcquireWriteLock(path, "pid 2 second")
	var locked *LockedError
	if !errors.As(err, &locked) {
		t.Fatalf("second acquire: got %v, want LockedError", err)
	}
	if locked.Holder != "pid 1 first" || !strings.Contains(err.Error(), "pid 1 first") {
		t.Fatalf("holder not reported: %q", err)
	}
	if err := first.Release(); err != nil {
		t.Fatal(err)
	}
	again, err := AcquireWriteLock(path, "pid 3 third")
	if err != nil {
		t.Fatalf("acquire after release: %v", err)
	}
	defer again.Release()
}

// A holder that dies without releasing leaves the file but not the lock.
func TestWriteLockLeftoverFileIsNotALock(t *testing.T) {
	path := filepath.Join(t.TempDir(), "updater-write.lock")
	if err := os.WriteFile(path, []byte("pid 99 crashed\n"), 0o600); err != nil {
		t.Fatal(err)
	}
	l, err := AcquireWriteLock(path, "pid 100 next")
	if err != nil {
		t.Fatalf("leftover file blocked the lock: %v", err)
	}
	defer l.Release()
	note, _ := os.ReadFile(path)
	if string(note) != "pid 100 next\n" {
		t.Fatalf("holder note = %q", note)
	}
}

// The real failure mode: another process holds the lock and is SIGKILLed.
// The holder is this test binary re-executed into TestLockHolderHelper.
func TestWriteLockReleasedWhenHolderIsKilled(t *testing.T) {
	path := filepath.Join(t.TempDir(), "updater-write.lock")
	holder := exec.Command(os.Args[0], "-test.run=^TestLockHolderHelper$")
	holder.Env = append(os.Environ(), "LOCK_HOLDER_HELPER_PATH="+path)
	ready, err := holder.StdoutPipe()
	if err != nil {
		t.Fatal(err)
	}
	if err := holder.Start(); err != nil {
		t.Fatal(err)
	}
	defer holder.Process.Kill()
	buf := make([]byte, 6)
	if _, err := io.ReadFull(ready, buf); err != nil || string(buf) != "locked" {
		t.Fatalf("helper did not report the lock: %q %v", buf, err)
	}
	if _, err := AcquireWriteLock(path, "probe"); err == nil {
		t.Fatal("lock not held by the helper")
	}
	if err := holder.Process.Kill(); err != nil {
		t.Fatal(err)
	}
	_ = holder.Wait()
	l, err := AcquireWriteLock(path, "after kill")
	if err != nil {
		t.Fatalf("lock survived its SIGKILLed holder: %v", err)
	}
	l.Release()
}

func TestLockHolderHelper(t *testing.T) {
	path := os.Getenv("LOCK_HOLDER_HELPER_PATH")
	if path == "" {
		t.Skip("helper process only")
	}
	if _, err := AcquireWriteLock(path, "helper"); err != nil {
		os.Exit(3)
	}
	os.Stdout.WriteString("locked")
	time.Sleep(time.Minute)
}

func waitTestHolder() string { return "waiter" }

func TestWaitWriteLock(t *testing.T) {
	const poll = 5 * time.Millisecond
	path := filepath.Join(t.TempDir(), "updater-write.lock")
	held, err := AcquireWriteLock(path, "holder")
	if err != nil {
		t.Fatal(err)
	}
	var locked *LockedError

	start := time.Now()
	if _, err := WaitWriteLock(context.Background(), path, waitTestHolder, 0, poll, nil); !errors.As(err, &locked) || time.Since(start) > time.Second {
		t.Fatalf("wait 0 must refuse at once: %v", err)
	}
	if _, err := WaitWriteLock(context.Background(), path, waitTestHolder, 30*time.Millisecond, poll, nil); !errors.As(err, &locked) {
		t.Fatalf("timed wait must end with LockedError: %v", err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	time.AfterFunc(30*time.Millisecond, cancel)
	if _, err := WaitWriteLock(ctx, path, waitTestHolder, -1, poll, nil); !errors.Is(err, context.Canceled) {
		t.Fatalf("cancelled wait: %v", err)
	}

	for _, wait := range []time.Duration{-1, 10 * time.Second} {
		var told []string
		current := held
		time.AfterFunc(40*time.Millisecond, func() { current.Release() })
		l, err := WaitWriteLock(context.Background(), path, waitTestHolder, wait, poll, func(e *LockedError) { told = append(told, e.Holder) })
		if err != nil {
			t.Fatalf("wait %v: %v", wait, err)
		}
		if len(told) != 1 || told[0] != "holder" {
			t.Fatalf("wait %v: waiting reports %q, want one for the holder", wait, told)
		}
		l.Release()
		if held, err = AcquireWriteLock(path, "holder"); err != nil {
			t.Fatal(err)
		}
	}
	held.Release()
}
