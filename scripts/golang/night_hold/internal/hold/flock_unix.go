//go:build unix

package hold

import (
	"os"
	"syscall"
)

// withResourceLock serializes the read-check-write of an acquire.
//
// Without it two processes can both read "free" and both write their holder
// file, and an exclusive hold quietly has two holders. The shell version had
// this bug too; it is simply not expressible without a real lock.
//
// This is flock used for what flock is good at: a critical section inside one
// process's lifetime. It is not a contradiction of holds not being flocks --
// that objection is about the *hold*, which has to outlive the shell that took
// it, and a descriptor-scoped lock cannot. This lock lives for microseconds.
func withResourceLock(dir string, fn func() error) error {
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return err
	}
	f, err := os.OpenFile(dir+"/.lock", os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		// Fail open rather than refusing to work: a hold that races is still
		// better than no hold at all.
		return fn()
	}
	defer f.Close()

	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX); err != nil {
		// Fail open for the same reason as above: a racy hold beats none.
		return fn()
	}
	defer syscall.Flock(int(f.Fd()), syscall.LOCK_UN)

	return fn()
}
