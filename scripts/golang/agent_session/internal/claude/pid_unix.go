//go:build unix

package claude

import (
	"errors"
	"syscall"
)

// pidAlive reports whether a process with this pid exists, and whether the
// platform could answer at all.
//
// Signal 0 runs the kernel's existence and permission checks without
// delivering anything: no error means the process is there, ESRCH means it is
// gone, and EPERM means it is there but owned by somebody else -- which a
// session started under a different account would be, so EPERM counts as
// alive. This is stdlib and needs no cgo.
func pidAlive(pid int) (alive, ok bool) {
	if pid <= 0 {
		return false, true
	}
	err := syscall.Kill(pid, 0)
	if err == nil {
		return true, true
	}
	return errors.Is(err, syscall.EPERM), true
}
