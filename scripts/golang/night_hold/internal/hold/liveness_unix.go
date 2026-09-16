//go:build unix

package hold

import "syscall"

// processAlive reports whether a pid is still running.
//
// EPERM means it exists and is not ours, which is still alive. Only ESRCH --
// no such process -- is death.
func processAlive(pid int) bool {
	if pid <= 0 {
		return false
	}
	err := syscall.Kill(pid, 0)
	if err == nil {
		return true
	}
	return err != syscall.ESRCH
}
