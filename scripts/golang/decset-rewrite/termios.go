//go:build darwin || linux

package main

import "golang.org/x/sys/unix"

// copyTermios copies the line discipline settings of src onto dst. Without
// this the child sees a freshly opened pty with default settings, so its erase
// character, flow control and flags would differ from the terminal the user is
// actually sitting at.
func copyTermios(src, dst uintptr) error {
	t, err := unix.IoctlGetTermios(int(src), ioctlGetTermios)
	if err != nil {
		return err
	}
	return unix.IoctlSetTermios(int(dst), ioctlSetTermios, t)
}
