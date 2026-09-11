//go:build darwin

package main

import "golang.org/x/sys/unix"

// The termios ioctl request numbers differ per OS, and x/term's State is
// opaque, so we go to x/sys/unix directly to copy the outer terminal's
// settings onto the pty slave.
const (
	ioctlGetTermios = unix.TIOCGETA
	ioctlSetTermios = unix.TIOCSETA
)
