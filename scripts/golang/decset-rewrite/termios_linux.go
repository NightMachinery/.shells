//go:build linux

package main

import "golang.org/x/sys/unix"

// See termios_darwin.go; Linux spells the same two ioctls differently.
const (
	ioctlGetTermios = unix.TCGETS
	ioctlSetTermios = unix.TCSETS
)
