//go:build !unix

package claude

// pidAlive cannot ask the kernel here: there is no signal 0 to send. It
// declines to answer and [recordAlive] falls back to the shared process table.
func pidAlive(pid int) (alive, ok bool) {
	return false, false
}
