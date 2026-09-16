//go:build !unix

package hold

// Without a cheap liveness probe every hold runs to its deadline, which is the
// behaviour this started with and is always safe.
func processAlive(pid int) bool { return true }
