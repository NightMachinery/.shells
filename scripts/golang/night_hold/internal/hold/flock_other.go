//go:build !unix

package hold

// Without flock the acquire is racy, as the shell version was. Every host this
// runs on is unix; this exists so the package still builds elsewhere.
func withResourceLock(dir string, fn func() error) error { return fn() }
