package progress

import (
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func readReport(t *testing.T, file string) string {
	t.Helper()
	b, err := os.ReadFile(file)
	if err != nil {
		t.Fatal(err)
	}
	return strings.TrimSpace(string(b))
}

// The phases share the percentage by weight, each phase's own unit maps onto
// its share, and Finish leaves a complete report behind.
func TestPhasesAndFinish(t *testing.T) {
	file := filepath.Join(t.TempDir(), "p")
	Enable(file, []Phase{{"read", 1}, {"pandoc", 3}})
	defer func() { path = "" }()

	Begin("read", 1000)
	if _, err := io.Copy(io.Discard, Reader(strings.NewReader(strings.Repeat("x", 500)))); err != nil {
		t.Fatal(err)
	}
	write()
	if got := readReport(t, file); got != "12 read 500 1000" {
		t.Errorf("half of the first quarter: got %q", got)
	}

	Begin("pandoc", 4)
	Add(2)
	write()
	if got := readReport(t, file); got != "62 pandoc 2 4" {
		t.Errorf("half of the remaining three quarters: got %q", got)
	}

	Add(10)
	write()
	if got := readReport(t, file); got != "100 pandoc 4 4" {
		t.Errorf("overshoot is clamped: got %q", got)
	}

	Finish()
	if got := readReport(t, file); got != "100 done 1 1" {
		t.Errorf("finish: got %q", got)
	}
}

// Disabled, everything is a no-op and Reader hands back what it was given.
func TestDisabled(t *testing.T) {
	path = ""
	r := strings.NewReader("x")
	if Reader(r) != io.Reader(r) {
		t.Error("Reader wrapped a reader with reporting off")
	}
	Begin("read", 1)
	Add(1)
	Finish()
}
