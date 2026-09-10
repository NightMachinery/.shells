package profiles

import (
	"crypto/sha256"
	"encoding/hex"
	"os"
	"strings"
	"testing"
)

// Where the source and the other two generated files sit, relative to this
// package. Generating three files in three languages from one YAML is only
// safe if staleness is detectable, and this is the check: the generator writes
// the source digest into each output, so one hash comparison covers all of
// them without any consumer needing a YAML parser.
const (
	sourcePath = "../../../../configFiles/claude-code/profiles.yaml"
	zshPath    = "../../../../zshlang/auto-load/others/claude-profiles.gen.zsh"
	bashPath   = "../../../../configFiles/claude-code/profiles.gen.bash"
)

func sourceDigest(t *testing.T) string {
	t.Helper()

	raw, err := os.ReadFile(sourcePath)
	if err != nil {
		t.Fatalf("reading the profile source: %v", err)
	}
	sum := sha256.Sum256(raw)
	return hex.EncodeToString(sum[:])
}

func TestGeneratedFromCurrentSource(t *testing.T) {
	want := sourceDigest(t)

	if SourceSHA256 != want {
		t.Errorf("profiles_gen.go is stale: records %s, source hashes to %s\n"+
			"run agent-profiles-sync (python/agent_profiles_gen.py)", SourceSHA256, want)
	}

	// The zsh tables and the bash the status line sources come from the same
	// generator run, so a mismatch there is the same fault and deserves the
	// same failure rather than being discovered by eye months later.
	for _, f := range []struct{ what, path string }{
		{"the zsh tables", zshPath},
		{"the status line tables", bashPath},
	} {
		raw, err := os.ReadFile(f.path)
		if err != nil {
			t.Errorf("reading %s: %v", f.what, err)
			continue
		}
		if !strings.Contains(string(raw), want) {
			t.Errorf("%s (%s) is stale: does not record source digest %s\n"+
				"run agent-profiles-sync (python/agent_profiles_gen.py)", f.what, f.path, want)
		}
	}
}

func TestTablesArePopulated(t *testing.T) {
	// A generator bug that produced empty maps would otherwise be invisible
	// until a picker rendered with no colour at all, which reads as a styling
	// choice rather than a fault.
	if len(Order) == 0 {
		t.Fatal("Order is empty")
	}
	if len(Markers) != len(Order) {
		t.Errorf("Markers has %d entries for %d seats: %v", len(Markers), len(Order), Markers)
	}
	if len(PickerColors) == 0 {
		t.Error("PickerColors is empty; every seat is meant to be painted in the pickers")
	}

	for home, colour := range PickerColors {
		if !strings.HasPrefix(home, ".") {
			t.Errorf("PickerColors key %q is not a config home basename", home)
		}
		// The value is spliced into an SGR sequence, so anything but digits
		// and semicolons would emit a broken escape.
		if strings.TrimLeft(colour, "0123456789;") != "" {
			t.Errorf("PickerColors[%q] = %q is not an R;G;B triplet", home, colour)
		}
	}
}
