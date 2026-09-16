package main

import (
	"testing"
)

// flag.Parse stops at the first non-flag argument, so `acquire <resource> --ttl
// 10m` parsed nothing and every flag silently took its default: the hold was
// taken for the wrong duration, by the wrong holder, in the wrong mode. The
// package tests could not see this -- it is entirely in the CLI layer.
func TestParsePositionalAcceptsFlagsOnEitherSide(t *testing.T) {
	cases := []struct {
		name     string
		args     []string
		resource string
		ttl      string
		shared   bool
		matches  int
	}{
		{"flags after", []string{"repo:~/x", "--ttl", "10m", "--shared"}, "repo:~/x", "10m", true, 0},
		{"flags before", []string{"--ttl", "10m", "--shared", "repo:~/x"}, "repo:~/x", "10m", true, 0},
		{"flags either side", []string{"--ttl", "10m", "repo:~/x", "--shared"}, "repo:~/x", "10m", true, 0},
		{"repeated match", []string{"repo:~/x", "--match", "a", "--match", "b"}, "repo:~/x", "", false, 2},
		{"no flags", []string{"gpu:0"}, "gpu:0", "", false, 0},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			fs := flagsFor("acquire")
			ttl := fs.String("ttl", "", "")
			shared := fs.Bool("shared", false, "")
			var matches matchList
			fs.Var(&matches, "match", "")

			got, err := parsePositional(fs, c.args)
			if err != nil {
				t.Fatal(err)
			}
			if got != c.resource {
				t.Errorf("resource = %q, want %q", got, c.resource)
			}
			if *ttl != c.ttl {
				t.Errorf("ttl = %q, want %q", *ttl, c.ttl)
			}
			if *shared != c.shared {
				t.Errorf("shared = %v, want %v", *shared, c.shared)
			}
			if len(matches) != c.matches {
				t.Errorf("matches = %v, want %d", matches, c.matches)
			}
		})
	}
}

func TestParsePositionalRejectsASecondArgument(t *testing.T) {
	fs := flagsFor("acquire")
	if _, err := parsePositional(fs, []string{"repo:~/x", "repo:~/y"}); err == nil {
		t.Error("two resources should be an error, not a silently ignored one")
	}
}
