package session

import (
	"flag"
	"os"
	"path/filepath"
	"testing"
)

// The label has to survive both scopes the picker uses: the profile roots
// themselves, and those roots scoped down to a single project, where every
// root ends in the same component.
func TestProfileLabels(t *testing.T) {
	cases := []struct {
		name  string
		roots []string
		want  map[string]string
	}{
		{
			name:  "single root is unlabelled",
			roots: []string{"/Users/e/.claude/projects"},
			want:  map[string]string{},
		},
		{
			name:  "profile roots",
			roots: []string{"/Users/e/.claude/projects", "/Users/e/.claude-work/projects"},
			want: map[string]string{
				"/Users/e/.claude/projects":      ".claude",
				"/Users/e/.claude-work/projects": ".claude-work",
			},
		},
		{
			name: "scoped to one project apiece",
			roots: []string{
				"/Users/e/.claude/projects/-Users-e-scripts",
				"/Users/e/.claude-work/projects/-Users-e-scripts",
			},
			want: map[string]string{
				"/Users/e/.claude/projects/-Users-e-scripts":      ".claude",
				"/Users/e/.claude-work/projects/-Users-e-scripts": ".claude-work",
			},
		},
		{
			name:  "trailing slashes do not shift the component",
			roots: []string{"/Users/e/.claude/projects/", "/Users/e/.claude-work/projects"},
			want: map[string]string{
				"/Users/e/.claude/projects/":     ".claude",
				"/Users/e/.claude-work/projects": ".claude-work",
			},
		},
		{
			name:  "one root a prefix of the other",
			roots: []string{"/a/b", "/a/b/c"},
			want: map[string]string{
				"/a/b":   "b",
				"/a/b/c": "c",
			},
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := ProfileLabels(tc.roots)
			if len(got) != len(tc.want) {
				t.Fatalf("got %d labels %v, want %d %v", len(got), got, len(tc.want), tc.want)
			}
			for k, want := range tc.want {
				if got[k] != want {
					t.Errorf("%s: got %q, want %q", k, got[k], want)
				}
			}
		})
	}
}

// Paths inside ~/.claude/projects start with a dash, since project
// directories are named after the cwd they belong to.
func TestGuardPathArgs(t *testing.T) {
	dir := t.TempDir()
	dashed := filepath.Join(dir, "-Users-evar-scripts.jsonl")
	if err := os.WriteFile(dashed, []byte("{}\n"), 0o600); err != nil {
		t.Fatal(err)
	}

	cwd, _ := os.Getwd()
	defer os.Chdir(cwd)
	if err := os.Chdir(dir); err != nil {
		t.Fatal(err)
	}

	fs := flag.NewFlagSet("render", flag.ContinueOnError)
	format := fs.String("format", "md", "")
	diff := fs.Bool("diff", true, "")

	argv := []string{"-format=org", "-diff=false", "-Users-evar-scripts.jsonl"}
	if err := fs.Parse(GuardPathArgs(fs, argv)); err != nil {
		t.Fatalf("parse: %v", err)
	}

	if *format != "org" || *diff {
		t.Errorf("real flags stopped parsing: format=%q diff=%v", *format, *diff)
	}
	if got := fs.Arg(0); got != "./-Users-evar-scripts.jsonl" {
		t.Errorf("path argument = %q, want it spelled with a ./ prefix", got)
	}
}

// A file named like a flag must not shadow the flag.
func TestGuardPathArgsLeavesRealFlagsAlone(t *testing.T) {
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "-diff"), []byte("x"), 0o600); err != nil {
		t.Fatal(err)
	}
	cwd, _ := os.Getwd()
	defer os.Chdir(cwd)
	os.Chdir(dir)

	fs := flag.NewFlagSet("render", flag.ContinueOnError)
	diff := fs.Bool("diff", false, "")
	if err := fs.Parse(GuardPathArgs(fs, []string{"-diff"})); err != nil {
		t.Fatalf("parse: %v", err)
	}
	if !*diff {
		t.Error("-diff was treated as a path instead of a flag")
	}
}
