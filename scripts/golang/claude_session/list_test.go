package main

import "testing"

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
			got := profileLabels(tc.roots)
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
