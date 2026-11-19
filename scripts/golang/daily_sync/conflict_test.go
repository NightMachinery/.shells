package main

import (
	"bytes"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"
)

// Table tests of the engine's parts; corpus_test.go checks the whole against
// the perl engine's outputs.

func TestWeak(t *testing.T) {
	for line, want := range map[string]bool{
		"\n": true, "   \t\n": true, "abc\n": true, " abc \n": true, "abcd\n": false,
		"äb\n": true, "日本\n": false, // bytes, not characters
		"#+begin_verse\n": true, "#+END_SRC\n": true, "#+begin_src python\n": false,
		"#+title:\n": true, "#+TITLE\n": true, "#+title: x\n": false, "#+key::\n": false,
		":PROPERTIES:\n": true, ":a b:\n": false, "**\n": true, "*** -\n": true, "** _\n": true,
		"*  -\n": false, "**_\n": true, "abc\v\n": true, "abc\u00a0\n": false,
		"a real line\n": false,
	} {
		if got := weak(line); got != want {
			t.Errorf("weak(%q) = %v, want %v", line, got, want)
		}
	}
}

func TestIsMarker(t *testing.T) {
	for _, c := range []struct {
		line  string
		c     byte
		label bool
		want  bool
	}{
		{"<<<<<<< HEAD\n", '<', true, true}, {"<<<<<<<\n", '<', true, true}, {"<<<<<<<", '<', true, true},
		{"<<<<<<<\r\n", '<', true, true}, {"<<<<<<<\r", '<', true, true}, {"<<<<<<<\r\r\n", '<', true, false},
		{"<<<<<<<<\n", '<', true, false}, {"<<<<<<<x\n", '<', true, false}, {"<<<<<<\n", '<', true, false},
		{"=======\n", '=', false, true}, {"======= x\n", '=', false, false}, {"=======x\n", '=', false, false},
		{"||||||| base\n", '|', true, true}, {">>>>>>> c\n", '>', true, true},
	} {
		if got := isMarker(c.line, c.c, c.label); got != c.want {
			t.Errorf("isMarker(%q) = %v, want %v", c.line, got, c.want)
		}
	}
}

func TestParseOrder(t *testing.T) {
	text := "x\n<<<<<<< a\nA\n||||||| b\nB\n=======\nC\n>>>>>>> c\ny\n"
	_, h := parseConflicts(splitLines(text), true)
	if len(h) != 1 || h[0].up[0] != "A\n" || h[0].loc[0] != "C\n" || h[0].base[0] != "B\n" || h[0].line != 2 || !h[0].based {
		t.Fatalf("rebase: %+v", h[0])
	}
	_, h = parseConflicts(splitLines(text), false)
	if h[0].up[0] != "C\n" || h[0].loc[0] != "A\n" {
		t.Fatalf("merge: %+v", h[0])
	}
}

func mk(up, base, loc string, based bool) *hunk {
	return &hunk{up: splitLinesE(up), base: splitLinesE(base), loc: splitLinesE(loc), based: based}
}

func splitLinesE(s string) []string { return orEmpty(splitLines(s)) }

func TestClass(t *testing.T) {
	for _, c := range []struct {
		h    *hunk
		want string
	}{
		{mk("a b\n", "x\n", "a  b \n", true), "SAME-IGNORING-WHITESPACE"},
		{mk("a\n", "", "b\n", false), "BASE-NOT-SHOWN"},
		{mk("a  b\n", "a b\n", "c\n", true), "UPSTREAM-ONLY-WHITESPACE"},
		{mk("c\n", "a b\n", "a\tb\n", true), "LOCAL-ONLY-WHITESPACE"},
		{mk("k\nup\n", "k\n", "k\nloc\n", true), "BOTH-ONLY-ADDED"},
		{mk("up\n", "", "loc\n", true), "BOTH-ONLY-ADDED"},
		{mk("k2\n", "k\n", "k\nloc\n", true), "ONE-EDITED-OTHER-ADDED"},
		{mk("k up\n", "k\n", "k loc\n", true), "BOTH-EDITED"},
		{mk("", "k\n", "k loc\n", true), "BOTH-EDITED"},
	} {
		if got := class(c.h); got != c.want {
			t.Errorf("class(up %q base %q loc %q) = %s, want %s", c.h.up, c.h.base, c.h.loc, got, c.want)
		}
	}
}

func TestBoth(t *testing.T) {
	for _, c := range []struct {
		h               *hunk
		both, localLast string
	}{
		// Both added an entry.
		{mk("entry one\nremote entry\n", "entry one\n", "entry one\nlocal entry\n", true), "entry one\nremote entry\nlocal entry\n", "local entry\nentry one\nremote entry\n"},
		// A short line matched alone proves nothing, so local's copy is kept.
		{mk("e1\nremote entry\n", "e1\n", "e1\nlocal entry\n", true), "e1\nremote entry\ne1\nlocal entry\n", "e1\nlocal entry\ne1\nremote entry\n"},
		// Entries spaced by blank lines get a blank line between the sides.
		{mk("\nremote entry\n", "", "\nlocal entry\n", true), "\nremote entry\n\nlocal entry\n", "\nlocal entry\n\nremote entry\n"},
		// Local deleted a base line upstream kept: it stays deleted.
		{mk("keep me\ndrop me\nnew up\n", "keep me\ndrop me\n", "keep me\n", true), "keep me\nnew up\n", "keep me\nnew up\n"},
		// A blank line and #+begin_verse elsewhere in upstream do not swallow local's.
		{mk("#+begin_verse\nold poem\n#+end_verse\n\n", "", "#+begin_verse\nnew poem\n#+end_verse\n\n", true),
			"#+begin_verse\nold poem\n#+end_verse\n\n#+begin_verse\nnew poem\n#+end_verse\n\n",
			"#+begin_verse\nnew poem\n#+end_verse\n\n#+begin_verse\nold poem\n#+end_verse\n\n"},
		// But a run holding a real line is already there.
		{mk("intro\nshared real line\n\n", "", "shared real line\n\n", true), "intro\nshared real line\n\n", "intro\nshared real line\n\n"},
	} {
		if got := strings.Join(joinBlank(upKept(c.h), localAdded(c.h)), ""); got != c.both {
			t.Errorf("both(up %q base %q loc %q) = %q, want %q", c.h.up, c.h.base, c.h.loc, got, c.both)
		}
		if got := strings.Join(joinBlank(localAdded(c.h), upKept(c.h)), ""); got != c.localLast {
			t.Errorf("both-local-first(up %q base %q loc %q) = %q, want %q", c.h.up, c.h.base, c.h.loc, got, c.localLast)
		}
	}
}

func TestLCSFallback(t *testing.T) {
	// Past 2,000,000 cells every q line p has anywhere matches, out of order too.
	p := make([]string, 1500)
	q := make([]string, 1500)
	for i := range p {
		p[i], q[i] = fmt.Sprint("p", i), fmt.Sprint("q", i)
	}
	p[0], q[1499], p[1499], q[0] = "x", "x", "y", "y"
	hit := matched(p, q)
	if !hit[0] || !hit[1499] {
		t.Fatalf("set membership should match both swapped lines: %v %v", hit[0], hit[1499])
	}
	pairs := lcsPairs(p[:1000], q[:1000])
	if len(pairs) != 0 {
		t.Fatalf("no common lines in the first 1000: %v", pairs)
	}
}

func TestShowLines(t *testing.T) {
	var b bytes.Buffer
	showLines(&b, []string{"a\n", "b\r\n", "c\r\r\n", "d", "e\r", "f\n", "g\n"}, 6)
	want := "  | a\n  | b\n  | c\r\n  | d\n  | e\n  | f\n  ... 1 more lines\n"
	if b.String() != want {
		t.Fatalf("got %q, want %q", b.String(), want)
	}
}

func TestParseSpec(t *testing.T) {
	for s, want := range map[string]string{
		"1=local": "1 local", "all=both": "all both", "01=upstream": "01 upstream", "2=both-local-first\n": "2 both-local-first",
		"1=Local": "", "1=local\n\n": "", "=local": "", "a=local": "", "1=local=x": "", "-1=local": "", "1 =local": "",
	} {
		k, c, ok := parseSpec(s)
		got := ""
		if ok {
			got = k + " " + c
		}
		if got != want {
			t.Errorf("parseSpec(%q) = %q, want %q", s, got, want)
		}
	}
}

func TestResolveWrites(t *testing.T) {
	path := filepath.Join(t.TempDir(), "f.org")
	must(t, os.WriteFile(path, []byte("top\n<<<<<<< a\nup\n||||||| b\nbase\n=======\nloc\n>>>>>>> c\nmid\n<<<<<<< a\nu2\n=======\nl2\n>>>>>>> c\n"), 0o640))
	var out, errb bytes.Buffer
	if rc := runConflict([]string{"resolve", "rebase", "box", path, "notes/f.org", "1=local"}, &out, &errb); rc != 0 {
		t.Fatalf("exit %d: %s", rc, errb.String())
	}
	b, _ := os.ReadFile(path)
	if want := "top\nloc\nmid\n<<<<<<< a\nu2\n=======\nl2\n>>>>>>> c\n"; string(b) != want {
		t.Fatalf("file %q", b)
	}
	if want := "notes/f.org hunk 1 -> local, now reads:\n  | loc\nnotes/f.org: 1 hunk(s) still unresolved\n"; out.String() != want {
		t.Fatalf("stdout %q", out.String())
	}
	if st, _ := os.Stat(path); st.Mode().Perm() != 0o640 {
		t.Fatalf("mode changed: %v", st.Mode())
	}
}

// The embedded source is the source on disk: the version names what was built.
func TestSourceHash(t *testing.T) {
	ents, err := os.ReadDir(".")
	must(t, err)
	var names []string
	for _, e := range ents {
		n := e.Name()
		if (strings.HasSuffix(n, ".go") && !strings.HasSuffix(n, "_test.go")) || n == "go.mod" {
			names = append(names, n)
		}
	}
	sort.Strings(names)
	h := sha256.New()
	for _, n := range names {
		b, err := os.ReadFile(n)
		must(t, err)
		fmt.Fprintf(h, "%s\x00%d\x00", n, len(b))
		h.Write(b)
	}
	if want := hex.EncodeToString(h.Sum(nil))[:16]; sourceHash() != want {
		t.Fatalf("sourceHash() = %s, want %s", sourceHash(), want)
	}
}
