package main

// The corpus test: the Go conflict engine must give exactly what the perl
// engine it replaced gave (stdout, exit status and the rewritten file) on
// every file in testdata/corpus/inputs, under both orders and a range of
// picks. testdata/corpus/golden holds perl's results; TestCorpus checks Go
// against them, so it keeps running now that perl is gone from daily-sync.
// The goldens are one gzipped JSON file (zcat it to read), keyed by input;
// inputs over 20 KB are gzipped too.
//
// Regenerating (needs git and perl; testdata/reference.pl is the perl engine):
//
//	DS_CORPUS_INPUTS=1 go test -run TestCorpusInputs   # stage repositories, rewrite the inputs
//	DS_CORPUS_GOLDEN=1 go test -run TestCorpusGolden   # run perl, rewrite the goldens
//	DS_CORPUS_PERL=1   go test -run TestCorpusPerl     # check perl still gives the goldens
//
// Rewriting the goldens is only right while they are perl's: once Go is the
// engine, a golden change is a behaviour change and needs a reason.

import (
	"bytes"
	"compress/gzip"
	"crypto/sha256"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"unicode/utf8"
)

const corpusDir = "testdata/corpus"

// specSets are the picks every input is resolved with.
var specSets = [][]string{
	{},
	{"all=upstream"},
	{"all=local"},
	{"all=both"},
	{"all=both-local-first"},
	{"1=both", "2=local", "3=both-local-first"},
	{"1=both-local-first"},
	{"2=upstream", "all=both"},
	{"all=local", "1=both"},
	{"1=local", "1=upstream"},
	{"01=local", "99=both"},
	{"1=local\n"},
	{"1=local", "bogus"},
	{"1=Local"},
	{"all=both\n\n"},
	{"0=both", "x=both", "all=nothing"},
}

// blob is text that survives JSON byte for byte: a string when it is valid
// UTF-8, else {"base64": ...}.
type blob string

func (b blob) MarshalJSON() ([]byte, error) {
	if utf8.ValidString(string(b)) {
		return json.Marshal(string(b))
	}
	return json.Marshal(map[string]string{"base64": base64.StdEncoding.EncodeToString([]byte(b))})
}

func (b *blob) UnmarshalJSON(d []byte) error {
	var s string
	if err := json.Unmarshal(d, &s); err == nil {
		*b = blob(s)
		return nil
	}
	var m map[string]string
	if err := json.Unmarshal(d, &m); err != nil {
		return err
	}
	r, err := base64.StdEncoding.DecodeString(m["base64"])
	*b = blob(r)
	return err
}

// goldenRun is one engine call and what it gave. File is the file after the
// call, or its sha256 when it is long.
type goldenRun struct {
	Args   []string `json:"args"`
	Exit   int      `json:"exit"`
	Stdout blob     `json:"stdout"`
	File   blob     `json:"file"`
}

func fileDigest(s string) blob {
	if len(s) <= 1000 {
		return blob(s)
	}
	h := sha256.Sum256([]byte(s))
	return blob("sha256:" + hex.EncodeToString(h[:]))
}

// corpusArgs are the calls made on one input; the path goes after the label.
func corpusArgs(input string) [][]string {
	var out [][]string
	name := "notes/" + input
	for _, order := range []string{"rebase", "merge"} {
		out = append(out, []string{"show", order, "testbox", name})
		for _, s := range specSets {
			out = append(out, append([]string{"resolve", order, "testbox", name}, s...))
		}
	}
	return out
}

// engine runs one call on a fresh copy of input and returns the result.
type engine func(t *testing.T, args []string, path string) (string, int)

func goEngine(t *testing.T, args []string, path string) (string, int) {
	var out, errb bytes.Buffer
	full := append([]string{args[0], args[1], args[2], path}, args[3:]...)
	rc := runConflict(full, &out, &errb)
	return out.String(), rc
}

func perlEngine(t *testing.T, args []string, path string) (string, int) {
	full := append([]string{filepath.Join("testdata", "reference.pl"), args[0], args[1], args[2], path}, args[3:]...)
	cmd := exec.Command("perl", full...)
	var out bytes.Buffer
	cmd.Stdout = &out
	err := cmd.Run()
	rc := 0
	if ee, ok := err.(*exec.ExitError); ok {
		rc = ee.ExitCode()
	} else if err != nil {
		t.Fatal(err)
	}
	return out.String(), rc
}

func runOn(t *testing.T, e engine, input string, args []string) goldenRun {
	src := readInput(t, input)
	path := filepath.Join(t.TempDir(), "f")
	must(t, os.WriteFile(path, src, 0o644))
	out, rc := e(t, args, path)
	after, err := os.ReadFile(path)
	must(t, err)
	return goldenRun{Args: args, Exit: rc, Stdout: blob(out), File: fileDigest(string(after))}
}

func corpusInputs(t *testing.T) []string {
	ents, err := os.ReadDir(filepath.Join(corpusDir, "inputs"))
	must(t, err)
	var names []string
	for _, e := range ents {
		names = append(names, strings.TrimSuffix(e.Name(), ".gz"))
	}
	sort.Strings(names)
	if len(names) == 0 {
		t.Fatal("no corpus inputs")
	}
	return names
}

func readInput(t *testing.T, input string) []byte {
	p := filepath.Join(corpusDir, "inputs", input)
	b, err := os.ReadFile(p)
	if os.IsNotExist(err) {
		b, err = os.ReadFile(p + ".gz")
		if err == nil {
			b, err = gunzip(b)
		}
	}
	must(t, err)
	return b
}

func gunzip(b []byte) ([]byte, error) {
	r, err := gzip.NewReader(bytes.NewReader(b))
	if err != nil {
		return nil, err
	}
	return io.ReadAll(r)
}

func gzipBytes(b []byte) []byte {
	var out bytes.Buffer
	w, _ := gzip.NewWriterLevel(&out, gzip.BestCompression)
	w.Write(b)
	w.Close()
	return out.Bytes()
}

const goldenPath = corpusDir + "/golden.json.gz"

func loadGoldens(t *testing.T) map[string][]goldenRun {
	b, err := os.ReadFile(goldenPath)
	must(t, err)
	b, err = gunzip(b)
	must(t, err)
	var g map[string][]goldenRun
	must(t, json.Unmarshal(b, &g))
	return g
}

func checkAgainstGolden(t *testing.T, e engine) {
	n := 0
	goldens := loadGoldens(t)
	for _, input := range corpusInputs(t) {
		runs := goldens[input]
		if len(runs) != len(corpusArgs(input)) {
			t.Errorf("%s: %d golden runs, want %d (regenerate the goldens)", input, len(runs), len(corpusArgs(input)))
		}
		for _, want := range runs {
			got := runOn(t, e, input, want.Args)
			n++
			if got.Exit != want.Exit || got.Stdout != want.Stdout || got.File != want.File {
				t.Errorf("%s %q:\nexit %d, want %d\nstdout:\n%s\nwant:\n%s\nfile:\n%s\nwant:\n%s",
					input, want.Args, got.Exit, want.Exit, got.Stdout, want.Stdout, got.File, want.File)
			}
		}
	}
	t.Logf("%d calls compared", n)
}

func TestCorpus(t *testing.T) { checkAgainstGolden(t, goEngine) }

func TestCorpusPerl(t *testing.T) {
	if os.Getenv("DS_CORPUS_PERL") == "" {
		t.Skip("set DS_CORPUS_PERL=1 to check perl against the goldens")
	}
	checkAgainstGolden(t, perlEngine)
}

func TestCorpusGolden(t *testing.T) {
	if os.Getenv("DS_CORPUS_GOLDEN") == "" {
		t.Skip("set DS_CORPUS_GOLDEN=1 to rewrite the goldens with perl")
	}
	all := map[string][]goldenRun{}
	for _, input := range corpusInputs(t) {
		for _, args := range corpusArgs(input) {
			all[input] = append(all[input], runOn(t, perlEngine, input, args))
		}
	}
	b, err := json.MarshalIndent(all, "", " ")
	must(t, err)
	must(t, os.WriteFile(goldenPath, gzipBytes(append(b, '\n')), 0o644))
}

// Errors: a path that is not there, and a directory (it reads as empty, and
// cannot be written). The exit status is the errno, as perl's die gave.
func TestConflictErrors(t *testing.T) {
	tmp := t.TempDir()
	missing := filepath.Join(tmp, "missing")
	cases := []struct {
		args []string
		exit int
		out  string
	}{
		{[]string{"show", "rebase", "m", missing, "x"}, 2, ""},
		{[]string{"resolve", "rebase", "m", missing, "x", "all=local"}, 2, ""},
		{[]string{"show", "merge", "m", tmp, "x"}, 0, ""},
		{[]string{"resolve", "merge", "m", tmp, "x"}, 21, ""},
	}
	for _, c := range cases {
		var out, errb bytes.Buffer
		rc := runConflict(c.args, &out, &errb)
		if rc != c.exit || out.String() != c.out {
			t.Errorf("%q: exit %d stdout %q stderr %q", c.args, rc, out.String(), errb.String())
		}
		if _, err := exec.LookPath("perl"); err == nil {
			cmd := exec.Command("perl", append([]string{filepath.Join("testdata", "reference.pl")}, c.args...)...)
			var po, pe bytes.Buffer
			cmd.Stdout, cmd.Stderr = &po, &pe
			err := cmd.Run()
			prc := 0
			if ee, ok := err.(*exec.ExitError); ok {
				prc = ee.ExitCode()
			}
			if prc != rc || po.String() != out.String() || (c.exit != 0 && pe.String() != errb.String()) {
				t.Errorf("%q: perl exit %d stdout %q stderr %q; Go exit %d stdout %q stderr %q",
					c.args, prc, po.String(), pe.String(), rc, out.String(), errb.String())
			}
		}
	}
}

// --- Generating the inputs ---

type scenario struct{ name, base, up, loc string }

func lines(prefix string, n int, edit func(i int) string) string {
	var b strings.Builder
	for i := 1; i <= n; i++ {
		if s := edit(i); s != "" {
			b.WriteString(s)
			continue
		}
		fmt.Fprintf(&b, "%s line %d of the long entry\n", prefix, i)
	}
	return b.String()
}

func corpusScenarios() []scenario {
	crlf := func(s string) string { return strings.ReplaceAll(s, "\n", "\r\n") }
	pad := "* Heading\nsome context that stays\nmore context that stays\nand a third context line\n"
	tail := "closing context one\nclosing context two\nclosing context three\n"
	verseBase := pad + "#+begin_verse\nan old poem line\n#+end_verse\n\n" + tail
	// One hunk each: no line is unchanged on both sides, so nothing splits it.
	// 1500 lines a side is past the 2,000,000-cell LCS table, which falls back
	// to set membership; 1000 a side is the largest table perl filled.
	big := func(n, keep int, tag string) string {
		return lines("base", n, func(i int) string {
			if i%10 == keep {
				return ""
			}
			return fmt.Sprintf("%s edit of line %d\n", tag, i)
		})
	}
	return []scenario{
		{"both-edited.org", pad + "alpha line\n" + tail, pad + "alpha line remote\n" + tail, pad + "alpha line local\n" + tail},
		{"both-added.org", "* Log\nfirst entry\n", "* Log\nfirst entry\nremote entry\n", "* Log\nfirst entry\nlocal entry\n"},
		{"both-added-spaced.org", "* Log\n\nfirst entry\n", "* Log\n\nfirst entry\n\nremote entry\n", "* Log\n\nfirst entry\n\nlocal entry\n"},
		{"both-added-around.org", pad + "kept line in the middle\n" + tail, pad + "kept line in the middle\nremote line below\n" + tail, pad + "local line above\nkept line in the middle\nlocal line below\n" + tail},
		{"edited-and-added.org", pad + "entry one\nentry two\n" + tail, pad + "entry one\nentry two, edited remotely\n" + tail, pad + "entry one\nentry two\nentry three, added locally\n" + tail},
		{"added-and-edited.org", pad + "entry one\nentry two\n" + tail, pad + "entry one\nentry two\nentry three, added remotely\n" + tail, pad + "entry one\nentry two, edited locally\n" + tail},
		{"same-ignoring-ws.org", pad + "x y z line\n" + tail, pad + "x  y z line\n" + tail, pad + "x y\tz line \n" + tail},
		{"upstream-only-ws.org", pad + "the line here\n" + tail, pad + "the  line   here\n" + tail, pad + "the line here, changed locally\n" + tail},
		{"local-only-ws.org", pad + "the line here\n" + tail, pad + "the line here, changed remotely\n" + tail, pad + "the line\there \n" + tail},
		{"ws-only-sides.org", pad + "   \n\t\nvalue\n" + tail, pad + "\t\t\n\nvalue\n" + tail, pad + " \n  \t \nvalue\n" + tail},
		{"blank-lines-added.org", pad + "entry\n" + tail, pad + "entry\n\n\nremote entry\n" + tail, pad + "entry\n\nlocal entry\n\n" + tail},
		{"deleted-vs-edited.org", pad + "doomed line one\ndoomed line two\n" + tail, pad + tail, pad + "doomed line one, edited\ndoomed line two\n" + tail},
		{"verse-rewritten.org", verseBase, pad + "#+begin_verse\nan old poem line, revised\n#+end_verse\n\nremote prose\n" + tail,
			pad + "#+begin_verse\na new poem line\n#+end_verse\n\n#+begin_verse\nanother new poem\n#+end_verse\n\n" + tail},
		{"weak-markup.org", pad + "* A\n:PROPERTIES:\n:END:\n#+title: x\nbody\n" + tail,
			pad + "* A\n:PROPERTIES:\n:END:\n#+title: x\nbody, remote\n** -\n" + tail,
			pad + "* A\n:PROPERTIES:\n:END:\n#+TITLE:\n:LOGBOOK:\n#+BEGIN_VERSE\nab\n** -\n*** _\nbody, local\n" + tail},
		{"weak-run-with-strong.org", pad + "#+begin_quote\nshared strong sentence\n#+end_quote\n" + tail,
			pad + "#+begin_quote\nshared strong sentence\n#+end_quote\nremote addition\n" + tail,
			pad + "new local intro\n#+begin_quote\nshared strong sentence\n#+end_quote\n\nok\n" + tail},
		{"several-hunks.org", "top one\n" + pad + "mid one\n" + tail + "bottom one\n", "top one remote\n" + pad + "mid one remote\n" + tail + "bottom one remote\n",
			"top one local\n" + pad + "mid one local\n" + tail + "bottom one\nbottom two local\n"},
		{"no-trailing-newline.org", pad + "last line", pad + "last line, remote", pad + "last line, local"},
		{"crlf.org", crlf(pad + "entry\n" + tail), crlf(pad + "entry\nremote entry\n" + tail), crlf(pad + "entry, local\n\n" + tail)},
		{"crlf-mixed.org", pad + "entry\n" + tail, pad + "entry remote\r\n" + tail, pad + "entry local\n" + tail},
		{"unicode.org", pad + "Ünïcödé line ✓\n日本語の行\n" + tail, pad + "Ünïcödé line ✓ (fern)\n日本語の行\näb\n" + tail, pad + "Ünïcödé line ✓\n日本語の行です\n→ ok\n" + tail},
		{"nbsp.org", pad + "a b c line\n" + tail, pad + "a\u00a0b c line\n" + tail, pad + "a b\u00a0c line\n" + tail},
		{"latin1.org", pad + "caf\xe9 line\n" + tail, pad + "caf\xe9  line\xa0\n" + tail, pad + "caf\xe9 line \x85\n" + tail},
		{"marker-like-content.org", pad + "x line\n" + tail, pad + "x line up\n=======\nafter rule\n" + tail, pad + "x line loc\n||||||| not base\n" + tail},
		{"dup-lines.org", pad + "same\nsame\nsame\nunique base\n" + tail, pad + "same\nunique up\nsame\n" + tail, pad + "same\nsame\nunique loc\nsame\nsame\n" + tail},
		{"big-set-membership.txt", lines("base", 1500, func(int) string { return "" }), big(1500, 0, "up"), big(1500, 5, "loc")},
		{"big-dp.txt", lines("base", 1000, func(int) string { return "" }), big(1000, 0, "up"), big(1000, 5, "loc")},
	}
}

var corpusGitEnv []string

func cgit(t *testing.T, dir string, args ...string) string {
	t.Helper()
	cmd := exec.Command("git", append([]string{"-C", dir}, args...)...)
	cmd.Env = corpusGitEnv
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("git %v: %v\n%s", args, err, out)
	}
	return string(out)
}

// stage makes the scenarios conflict for real: two clones of a bare
// repository, the remote's edit pushed first, then this machine's pull.
func stage(t *testing.T, root, style string, rebase bool, sc []scenario) map[string]string {
	tmp := filepath.Join(root, fmt.Sprintf("%s-%v", style, rebase))
	must(t, os.MkdirAll(tmp, 0o755))
	write := func(dir string, pick func(scenario) string) {
		for _, s := range sc {
			must(t, os.WriteFile(filepath.Join(dir, s.name), []byte(pick(s)), 0o644))
		}
	}
	cgit(t, tmp, "init", "--quiet", "--bare", "origin.git")
	cgit(t, tmp, "clone", "--quiet", "origin.git", "a")
	a, b := filepath.Join(tmp, "a"), filepath.Join(tmp, "b")
	write(a, func(s scenario) string { return s.base })
	must(t, os.WriteFile(filepath.Join(a, "dirty.txt"), []byte("clean\n"), 0o644))
	cgit(t, a, "add", ".")
	cgit(t, a, "commit", "--quiet", "-m", "start")
	cgit(t, a, "push", "--quiet", "origin", "main")
	cgit(t, tmp, "clone", "--quiet", "origin.git", "b")
	write(b, func(s scenario) string { return s.up })
	cgit(t, b, "commit", "--quiet", "-am", "remote edit")
	cgit(t, b, "push", "--quiet")
	write(a, func(s scenario) string { return s.loc })
	cgit(t, a, "commit", "--quiet", "-am", "local edit")
	how := []string{"--rebase"}
	if !rebase {
		// daily-sync merges when tracked files are dirty.
		must(t, os.WriteFile(filepath.Join(a, "dirty.txt"), []byte("dirty\n"), 0o644))
		how = []string{"--no-rebase", "--no-edit"}
	}
	cmd := exec.Command("git", append([]string{"-C", a, "-c", "merge.conflictStyle=" + style, "pull"}, how...)...)
	cmd.Env = corpusGitEnv
	if out, err := cmd.CombinedOutput(); err == nil {
		t.Fatalf("%s: the pull did not conflict:\n%s", tmp, out)
	}
	got := map[string]string{}
	for _, s := range sc {
		b, err := os.ReadFile(filepath.Join(a, s.name))
		must(t, err)
		if bytes.Contains(b, []byte("\n<<<<<<<")) || bytes.HasPrefix(b, []byte("<<<<<<<")) {
			got[s.name] = string(b)
		} else {
			t.Logf("%s %s: no conflict markers", style, s.name)
		}
	}
	return got
}

// handmade are inputs git does not produce but the engine must survive.
var handmade = map[string]string{
	"hand-no-base.org":        "top\n<<<<<<< HEAD\nup line\n=======\nlocal line\n>>>>>>> abc (local)\nbottom\n",
	"hand-cr-markers.org":     "top\r\n<<<<<<<\r\nup\r\n|||||||\r\nbase\r\n=======\r\nloc\r\n>>>>>>>\r",
	"hand-unterminated.org":   "top\n<<<<<<< ours\nup one\n||||||| base\nb\n=======\nloc one\nloc two without newline",
	"hand-empty.org":          "",
	"hand-no-hunks.org":       "just text\n<<<<<<<< eight is not a marker\n<<<<<<<x neither\n=======\n",
	"hand-not-markers.org":    "a\n<<<<<<< opens\nup\n=======x\n======= labelled\n|||||||y\nloc?\n>>>>>>>z\n>>>>>>> closes\nb\n",
	"hand-repeat-markers.org": "<<<<<<< a\nu1\n||||||| b\nb1\n=======\nl1\n||||||| again\nb2\n=======\nl2\n>>>>>>> c\n",
	"hand-empty-sides.org":    "x\n<<<<<<<\n=======\n>>>>>>>\ny\n<<<<<<< a\n|||||||\n=======\n>>>>>>> b\n",
	"hand-based-empty.org":    "<<<<<<< a\nremote new\n||||||| base\n=======\nlocal new\n>>>>>>> b\n",
	"hand-cr-only.org":        "<<<<<<< a\rup\r=======\rloc\r>>>>>>> b\r",
	"hand-weak.org": "<<<<<<< a\n#+begin_verse\nshared poem line\n#+end_verse\n\n||||||| b\n#+BEGIN_VERSE\n#+end_verse\n=======\n" +
		"#+begin_verse\n\n#+end_verse\n#+begin_src python\n:x:\n*\n** \n**_\n* -\n#+key:\n#+key::\nab\n \t\v\f\n\n>>>>>>> c\n",
	"hand-spacing.org":     "<<<<<<< a\nremote entry\n\n||||||| b\n\n=======\n\nlocal entry\n>>>>>>> c\n<<<<<<< a\n  \nremote\n||||||| b\n=======\nlocal  \n>>>>>>> c\n",
	"hand-trailing-cr.org": "<<<<<<< a\nup\r\r\n=======\nloc\r\n>>>>>>> c\nend\r\r",
	// Local swapped two lines: which one an LCS keeps decides what "both" drops.
	"hand-swapped.org": "<<<<<<< a\nalpha line one\nbeta line two\ngamma, remote\n||||||| b\nalpha line one\nbeta line two\n=======\nbeta line two\nalpha line one\n>>>>>>> c\n" +
		"<<<<<<< a\nfirst kept\nsecond kept\n||||||| b\nsecond kept\nfirst kept\n=======\nsecond kept\nfirst kept\nlocal extra\n>>>>>>> c\n",
	// \v is whitespace to perl's \s.
	"hand-vtab.org": "<<<<<<< a\nx\vy line\n||||||| b\nx y line, base\n=======\nx y line\n>>>>>>> c\n" +
		"<<<<<<< a\nremote one\n||||||| b\nunique base one\nabc\v\nunique base two\n=======\nnew local one\nabc\v\nnew local two\n>>>>>>> c\n",
	// Each weak kind, matched alone between lines that differ: weak lines do not count as present.
	"hand-weak-isolated.org": "<<<<<<< a\nremote only\n||||||| b\n" +
		"base 1\nabc\nbase 2\nabcd\nbase 3\n#+title:\nbase 4\n#+TITLE\nbase 5\n:drawer:\nbase 6\n#+end_src\nbase 7\n**\nbase 8\n*** -\nbase 9\n#+begin_src x\nbase 10\n:a b:\nbase 11\n*  -\nbase 12\n" +
		"=======\n" +
		"local 1\nabc\nlocal 2\nabcd\nlocal 3\n#+title:\nlocal 4\n#+TITLE\nlocal 5\n:drawer:\nlocal 6\n#+end_src\nlocal 7\n**\nlocal 8\n*** -\nlocal 9\n#+begin_src x\nlocal 10\n:a b:\nlocal 11\n*  -\nlocal 12\n" +
		">>>>>>> c\n",
}

// handmadeBig is a hunk past the LCS table limit whose sides reorder base
// lines, so set membership and a real LCS would disagree.
func handmadeBig() string {
	var b strings.Builder
	side := func(tag string, keep func(i int) bool) {
		for i := 1; i <= 1500; i++ {
			j := i
			if i%50 == 10 {
				j = i + 10 // swap each pair (10, 20) of every 50
			} else if i%50 == 20 {
				j = i - 10
			}
			switch {
			case i%97 == 0:
				b.WriteString("\n")
			case keep(i):
				fmt.Fprintf(&b, "base line %d\n", j)
			default:
				fmt.Fprintf(&b, "%s line %d\n", tag, i)
			}
		}
	}
	b.WriteString("<<<<<<< a\n")
	side("up", func(i int) bool { return i%3 != 0 })
	b.WriteString("||||||| b\n")
	for i := 1; i <= 1500; i++ {
		fmt.Fprintf(&b, "base line %d\n", i)
	}
	b.WriteString("=======\n")
	side("loc", func(i int) bool { return i%4 != 1 })
	b.WriteString(">>>>>>> c\n")
	return b.String()
}

func TestCorpusInputs(t *testing.T) {
	if os.Getenv("DS_CORPUS_INPUTS") == "" {
		t.Skip("set DS_CORPUS_INPUTS=1 to rewrite the corpus inputs from staged repositories")
	}
	root := t.TempDir()
	gitconfig := filepath.Join(root, "gitconfig")
	must(t, os.WriteFile(gitconfig, []byte("[user]\n\tname = Tester\n\temail = tester@example.invalid\n[commit]\n\tgpgsign = false\n[init]\n\tdefaultBranch = main\n[core]\n\tautocrlf = false\n"), 0o600))
	corpusGitEnv = nil
	for _, kv := range os.Environ() {
		if !strings.HasPrefix(kv, "GIT_") {
			corpusGitEnv = append(corpusGitEnv, kv)
		}
	}
	corpusGitEnv = append(corpusGitEnv, "GIT_CONFIG_GLOBAL="+gitconfig, "GIT_CONFIG_NOSYSTEM=1",
		"GIT_AUTHOR_DATE=2026-01-01T00:00:00Z", "GIT_COMMITTER_DATE=2026-01-01T00:00:00Z")
	dir := filepath.Join(corpusDir, "inputs")
	must(t, os.RemoveAll(dir))
	must(t, os.MkdirAll(dir, 0o755))
	sc := corpusScenarios()
	for _, v := range []struct {
		style  string
		rebase bool
	}{{"zdiff3", true}, {"zdiff3", false}, {"diff3", true}, {"merge", true}} {
		order := "merge"
		if v.rebase {
			order = "rebase"
		}
		for name, s := range stage(t, root, v.style, v.rebase, sc) {
			if strings.HasPrefix(name, "big-") && !(v.style == "zdiff3" && v.rebase) {
				continue // big files once: they test the LCS, not the markers
			}
			p := filepath.Join(dir, fmt.Sprintf("git-%s-%s-%s", v.style, order, name))
			if len(s) > 20000 {
				must(t, os.WriteFile(p+".gz", gzipBytes([]byte(s)), 0o644))
				continue
			}
			must(t, os.WriteFile(p, []byte(s), 0o644))
		}
	}
	for name, s := range handmade {
		must(t, os.WriteFile(filepath.Join(dir, name), []byte(s), 0o644))
	}
	must(t, os.WriteFile(filepath.Join(dir, "hand-big-inversions.txt.gz"), gzipBytes([]byte(handmadeBig())), 0o644))
}
