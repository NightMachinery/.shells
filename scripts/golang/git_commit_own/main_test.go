package main

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// testEnv is the process environment without any GIT_* variable, with git
// configuration isolated from the machine's and a fixed identity.
func testEnv(t *testing.T, dir string) []string {
	t.Helper()
	var env []string
	for _, kv := range os.Environ() {
		if !strings.HasPrefix(kv, "GIT_") {
			env = append(env, kv)
		}
	}
	global := filepath.Join(dir, "gitconfig")
	if err := os.WriteFile(global, nil, 0o644); err != nil {
		t.Fatal(err)
	}
	return append(env,
		"GIT_CONFIG_GLOBAL="+global, "GIT_CONFIG_NOSYSTEM=1",
		"GIT_AUTHOR_NAME=Tester", "GIT_AUTHOR_EMAIL=tester@example.invalid",
		"GIT_COMMITTER_NAME=Tester", "GIT_COMMITTER_EMAIL=tester@example.invalid",
	)
}

type fixture struct {
	t    *testing.T
	root string   // scratch directory
	work string   // the work tree
	env  []string // environment for every git call
}

func (f *fixture) git(args ...string) string {
	f.t.Helper()
	return f.gitIn(f.work, args...)
}

func (f *fixture) gitIn(dir string, args ...string) string {
	f.t.Helper()
	cmd := exec.Command("git", args...)
	cmd.Dir, cmd.Env = dir, f.env
	out, err := cmd.CombinedOutput()
	if err != nil {
		f.t.Fatalf("git %s: %v\n%s", strings.Join(args, " "), err, out)
	}
	return strings.TrimSpace(string(out))
}

func (f *fixture) write(name, content string) {
	f.t.Helper()
	p := filepath.Join(f.work, name)
	if err := os.MkdirAll(filepath.Dir(p), 0o755); err != nil {
		f.t.Fatal(err)
	}
	if err := os.WriteFile(p, []byte(content), 0o644); err != nil {
		f.t.Fatal(err)
	}
}

func (f *fixture) read(name string) string {
	f.t.Helper()
	b, err := os.ReadFile(filepath.Join(f.work, name))
	if err != nil {
		f.t.Fatal(err)
	}
	return string(b)
}

type result struct {
	code           int
	stdout, stderr string
}

func (f *fixture) own(stdin string, args ...string) result {
	f.t.Helper()
	return f.ownIn(f.work, stdin, args...)
}

func (f *fixture) ownIn(dir, stdin string, args ...string) result {
	f.t.Helper()
	var out, errb bytes.Buffer
	a := &app{dir: dir, env: f.env, stdin: strings.NewReader(stdin), stdout: &out, stderr: &errb}
	code := a.run(args)
	a.cleanup()
	return result{code, out.String(), errb.String()}
}

func (f *fixture) mustOwn(args ...string) result {
	f.t.Helper()
	r := f.own("", args...)
	if r.code != 0 {
		f.t.Fatalf("git-commit-own %s: exit %d\n%s%s", strings.Join(args, " "), r.code, r.stdout, r.stderr)
	}
	return r
}

// newRepo makes a repository with a.txt and b.txt committed on master.
func newRepo(t *testing.T) *fixture {
	t.Helper()
	root := t.TempDir()
	f := &fixture{t: t, root: root, work: filepath.Join(root, "work"), env: testEnv(t, root)}
	f.gitIn(root, "init", "-q", "-b", "master", "work")
	f.write("a.txt", "a\n")
	f.write("b.txt", "b\n")
	f.git("add", "a.txt", "b.txt")
	f.git("commit", "-q", "-m", "base")
	return f
}

func (f *fixture) changedIn(rev string) string {
	f.t.Helper()
	return f.git("diff-tree", "--root", "--no-commit-id", "-r", "--name-only", rev)
}

func TestParseArgs(t *testing.T) {
	cases := []struct {
		args []string
		err  string
		msgs []string
		path []string
	}{
		{args: []string{"-m", "x", "a"}, msgs: []string{"x"}, path: []string{"a"}},
		{args: []string{"a", "-mx", "--message=y", "--", "-odd"}, msgs: []string{"x", "y"}, path: []string{"a", "-odd"}},
		{args: []string{"a"}, err: "message is required"},
		{args: []string{"-m", "x"}, err: "name the paths"},
		{args: []string{"-m", "x", "-F", "f", "a"}, err: "not both"},
		{args: []string{"-m", "x", "--bogus", "a"}, err: "unknown option"},
		{args: []string{"-m"}, err: "needs a value"},
		{args: []string{"-m", "x", "--dry-run=1", "a"}, err: "takes no value"},
		{args: []string{"-F", "-", "--patch", "-"}, err: "stdin"},
		{args: []string{"-m", "x", ""}, err: "empty path"},
		{args: []string{"--dry-run", "a"}, path: []string{"a"}},
	}
	for _, c := range cases {
		o, err := parseArgs(c.args)
		if c.err != "" {
			if err == nil || !strings.Contains(err.Error(), c.err) {
				t.Errorf("%q: want error %q, got %v", c.args, c.err, err)
			}
			continue
		}
		if err != nil {
			t.Errorf("%q: %v", c.args, err)
			continue
		}
		if strings.Join(o.messages, "|") != strings.Join(c.msgs, "|") || strings.Join(o.paths, "|") != strings.Join(c.path, "|") {
			t.Errorf("%q: messages %q paths %q", c.args, o.messages, o.paths)
		}
	}
}

// The core promise: only the named path is committed, another session's
// staged work stays staged and out of the commit, and the committed path
// shows clean afterwards.
func TestCommitsOnlyNamedPaths(t *testing.T) {
	f := newRepo(t)
	f.write("a.txt", "a2\n")
	f.write("b.txt", "b2\n")
	f.git("add", "b.txt") // someone else's staged work

	f.mustOwn("-m", "change a", "a.txt")

	if got := f.changedIn("HEAD"); got != "a.txt" {
		t.Fatalf("commit changed %q, want a.txt", got)
	}
	if got := f.git("show", "HEAD:a.txt"); got != "a2" {
		t.Fatalf("committed a.txt = %q", got)
	}
	if got := f.git("diff", "--cached", "--name-only"); got != "b.txt" {
		t.Fatalf("shared index staged %q, want b.txt still staged", got)
	}
	if got := f.git("status", "--porcelain"); got != "M  b.txt" {
		t.Fatalf("status = %q", got)
	}
	if got := f.git("log", "-1", "--format=%s"); got != "change a" {
		t.Fatalf("subject %q", got)
	}
	if got := f.git("reflog", "-1", "--format=%gs", "master"); got != "commit: change a" {
		t.Fatalf("reflog %q", got)
	}
}

// A path the caller had already staged in the shared index (an old habit)
// is committed from disk and ends up clean.
func TestPreStagedPathEndsClean(t *testing.T) {
	f := newRepo(t)
	f.write("a.txt", "staged\n")
	f.git("add", "a.txt")
	f.write("a.txt", "on disk\n")

	f.mustOwn("-m", "a", "a.txt")

	if got := f.git("show", "HEAD:a.txt"); got != "on disk" {
		t.Fatalf("committed %q, want the disk contents", got)
	}
	if got := f.git("status", "--porcelain"); got != "" {
		t.Fatalf("status = %q, want clean", got)
	}
}

func TestNewAndDeletedFiles(t *testing.T) {
	f := newRepo(t)
	f.write("dir/new.txt", "new\n")
	if err := os.Remove(filepath.Join(f.work, "b.txt")); err != nil {
		t.Fatal(err)
	}

	f.mustOwn("-m", "add and delete", "dir", "b.txt")

	if got := f.changedIn("HEAD"); got != "b.txt\ndir/new.txt" {
		t.Fatalf("changed %q", got)
	}
	if got := f.git("status", "--porcelain"); got != "" {
		t.Fatalf("status = %q", got)
	}
}

func TestUntrackKeepsFile(t *testing.T) {
	f := newRepo(t)
	f.write("big.bin", "big\n")
	f.git("add", "big.bin")
	f.git("commit", "-q", "-m", "oops")
	f.write(".gitignore", "*.bin\n")

	f.mustOwn("-m", "stop tracking big.bin", ".gitignore", "--untrack", "big.bin")

	if got := f.changedIn("HEAD"); got != ".gitignore\nbig.bin" {
		t.Fatalf("changed %q", got)
	}
	if _, err := os.Stat(filepath.Join(f.work, "big.bin")); err != nil {
		t.Fatalf("big.bin gone from disk: %v", err)
	}
	if got := f.git("ls-files", "big.bin"); got != "" {
		t.Fatalf("big.bin still in the shared index")
	}
	if got := f.git("status", "--porcelain"); got != "" {
		t.Fatalf("status = %q", got)
	}
}

func TestPatchCommitsPartOfAFile(t *testing.T) {
	f := newRepo(t)
	lines := "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n"
	f.write("n.txt", lines)
	f.git("add", "n.txt")
	f.git("commit", "-q", "-m", "numbers")
	f.write("n.txt", strings.Replace(strings.Replace(lines, "1\n", "one\n", 1), "10\n", "ten\n", 1))

	diff := f.git("diff", "-U1", "n.txt") + "\n"
	hunks := strings.Split(diff, "\n@@")
	if len(hunks) != 3 {
		t.Fatalf("want two hunks:\n%s", diff)
	}
	first := hunks[0] + "\n@@" + hunks[1] + "\n"
	if err := os.WriteFile(filepath.Join(f.root, "first.patch"), []byte(first), 0o644); err != nil {
		t.Fatal(err)
	}

	f.mustOwn("-m", "first hunk", "--patch", filepath.Join(f.root, "first.patch"))

	committed := f.git("show", "HEAD:n.txt")
	if !strings.HasPrefix(committed, "one\n") || !strings.HasSuffix(committed, "\n10") {
		t.Fatalf("committed:\n%s", committed)
	}
	if !strings.HasSuffix(f.read("n.txt"), "ten\n") {
		t.Fatalf("the second change left the disk")
	}
	if got := f.git("status", "--porcelain"); got != "M n.txt" {
		t.Fatalf("status = %q, want only an unstaged change", got)
	}
}

func TestTypoCommitsNothing(t *testing.T) {
	f := newRepo(t)
	f.write("a.txt", "a2\n")
	before := f.git("rev-parse", "HEAD")

	if r := f.own("", "-m", "x", "a.txt", "no-such-file"); r.code == 0 {
		t.Fatal("a path that matches nothing was accepted")
	}
	if r := f.own("", "-m", "x", "--untrack", "no-such-file"); r.code == 0 {
		t.Fatal("--untrack of an untracked path was accepted")
	}
	if got := f.git("rev-parse", "HEAD"); got != before {
		t.Fatal("HEAD moved")
	}
}

func TestWholeTreeNeedsFlag(t *testing.T) {
	f := newRepo(t)
	f.write("a.txt", "a2\n")
	if err := os.MkdirAll(filepath.Join(f.work, "sub"), 0o755); err != nil {
		t.Fatal(err)
	}
	for _, p := range []string{".", "./", f.work} {
		if r := f.own("", "-m", "x", p); r.code != 2 {
			t.Fatalf("%q: exit %d, want 2\n%s", p, r.code, r.stderr)
		}
	}
	if r := f.ownIn(filepath.Join(f.work, "sub"), "", "-m", "x", ".."); r.code != 2 {
		t.Fatalf("..: exit %d, want 2", r.code)
	}
	f.mustOwn("-m", "everything", "--whole-tree", ".")
}

func TestNothingToCommit(t *testing.T) {
	f := newRepo(t)
	r := f.own("", "-m", "x", "a.txt")
	if r.code != 1 || !strings.Contains(r.stderr, "nothing to commit") || !strings.Contains(r.stderr, "no changes") {
		t.Fatalf("exit %d\n%s", r.code, r.stderr)
	}
	f.mustOwn("-m", "empty", "--allow-empty", "a.txt")
	if got := f.git("rev-list", "--count", "HEAD"); got != "2" {
		t.Fatalf("%s commits", got)
	}
}

func TestUnbornBranch(t *testing.T) {
	root := t.TempDir()
	f := &fixture{t: t, root: root, work: filepath.Join(root, "work"), env: testEnv(t, root)}
	f.gitIn(root, "init", "-q", "-b", "main", "work")
	f.write("a.txt", "a\n")
	f.write("other.txt", "not mine\n")

	r := f.mustOwn("-m", "first", "a.txt")

	if !strings.Contains(r.stdout, "(root-commit)") {
		t.Fatalf("stdout:\n%s", r.stdout)
	}
	if got := f.changedIn("main"); got != "a.txt" {
		t.Fatalf("changed %q", got)
	}
	if got := f.git("reflog", "-1", "--format=%gs", "main"); got != "commit (initial): first" {
		t.Fatalf("reflog %q", got)
	}
	if got := f.git("status", "--porcelain"); got != "?? other.txt" {
		t.Fatalf("status = %q", got)
	}
}

// When another session commits while ours is being built, the branch tip
// moves under us; the commit is rebuilt on the new tip, so their commit
// survives and ours does not revert it.
func TestRaceRebuildsOnNewTip(t *testing.T) {
	f := newRepo(t)
	marker := filepath.Join(f.root, "raced")
	hook := `#!/bin/sh
[ -e "` + marker + `" ] && exit 0
touch "` + marker + `"
export GIT_INDEX_FILE="$(git rev-parse --git-dir)/other.index"
git read-tree HEAD
echo theirs > b.txt
git add b.txt
git commit -q -m theirs
`
	f.write(".git/hooks/pre-commit", hook)
	if err := os.Chmod(filepath.Join(f.work, ".git/hooks/pre-commit"), 0o755); err != nil {
		t.Fatal(err)
	}
	f.write("a.txt", "ours\n")

	r := f.mustOwn("-m", "ours", "a.txt")

	if !strings.Contains(r.stderr, "rebuilding") {
		t.Fatalf("no race reported:\n%s", r.stderr)
	}
	if got := f.git("log", "--format=%s"); got != "ours\ntheirs\nbase" {
		t.Fatalf("log:\n%s", got)
	}
	if got := f.git("show", "HEAD:b.txt"); got != "theirs" {
		t.Fatalf("our commit reverted theirs: b.txt = %q", got)
	}
	if got := f.changedIn("HEAD"); got != "a.txt" {
		t.Fatalf("changed %q", got)
	}
}

// vcsh runs git with GIT_DIR and GIT_WORK_TREE set, the work tree being the
// home directory; paths are relative to wherever the command runs.
func TestSeparateGitDir(t *testing.T) {
	root := t.TempDir()
	home := filepath.Join(root, "home")
	gitDir := filepath.Join(root, "repo.git")
	f := &fixture{t: t, root: root, work: home, env: testEnv(t, root)}
	if err := os.MkdirAll(home, 0o755); err != nil {
		t.Fatal(err)
	}
	f.env = append(f.env, "GIT_DIR="+gitDir, "GIT_WORK_TREE="+home)
	f.gitIn(root, "init", "-q", "-b", "master")
	f.write("scripts/x.zsh", "x\n")
	f.write("scripts/y.zsh", "y\n")
	f.write(".profile", "untracked dotfile\n")
	f.git("add", "scripts")
	f.git("commit", "-q", "-m", "base")
	f.write("scripts/x.zsh", "x2\n")
	f.write("scripts/y.zsh", "y2\n")

	if r := f.ownIn(filepath.Join(home, "scripts"), "", "-m", "x", "x.zsh"); r.code != 0 {
		t.Fatalf("exit %d\n%s", r.code, r.stderr)
	}

	if got := f.changedIn("HEAD"); got != "scripts/x.zsh" {
		t.Fatalf("changed %q", got)
	}
	if got := f.git("status", "--porcelain", "-uno"); got != "M scripts/y.zsh" {
		t.Fatalf("status = %q", got)
	}
	if r := f.ownIn(home, "", "-m", "x", "."); r.code != 2 {
		t.Fatalf("the home directory as a path: exit %d", r.code)
	}
}

func TestSigning(t *testing.T) {
	f := newRepo(t)
	gpg := filepath.Join(f.root, "fake-gpg")
	script := "#!/bin/sh\nprintf '\\n[GNUPG:] SIG_CREATED D 1 8 00 0 X\\n' >&2\ncat >/dev/null\nprintf -- '-----BEGIN PGP SIGNATURE-----\\nfake\\n-----END PGP SIGNATURE-----\\n'\n"
	if err := os.WriteFile(gpg, []byte(script), 0o755); err != nil {
		t.Fatal(err)
	}
	f.git("config", "gpg.program", gpg)
	f.git("config", "commit.gpgSign", "true")

	f.write("a.txt", "signed\n")
	f.mustOwn("-m", "signed", "a.txt")
	if !strings.Contains(f.git("cat-file", "commit", "HEAD"), "gpgsig") {
		t.Fatal("commit.gpgSign set, but the commit is not signed")
	}

	f.write("a.txt", "unsigned\n")
	f.mustOwn("-m", "unsigned", "--no-gpg-sign", "a.txt")
	if strings.Contains(f.git("cat-file", "commit", "HEAD"), "gpgsig") {
		t.Fatal("--no-gpg-sign commit is signed")
	}

	if err := os.WriteFile(gpg, []byte("#!/bin/sh\necho 'no secret key' >&2\nexit 2\n"), 0o755); err != nil {
		t.Fatal(err)
	}
	before := f.git("rev-parse", "HEAD")
	f.write("a.txt", "fails\n")
	r := f.own("", "-m", "fails", "a.txt")
	if r.code != 1 || !strings.Contains(r.stderr, "--no-gpg-sign") {
		t.Fatalf("exit %d\n%s", r.code, r.stderr)
	}
	if got := f.git("rev-parse", "HEAD"); got != before {
		t.Fatal("HEAD moved after a failed signature")
	}
}

func TestHooks(t *testing.T) {
	f := newRepo(t)
	hooks := filepath.Join(f.work, ".git", "hooks")
	if err := os.MkdirAll(hooks, 0o755); err != nil {
		t.Fatal(err)
	}
	write := func(name, body string) {
		if err := os.WriteFile(filepath.Join(hooks, name), []byte("#!/bin/sh\n"+body), 0o755); err != nil {
			t.Fatal(err)
		}
	}
	write("commit-msg", "printf '\\nTrailer: yes\\n' >> \"$1\"\n")
	// The pre-commit hook sees the private index: only a.txt is staged there.
	write("pre-commit", "test \"$(git diff --cached --name-only)\" = a.txt || { echo \"saw $(git diff --cached --name-only)\" >&2; exit 1; }\n")
	f.write("a.txt", "a2\n")
	f.write("b.txt", "b2\n")
	f.git("add", "b.txt")

	f.mustOwn("-m", "with hooks", "a.txt")
	if got := f.git("log", "-1", "--format=%B"); got != "with hooks\n\nTrailer: yes" {
		t.Fatalf("message:\n%s", got)
	}

	write("pre-commit", "exit 1\n")
	f.write("a.txt", "a3\n")
	before := f.git("rev-parse", "HEAD")
	if r := f.own("", "-m", "blocked", "a.txt"); r.code != 1 || !strings.Contains(r.stderr, "pre-commit") {
		t.Fatalf("exit %d\n%s", r.code, r.stderr)
	}
	if got := f.git("rev-parse", "HEAD"); got != before {
		t.Fatal("HEAD moved past a failing pre-commit hook")
	}
	f.mustOwn("-m", "skipped", "--no-verify", "a.txt")
	if got := f.git("log", "-1", "--format=%B"); got != "skipped" {
		t.Fatalf("--no-verify ran commit-msg: %q", got)
	}
}

func TestMessageSources(t *testing.T) {
	f := newRepo(t)
	f.write("a.txt", "1\n")
	f.mustOwn("-m", "subject", "-m", "body  ", "a.txt")
	if got := f.git("log", "-1", "--format=%B"); got != "subject\n\nbody" {
		t.Fatalf("message %q", got)
	}
	f.write("a.txt", "2\n")
	if r := f.own("from stdin\n\n\n", "-F", "-", "a.txt"); r.code != 0 {
		t.Fatalf("exit %d\n%s", r.code, r.stderr)
	}
	if got := f.git("log", "-1", "--format=%B"); got != "from stdin" {
		t.Fatalf("message %q", got)
	}
	f.write("a.txt", "3\n")
	if r := f.own("", "-m", "  ", "a.txt"); r.code != 1 || !strings.Contains(r.stderr, "empty") {
		t.Fatalf("blank message: exit %d\n%s", r.code, r.stderr)
	}
}

func TestDryRun(t *testing.T) {
	f := newRepo(t)
	f.write("a.txt", "a2\n")
	before := f.git("rev-parse", "HEAD")
	r := f.mustOwn("--dry-run", "a.txt")
	if !strings.Contains(r.stdout, "Would commit on master") || !strings.Contains(r.stdout, "a.txt") {
		t.Fatalf("stdout:\n%s", r.stdout)
	}
	if got := f.git("rev-parse", "HEAD"); got != before {
		t.Fatal("dry run moved HEAD")
	}
	if got := f.git("status", "--porcelain"); got != "M a.txt" {
		t.Fatalf("status = %q", got)
	}
}

func TestRefusesDuringMerge(t *testing.T) {
	f := newRepo(t)
	f.git("update-ref", "MERGE_HEAD", "HEAD")
	f.write("a.txt", "a2\n")
	if r := f.own("", "-m", "x", "a.txt"); r.code != 1 || !strings.Contains(r.stderr, "MERGE_HEAD") {
		t.Fatalf("exit %d\n%s", r.code, r.stderr)
	}
}

func TestNoTempFilesLeft(t *testing.T) {
	f := newRepo(t)
	f.write("a.txt", "a2\n")
	f.mustOwn("-m", "x", "a.txt")
	matches, err := filepath.Glob(filepath.Join(f.work, ".git", "commit-own-*"))
	if err != nil {
		t.Fatal(err)
	}
	if len(matches) != 0 {
		t.Fatalf("left behind: %q", matches)
	}
}
