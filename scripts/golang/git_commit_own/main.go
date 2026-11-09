// git-commit-own: commit exactly the named paths, as they are on disk, through
// a private index. Whatever else sits in the shared index (another session's
// staged work) neither lands in the commit nor is disturbed by it, and the
// commit cannot be swept into anyone else's, because nothing is built in the
// shared index at all.
//
// Plain `git commit -- <paths>` already ignores what others staged, but it
// takes the named paths from disk: it re-adds a file you `git rm --cached`,
// and it commits the whole file after a `git add -p`. Doing either in the
// shared index instead leaves the change exposed to the next `git commit`
// anyone runs. Here the private index carries untracking (--untrack) and part
// of a file (--patch) safely.
//
// Invoked as `git commit-own` through the shim in scripts/bin. See
// scripts/docs/git-commit-own.md. Stdlib only.
package main

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"regexp"
	"strings"
	"sync"
	"syscall"
	"time"
)

const usageText = `usage: git commit-own [options] [--] <path>...
       git commit-own --amend [options] [--] [<path>...]

Commit exactly the named paths, as they are on disk, through a private index.
Whatever else is staged in the shared index stays out of the commit, and is
left as it was.

  -m, --message MSG    commit message; repeat it for more paragraphs
  -F, --file FILE      read the message from FILE ("-": stdin)
      --untrack PATH   stop tracking PATH; the file stays on disk (repeatable)
      --patch FILE     apply FILE, a patch against HEAD, as part of the commit,
                       to commit only part of a file (repeatable; "-": stdin)
  -f, --force          allow adding ignored files
      --amend          replace the last commit with one that also holds the
                       named paths; its message and author are kept unless -m,
                       -F or --author is given; refused once it is pushed
      --author A       the author: "Name <email>", or a pattern matching the
                       author of an earlier commit, as for git commit
      --allow-empty    commit even when nothing changed
  -n, --no-verify      skip the pre-commit and commit-msg hooks
      --no-gpg-sign    do not sign, even when commit.gpgSign is set
      --whole-tree     allow a path that is the whole work tree, such as "."
      --dry-run        show what would be committed, and stop
  -h, --help           show this help

Paths are pathspecs relative to the current directory, as for git add, and
each is staged with "git add -A": new files are added, deleted files removed.
A path that matches nothing is an error, so a typo commits nothing.

The paths are staged first, then --untrack is applied, then each --patch.
After the commit, the shared index entries of the committed paths are reset
to the new commit, so git status shows them clean. If the branch moves while
the commit is being made, it is rebuilt on the new tip.
`

const maxAttempts = 5

var errHelp = errors.New("help")

type options struct {
	messages   []string
	msgFile    string
	paths      []string
	untrack    []string
	patchFiles []string
	force      bool
	allowEmpty bool
	noVerify   bool
	noSign     bool
	wholeTree  bool
	dryRun     bool
	amend      bool
	author     string
}

func parseArgs(args []string) (*options, error) {
	o := &options{}
	for i := 0; i < len(args); i++ {
		arg := args[i]
		if arg == "--" {
			o.paths = append(o.paths, args[i+1:]...)
			break
		}
		if !strings.HasPrefix(arg, "-") || arg == "-" {
			o.paths = append(o.paths, arg)
			continue
		}

		name, val, inline := arg, "", false
		if strings.HasPrefix(arg, "--") {
			if k := strings.IndexByte(arg, '='); k >= 0 {
				name, val, inline = arg[:k], arg[k+1:], true
			}
		} else if len(arg) > 2 {
			name, val, inline = arg[:2], arg[2:], true
		}
		value := func() (string, error) {
			if inline {
				return val, nil
			}
			if i+1 >= len(args) {
				return "", fmt.Errorf("%s needs a value", name)
			}
			i++
			return args[i], nil
		}
		flag := func(p *bool) error {
			if inline {
				return fmt.Errorf("%s takes no value", name)
			}
			*p = true
			return nil
		}

		var err error
		var v string
		switch name {
		case "-m", "--message":
			if v, err = value(); err == nil {
				o.messages = append(o.messages, v)
			}
		case "-F", "--file":
			if v, err = value(); err == nil {
				if o.msgFile != "" {
					err = fmt.Errorf("-F given twice")
				}
				o.msgFile = v
			}
		case "--untrack":
			if v, err = value(); err == nil {
				o.untrack = append(o.untrack, v)
			}
		case "--patch":
			if v, err = value(); err == nil {
				o.patchFiles = append(o.patchFiles, v)
			}
		case "-f", "--force":
			err = flag(&o.force)
		case "--allow-empty":
			err = flag(&o.allowEmpty)
		case "-n", "--no-verify":
			err = flag(&o.noVerify)
		case "--no-gpg-sign":
			err = flag(&o.noSign)
		case "--whole-tree":
			err = flag(&o.wholeTree)
		case "--dry-run":
			err = flag(&o.dryRun)
		case "--amend":
			err = flag(&o.amend)
		case "--author":
			v, err = value()
			o.author = v
		case "-h", "--help":
			return nil, errHelp
		default:
			err = fmt.Errorf("unknown option %s", arg)
		}
		if err != nil {
			return nil, err
		}
	}

	switch {
	case len(o.messages) > 0 && o.msgFile != "":
		return nil, fmt.Errorf("give the message with -m or with -F, not both")
	case len(o.messages) == 0 && o.msgFile == "" && !o.dryRun && !o.amend:
		return nil, fmt.Errorf("a commit message is required (-m or -F)")
	case len(o.paths)+len(o.untrack)+len(o.patchFiles) == 0 && !o.allowEmpty && !o.amend:
		return nil, fmt.Errorf("name the paths to commit")
	}
	stdinUsers := 0
	if o.msgFile == "-" {
		stdinUsers++
	}
	for _, p := range o.patchFiles {
		if p == "-" {
			stdinUsers++
		}
	}
	if stdinUsers > 1 {
		return nil, fmt.Errorf("only one of -F and --patch can read stdin")
	}
	for _, p := range append(append([]string{}, o.paths...), o.untrack...) {
		if p == "" {
			return nil, fmt.Errorf("empty path")
		}
	}
	return o, nil
}

// app holds what one run needs; tests build their own.
type app struct {
	dir    string   // where pathspecs are resolved; "" is the process's directory
	env    []string // the caller's environment, including any GIT_DIR or GIT_INDEX_FILE
	stdin  io.Reader
	stdout io.Writer
	stderr io.Writer

	mu    sync.Mutex
	temps []string
}

func main() {
	a := &app{env: os.Environ(), stdin: os.Stdin, stdout: os.Stdout, stderr: os.Stderr}
	sigs := make(chan os.Signal, 1)
	signal.Notify(sigs, os.Interrupt, syscall.SIGTERM, syscall.SIGHUP)
	go func() {
		s := <-sigs
		a.cleanup()
		code := 1
		if n, ok := s.(syscall.Signal); ok {
			code = 128 + int(n)
		}
		os.Exit(code)
	}()
	code := a.run(os.Args[1:])
	a.cleanup()
	os.Exit(code)
}

func (a *app) addTemp(paths ...string) {
	a.mu.Lock()
	defer a.mu.Unlock()
	a.temps = append(a.temps, paths...)
}

func (a *app) cleanup() {
	a.mu.Lock()
	defer a.mu.Unlock()
	for _, p := range a.temps {
		os.Remove(p)
	}
	a.temps = nil
}

func (a *app) warnf(format string, args ...any) {
	fmt.Fprintf(a.stderr, "git-commit-own: "+format+"\n", args...)
}

// gitErr is a failed git command. stderr is empty when the command was
// allowed to print it directly.
type gitErr struct {
	args   []string
	code   int
	stderr string
}

func (e *gitErr) Error() string {
	if msg := strings.TrimSpace(e.stderr); msg != "" {
		return fmt.Sprintf("git %s: %s", e.args[0], msg)
	}
	return fmt.Sprintf("git %s failed (exit %d)", e.args[0], e.code)
}

func exitCode(err error) int {
	var ge *gitErr
	if errors.As(err, &ge) {
		return ge.code
	}
	return -1
}

type call struct {
	dir   string   // "" is app.dir
	env   []string // added to the caller's environment, overriding it
	stdin string
	show  bool // let stdout and stderr through to the user
	errs  bool // let stderr through; stdout is still captured
}

func (a *app) git(c call, args ...string) (string, error) {
	cmd := exec.Command("git", args...)
	cmd.Dir = c.dir
	if cmd.Dir == "" {
		cmd.Dir = a.dir
	}
	cmd.Env = append(append([]string{}, a.env...), c.env...)
	if c.stdin != "" {
		cmd.Stdin = strings.NewReader(c.stdin)
	}
	var out, errb bytes.Buffer
	cmd.Stdout, cmd.Stderr = &out, &errb
	if c.show {
		cmd.Stdout, cmd.Stderr = a.stdout, a.stderr
	} else if c.errs {
		cmd.Stderr = a.stderr
	}
	if err := cmd.Run(); err != nil {
		var ee *exec.ExitError
		if !errors.As(err, &ee) {
			return "", err
		}
		return out.String(), &gitErr{args: args, code: ee.ExitCode(), stderr: errb.String()}
	}
	return out.String(), nil
}

func (a *app) line(c call, args ...string) (string, error) {
	out, err := a.git(c, args...)
	return strings.TrimSpace(out), err
}

type repo struct {
	gitDir string // absolute
	top    string // absolute work tree root
	prefix string // the current directory relative to top: "" or "sub/dir/"
	empty  string // the empty tree's id
	sign   bool
}

func (a *app) discover(o *options) (*repo, error) {
	out, err := a.git(call{}, "rev-parse", "--absolute-git-dir", "--show-toplevel", "--show-prefix")
	if err != nil {
		return nil, err
	}
	lines := strings.Split(strings.TrimSuffix(out, "\n"), "\n")
	for len(lines) < 3 {
		lines = append(lines, "")
	}
	r := &repo{gitDir: lines[0], top: lines[1], prefix: lines[2]}
	if r.empty, err = a.line(call{}, "hash-object", "-t", "tree", "--stdin"); err != nil {
		return nil, err
	}
	if !o.noSign {
		v, err := a.line(call{}, "config", "--type=bool", "--get", "commit.gpgSign")
		if err != nil && exitCode(err) != 1 {
			return nil, err
		}
		r.sign = v == "true"
	}
	for _, ref := range []string{"MERGE_HEAD", "CHERRY_PICK_HEAD", "REVERT_HEAD"} {
		if _, err := a.git(call{}, "rev-parse", "-q", "--verify", ref); err == nil {
			return nil, fmt.Errorf("%s exists: finish that with git commit first", ref)
		}
	}
	return r, nil
}

// isWholeTree reports whether a plain path argument names the work tree root.
func (r *repo) isWholeTree(p string) bool {
	if strings.HasPrefix(p, ":") {
		return false // pathspec magic: the caller meant it
	}
	var rel string
	if filepath.IsAbs(p) {
		if real, err := filepath.EvalSymlinks(p); err == nil {
			p = real // git reports the top resolved, as on macOS's /var -> /private/var
		}
		var err error
		if rel, err = filepath.Rel(r.top, p); err != nil {
			return false
		}
	} else {
		rel = filepath.Join(r.prefix, p)
	}
	rel = filepath.Clean(rel)
	return rel == "." || rel == "*"
}

type patch struct {
	name string
	data string
}

func (a *app) readInputs(o *options) (string, []patch, error) {
	read := func(name string) (string, error) {
		var b []byte
		var err error
		if name == "-" {
			b, err = io.ReadAll(a.stdin)
		} else {
			if !filepath.IsAbs(name) && a.dir != "" {
				name = filepath.Join(a.dir, name)
			}
			b, err = os.ReadFile(name)
		}
		return string(b), err
	}

	var msg string
	switch {
	case len(o.messages) > 0:
		msg = strings.Join(o.messages, "\n\n")
	case o.msgFile != "":
		var err error
		if msg, err = read(o.msgFile); err != nil {
			return "", nil, err
		}
	}

	var patches []patch
	for _, name := range o.patchFiles {
		data, err := read(name)
		if err != nil {
			return "", nil, err
		}
		if strings.TrimSpace(data) == "" {
			return "", nil, fmt.Errorf("patch %s is empty", name)
		}
		patches = append(patches, patch{name, data})
	}
	return msg, patches, nil
}

func (a *app) cleanMessage(raw string) (string, error) {
	msg, err := a.git(call{stdin: raw}, "stripspace")
	if err != nil {
		return "", err
	}
	if strings.TrimSpace(msg) == "" {
		return "", fmt.Errorf("the commit message is empty")
	}
	return msg, nil
}

func (a *app) run(args []string) int {
	o, err := parseArgs(args)
	if err == errHelp {
		fmt.Fprint(a.stdout, usageText)
		return 0
	}
	if err != nil {
		a.warnf("%v (see --help)", err)
		return 2
	}
	r, err := a.discover(o)
	if err != nil {
		a.warnf("%v", err)
		return 1
	}
	if !o.wholeTree {
		for _, p := range append(append([]string{}, o.paths...), o.untrack...) {
			if r.isWholeTree(p) {
				a.warnf("%q is the whole work tree, which would take in every file anyone changed; name your own paths, or pass --whole-tree", p)
				return 2
			}
		}
	}
	raw, patches, err := a.readInputs(o)
	if err != nil {
		a.warnf("%v", err)
		return 1
	}
	msg := "" // when amending, empty means "keep the old message"
	if !o.dryRun && (raw != "" || !o.amend) {
		if msg, err = a.cleanMessage(raw); err != nil {
			a.warnf("%v", err)
			return 1
		}
	}
	author, err := a.resolveAuthor(o.author)
	if err != nil {
		a.warnf("%v", err)
		return 1
	}

	for n := 1; n <= maxAttempts; n++ {
		err = a.attempt(o, r, msg, author, patches, n)
		if !errors.Is(err, errRaced) {
			break
		}
		if o.amend {
			// The new tip is someone else's commit; folding into it would mix
			// their work with ours.
			a.warnf("the branch moved while amending, so its tip is no longer the commit to amend; nothing was changed")
			return 1
		}
		if n < maxAttempts {
			a.warnf("the branch moved while committing; rebuilding on its new tip")
		}
	}
	if err != nil {
		if errors.Is(err, errRaced) {
			a.warnf("the branch kept moving; gave up after %d attempts, nothing was committed", maxAttempts)
		} else {
			a.warnf("%v", err)
		}
		return 1
	}
	return 0
}

var (
	errRaced   = errors.New("the branch moved")
	errNothing = errors.New("nothing to commit: the named paths already match HEAD (pass --allow-empty to commit anyway)")
)

type head struct {
	ref    string // what HEAD points at, such as refs/heads/master; "" when detached
	commit string // "" on an unborn branch
}

func (h head) where() string {
	switch {
	case h.ref == "":
		return "detached HEAD"
	case strings.HasPrefix(h.ref, "refs/heads/"):
		return strings.TrimPrefix(h.ref, "refs/heads/")
	}
	return h.ref
}

func (a *app) readHead() (head, error) {
	var h head
	var err error
	if h.ref, err = a.line(call{}, "symbolic-ref", "-q", "HEAD"); err != nil && exitCode(err) != 1 {
		return h, err
	}
	if h.commit, err = a.line(call{}, "rev-parse", "-q", "--verify", "HEAD^{commit}"); err != nil && exitCode(err) != 1 {
		return h, err
	}
	if h.ref == "" && h.commit == "" {
		return h, fmt.Errorf("HEAD is neither a branch nor a commit")
	}
	return h, nil
}

// attempt builds the commit from HEAD as it is now, and moves the branch only
// if HEAD is still there; otherwise it returns errRaced for a rebuild.
func (a *app) attempt(o *options, r *repo, msg string, author []string, patches []patch, n int) error {
	h, err := a.readHead()
	if err != nil {
		return err
	}
	base := h.commit
	if base == "" {
		if o.amend {
			return fmt.Errorf("nothing to amend: %s has no commits yet", h.where())
		}
		base = r.empty
	}
	baseTree, err := a.line(call{}, "rev-parse", base+"^{tree}")
	if err != nil {
		return err
	}

	// The new commit's parents, and the tree it must differ from: HEAD's for
	// a new commit, HEAD's first parent's when amending.
	var parents []string
	emptyTree := baseTree
	if o.amend {
		if parents, emptyTree, err = a.amendBase(h.commit, r); err != nil {
			return err
		}
		if err := a.checkUnpushed(h.commit); err != nil {
			return err
		}
		if msg == "" {
			old, err := a.git(call{}, "show", "-s", "--format=%B", h.commit)
			if err != nil {
				return err
			}
			if msg, err = a.cleanMessage(old); err != nil {
				return err
			}
		}
		// The amended commit keeps its author and author date, as with git
		// commit --amend; --author replaces only the name and email.
		info, err := a.git(call{}, "show", "-s", "--date=raw", "--format=%an%x00%ae%x00%ad", h.commit)
		if err != nil {
			return err
		}
		f := strings.SplitN(strings.TrimSuffix(info, "\n"), "\x00", 3)
		if len(f) != 3 {
			return fmt.Errorf("could not read the author of %s", h.commit)
		}
		author = append([]string{"GIT_AUTHOR_NAME=" + f[0], "GIT_AUTHOR_EMAIL=" + f[1], "GIT_AUTHOR_DATE=" + f[2]}, author...)
	} else if h.commit != "" {
		parents = []string{h.commit}
	}

	idx := filepath.Join(r.gitDir, fmt.Sprintf("commit-own-%d-%d.index", os.Getpid(), n))
	a.addTemp(idx, idx+".lock")
	defer os.Remove(idx)
	private := call{env: []string{"GIT_INDEX_FILE=" + idx}}
	staging := call{env: private.env, errs: true}

	if h.commit != "" {
		_, err = a.git(private, "read-tree", h.commit)
	} else {
		_, err = a.git(private, "read-tree", "--empty")
	}
	if err != nil {
		return err
	}
	if len(o.paths) > 0 {
		args := []string{"add", "-A"}
		if o.force {
			args = append(args, "-f")
		}
		args = append(append(args, "--"), o.paths...)
		if _, err := a.git(staging, args...); err != nil {
			return fmt.Errorf("staging the named paths failed; nothing was committed")
		}
	}
	if len(o.untrack) > 0 {
		args := append([]string{"rm", "--cached", "-r", "-q", "--"}, o.untrack...)
		if _, err := a.git(staging, args...); err != nil {
			return fmt.Errorf("--untrack failed; nothing was committed")
		}
	}
	for _, p := range patches {
		c := staging
		c.dir, c.stdin = r.top, p.data
		if _, err := a.git(c, "apply", "--cached", "-"); err != nil {
			return fmt.Errorf("patch %s does not apply to HEAD; nothing was committed", p.name)
		}
	}

	for _, p := range append(append([]string{}, o.paths...), o.untrack...) {
		_, err := a.git(private, "diff", "--cached", "--quiet", base, "--", p)
		if err == nil {
			a.warnf("note: %s has no changes against HEAD", p)
		} else if exitCode(err) != 1 {
			return err
		}
	}

	if !o.amend && !o.allowEmpty {
		if _, err := a.git(private, "diff", "--cached", "--quiet", base); err == nil {
			return errNothing
		} else if exitCode(err) != 1 {
			return err
		}
	}

	if o.dryRun {
		if o.amend {
			fmt.Fprintf(a.stdout, "Would amend %.10s on %s; the commit would contain:\n", h.commit, h.where())
		} else {
			fmt.Fprintf(a.stdout, "Would commit on %s:\n", h.where())
		}
		_, err := a.git(call{env: private.env, show: true}, "diff", "--cached", "--stat", emptyTree)
		return err
	}

	hook := call{env: append(append(private.env, "GIT_EDITOR=:"), author...), show: true}
	if !o.noVerify {
		if _, err := a.git(hook, "hook", "run", "--ignore-missing", "pre-commit"); err != nil {
			return fmt.Errorf("the pre-commit hook failed; nothing was committed")
		}
	}
	msgFile := filepath.Join(r.gitDir, fmt.Sprintf("commit-own-%d.msg", os.Getpid()))
	a.addTemp(msgFile)
	if err := os.WriteFile(msgFile, []byte(msg), 0o644); err != nil {
		return err
	}
	source := []string{msgFile, "message"}
	if o.amend && o.msgFile == "" && len(o.messages) == 0 {
		source = []string{msgFile, "commit", h.commit} // what git commit --amend --no-edit passes
	}
	if _, err := a.git(hook, append([]string{"hook", "run", "--ignore-missing", "prepare-commit-msg", "--"}, source...)...); err != nil {
		return fmt.Errorf("the prepare-commit-msg hook failed; nothing was committed")
	}
	if !o.noVerify {
		if _, err := a.git(hook, "hook", "run", "--ignore-missing", "commit-msg", "--", msgFile); err != nil {
			return fmt.Errorf("the commit-msg hook failed; nothing was committed")
		}
	}
	b, err := os.ReadFile(msgFile)
	if err != nil {
		return err
	}
	final, err := a.cleanMessage(string(b))
	if err != nil {
		return err
	}

	tree, err := a.line(private, "write-tree")
	if err != nil {
		return err
	}
	if tree == emptyTree && !o.allowEmpty {
		if o.amend {
			return fmt.Errorf("amending would leave the commit with no changes (pass --allow-empty to keep it anyway)")
		}
		return errNothing
	}

	args := []string{"commit-tree", tree}
	for _, p := range parents {
		args = append(args, "-p", p)
	}
	if r.sign {
		args = append(args, "-S")
	}
	args = append(args, "-F", "-")
	commit, err := a.line(call{stdin: final, env: author}, args...)
	if err != nil {
		if r.sign {
			return fmt.Errorf("%v\nsigning failed (commit.gpgSign is set); if the key is unavailable, retry with --no-gpg-sign", err)
		}
		return err
	}

	subject := strings.SplitN(final, "\n", 2)[0]
	kind := "commit"
	switch {
	case o.amend:
		kind = "commit (amend)"
	case h.commit == "":
		kind = "commit (initial)"
	}
	if _, err := a.git(call{}, "update-ref", "-m", kind+": "+subject, "HEAD", commit, h.commit); err != nil {
		now, herr := a.readHead()
		if herr == nil && now != h {
			return errRaced
		}
		return err
	}

	changed, err := a.git(call{}, "diff-tree", "-r", "-z", "--name-only", "--no-renames", "--no-commit-id", baseTree, tree)
	if err != nil {
		a.warnf("committed, but could not list the changed paths to update the shared index: %v", err)
	} else {
		a.syncIndex(r, commit, changed)
	}
	if _, err := a.git(call{env: []string{"GIT_EDITOR=:"}, show: true}, "hook", "run", "--ignore-missing", "post-commit"); err != nil {
		a.warnf("the post-commit hook failed (the commit stands)")
	}

	label := h.where()
	if len(parents) == 0 {
		label += " (root-commit)"
	}
	label = strings.ReplaceAll(label, "%", "%%")
	if _, err := a.git(call{show: true}, "show", "-s", "--format=["+label+" %h] %s", commit); err != nil {
		return err
	}
	_, err = a.git(call{show: true}, "-c", "color.ui=never", "diff-tree", "--stat", "--root", "--no-commit-id", "-r", commit)
	return err
}

// amendBase returns the parents of the commit being amended, and the tree of
// its first parent (the empty tree for a root commit).
func (a *app) amendBase(commit string, r *repo) ([]string, string, error) {
	out, err := a.line(call{}, "rev-parse", commit+"^@")
	if err != nil {
		return nil, "", err
	}
	if out == "" {
		return nil, r.empty, nil
	}
	parents := strings.Split(out, "\n")
	tree, err := a.line(call{}, "rev-parse", parents[0]+"^{tree}")
	return parents, tree, err
}

// checkUnpushed refuses to amend a commit that a remote-tracking branch
// already contains: replacing it rewrites history that others may have.
func (a *app) checkUnpushed(commit string) error {
	out, err := a.line(call{}, "for-each-ref", "--contains", commit, "--format=%(refname:short)", "refs/remotes")
	if err != nil {
		return err
	}
	if out != "" {
		return fmt.Errorf("%.10s is already pushed (it is in %s); amending would rewrite published history, so make a new commit instead",
			commit, strings.SplitN(out, "\n", 2)[0])
	}
	return nil
}

var authorForm = regexp.MustCompile(`^\s*([^<>]*?)\s*<([^<>]*)>\s*$`)

// resolveAuthor turns --author into GIT_AUTHOR_NAME and GIT_AUTHOR_EMAIL. As
// with git commit, the value is either "Name <email>", or a pattern matched
// against the authors of existing commits, the most recent match winning.
func (a *app) resolveAuthor(spec string) ([]string, error) {
	if spec == "" {
		return nil, nil
	}
	name, email := "", ""
	if m := authorForm.FindStringSubmatch(spec); m != nil {
		name, email = m[1], m[2]
	} else {
		out, err := a.git(call{}, "log", "-1", "--all", "-i", "--format=%an%x00%ae", "--author="+spec)
		if err != nil {
			return nil, err
		}
		if f := strings.SplitN(strings.TrimSuffix(out, "\n"), "\x00", 2); len(f) == 2 {
			name, email = f[0], f[1]
		} else {
			return nil, fmt.Errorf("--author %q is not \"Name <email>\", and matches no existing author", spec)
		}
	}
	if name == "" {
		return nil, fmt.Errorf("--author %q has no name", spec)
	}
	return []string{"GIT_AUTHOR_NAME=" + name, "GIT_AUTHOR_EMAIL=" + email}, nil
}

// syncIndex resets the shared index entries of the committed paths to the new
// commit. Without it the shared index still holds their old contents, and git
// status shows the commit undone, as staged changes. Other entries are not
// touched. The index lock may be held for a moment by another git command, so
// a few failures to take it are retried.
func (a *app) syncIndex(r *repo, commit, nulPaths string) {
	if nulPaths == "" {
		return
	}
	c := call{dir: r.top, stdin: nulPaths, env: []string{"GIT_LITERAL_PATHSPECS=1"}}
	var err error
	for i := 0; i < 50; i++ {
		if _, err = a.git(c, "reset", "-q", commit, "--pathspec-from-file=-", "--pathspec-file-nul"); err == nil {
			return
		}
		if !strings.Contains(err.Error(), "index.lock") {
			break
		}
		time.Sleep(100 * time.Millisecond)
	}
	a.warnf("committed, but could not update the shared index: %v\n  to fix git status, run: git reset -q %s -- <the committed paths>", err, commit)
}
