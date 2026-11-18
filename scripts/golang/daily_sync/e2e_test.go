package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// The end-to-end test stages a real rebase conflict between two clones of a
// bare repository under $TMPDIR and drives the real zsh functions with a
// scratch configuration: `daily-sync conflict' opens and aborts it, the
// desk question goes to a stub agent-desk that records the --on-answer
// command, and that command runs as agent-desk would run it. It needs the
// night zsh setup (daily-sync loaded by `zsh -c'), git, python3 and tmux;
// `go test -short' skips it. It never touches the real configuration, run
// directory or desk.

type e2e struct {
	t       *testing.T
	tmp     string // scratch root
	a, b    string // the clone daily-sync drives, and the other writer
	rel     string // a, relative to ~, as daily-sync names repositories
	run     string // the scratch run directory
	env     []string
	stubLog string
	stepJob string
	metaJob string
}

const e2eMachine = "testbox"

func newE2E(t *testing.T) *e2e {
	t.Helper()
	if testing.Short() {
		t.Skip("end-to-end test skipped with -short")
	}
	for _, c := range []string{"zsh", "git", "python3", "tmux"} {
		if _, err := exec.LookPath(c); err != nil {
			t.Skip("missing", c)
		}
	}
	if err := exec.Command("zsh", "-c", "whence -w daily-sync h-daily-sync-busy >/dev/null").Run(); err != nil {
		t.Skip("zsh -c does not load daily-sync (with h-daily-sync-busy)")
	}
	tmp, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	home, err := os.UserHomeDir()
	if err != nil {
		t.Fatal(err)
	}
	e := &e2e{t: t, tmp: tmp, a: filepath.Join(tmp, "a"), b: filepath.Join(tmp, "b"),
		run: filepath.Join(tmp, "runs", "run1"), stubLog: filepath.Join(tmp, "agent-desk.log"),
		stepJob: fmt.Sprintf("ds-e2e-step-%d", os.Getpid()), metaJob: fmt.Sprintf("ds-e2e-meta-%d", os.Getpid())}
	if e.rel, err = filepath.Rel(home, e.a); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() {
		for _, j := range []string{e.stepJob, e.metaJob} {
			exec.Command("tmux", "kill-session", "-t", "="+j).Run()
		}
	})

	bin := filepath.Join(tmp, "bin")
	must(t, os.MkdirAll(bin, 0o700))
	must(t, os.MkdirAll(e.run, 0o700))
	if out, err := exec.Command("go", "build", "-o", filepath.Join(bin, "daily_sync"), ".").CombinedOutput(); err != nil {
		t.Fatalf("go build: %v\n%s", err, out)
	}
	// A stub desk: it records each call, and the question's text on stdin.
	must(t, os.WriteFile(filepath.Join(bin, "agent-desk"), []byte(`#!/bin/sh
{ echo CALL; for a in "$@"; do printf '%s\n' "$a"; done; } >> "`+e.stubLog+`"
if [ "$1" = ask ]; then cat > "`+e.stubLog+`.stdin"; echo q-test; fi
exit 0
`), 0o700))
	// zsh's startup rebuilds PATH, so the configuration puts the stubs first.
	config := filepath.Join(tmp, "config.zsh")
	must(t, os.WriteFile(config, []byte(fmt.Sprintf(`path=( %q $path )
daily_sync_local=%s
daily_sync_peers=()
daily_sync_server=''
daily_sync_notes=( %q )
daily_sync_private_code=()
daily_sync_public=()
daily_sync_runs=%q
daily_sync_step_job=%s
daily_sync_job=%s
function h-daily-sync-notify { print -r -- "(test: no notification: $1)" }
function notif-tlg-heart { : }
`, bin, e2eMachine, e.rel, filepath.Join(tmp, "runs"), e.stepJob, e.metaJob)), 0o600))

	gitconfig := filepath.Join(tmp, "gitconfig")
	must(t, os.WriteFile(gitconfig, []byte("[user]\n\tname = Tester\n\temail = tester@example.invalid\n[commit]\n\tgpgsign = false\n[init]\n\tdefaultBranch = main\n"), 0o600))
	for _, kv := range os.Environ() {
		k, _, _ := strings.Cut(kv, "=")
		if strings.HasPrefix(k, "GIT_") || strings.HasPrefix(k, "AGENT_DESK_") || strings.HasPrefix(k, "daily_sync") || k == "PASEO_AGENT_ID" {
			continue
		}
		e.env = append(e.env, kv)
	}
	e.env = append(e.env, "GIT_CONFIG_GLOBAL="+gitconfig, "GIT_CONFIG_NOSYSTEM=1",
		"daily_sync_config="+config, "AGENT_DESK_HOME="+filepath.Join(tmp, "desk"))

	e.git(tmp, "init", "--quiet", "--bare", "origin.git")
	e.git(tmp, "clone", "--quiet", "origin.git", "a")
	e.write(e.a, "a.org", "* Title\nline one\nline two\n"+filler("x", 8)+"line eight\nend\n")
	e.write(e.a, "b.org", "* Log\nfirst entry\n")
	e.git(e.a, "add", ".")
	e.git(e.a, "commit", "--quiet", "-m", "start")
	e.git(e.a, "push", "--quiet", "origin", "main")
	e.git(tmp, "clone", "--quiet", "origin.git", "b")
	// The remote and this machine edit the same lines, and both add an entry.
	e.write(e.b, "a.org", "* Title\nline one\nline two remote\n"+filler("x", 8)+"line eight remote\nend\n")
	e.write(e.b, "b.org", "* Log\nfirst entry\nremote entry\n")
	e.git(e.b, "commit", "--quiet", "-am", "remote edit")
	e.git(e.b, "push", "--quiet")
	e.write(e.a, "a.org", "* Title\nline one\nline two local\n"+filler("x", 8)+"line eight local\nend\n")
	e.write(e.a, "b.org", "* Log\nfirst entry\nlocal entry\n")
	e.git(e.a, "commit", "--quiet", "-am", "local edit")
	return e
}

func filler(p string, n int) string {
	var b strings.Builder
	for i := 1; i <= n; i++ {
		fmt.Fprintf(&b, "%s filler %d\n", p, i)
	}
	return b.String()
}

func must(t *testing.T, err error) {
	t.Helper()
	if err != nil {
		t.Fatal(err)
	}
}

func (e *e2e) write(dir, name, s string) {
	e.t.Helper()
	must(e.t, os.WriteFile(filepath.Join(dir, name), []byte(s), 0o644))
}

func (e *e2e) git(dir string, args ...string) string {
	e.t.Helper()
	cmd := exec.Command("git", append([]string{"-C", dir}, args...)...)
	cmd.Env = e.env
	out, err := cmd.CombinedOutput()
	if err != nil {
		e.t.Fatalf("git %v: %v\n%s", args, err, out)
	}
	return strings.TrimSpace(string(out))
}

// daily runs `daily-sync <args>' in the scratch run, as the sync agent would.
func (e *e2e) daily(args ...string) string {
	e.t.Helper()
	cmd := exec.Command("zsh", append([]string{"-c", `daily-sync "$@"`, "zsh"}, args...)...)
	cmd.Env = append(e.env, "daily_sync_run="+e.run)
	out, _ := cmd.CombinedOutput()
	return string(out)
}

// ask opens the conflict and leaves it blocked, which files the question;
// it returns the --on-answer command.
func (e *e2e) ask() string {
	e.t.Helper()
	if out := e.daily("conflict", e2eMachine, e.rel); !strings.Contains(out, "CONFLICT-OPEN") || !strings.Contains(out, "hunk 2 of 2") {
		e.t.Fatalf("conflict did not open:\n%s", out)
	}
	if out := e.daily("conflict", e2eMachine, e.rel, "abort"); !strings.Contains(out, "ABORTED") || !strings.Contains(out, "filed on agent-desk") {
		e.t.Fatalf("abort did not file the question:\n%s", out)
	}
	b, err := os.ReadFile(e.stubLog)
	must(e.t, err)
	lines := strings.Split(string(b), "\n")
	cmd := ""
	for i, l := range lines {
		if l == "--on-answer" && i+1 < len(lines) {
			cmd = lines[i+1]
		}
	}
	if cmd == "" {
		e.t.Fatalf("no --on-answer in the desk calls:\n%s", b)
	}
	q, _ := os.ReadFile(e.stubLog + ".stdin")
	if !strings.Contains(string(q), "applied automatically") {
		e.t.Fatalf("question text:\n%s", q)
	}
	return cmd
}

// answer runs the on-answer command as agent-desk does: under sh -c, the answer on stdin.
func (e *e2e) answer(cmd, text string) (string, int) {
	e.t.Helper()
	c := exec.Command("sh", "-c", cmd)
	c.Dir = e.tmp
	c.Env = e.env
	c.Stdin = strings.NewReader(text)
	out, err := c.CombinedOutput()
	rc := 0
	if ee, ok := err.(*exec.ExitError); ok {
		rc = ee.ExitCode()
	} else if err != nil {
		e.t.Fatal(err)
	}
	return string(out), rc
}

func (e *e2e) inProgress() bool {
	for _, p := range []string{"rebase-merge", "rebase-apply", "MERGE_HEAD"} {
		if _, err := os.Stat(filepath.Join(e.a, ".git", p)); err == nil {
			return true
		}
	}
	return false
}

const e2eAnswer = "1. upstream: Keep the remote's version\n2. local: Keep testbox's version\n3. both: Keep both, the remote's first"

func TestEndToEndApplied(t *testing.T) {
	e := newE2E(t)
	cmd := e.ask()
	out, rc := e.answer(cmd, e2eAnswer)
	t.Logf("on-answer output (exit %d):\n%s", rc, out)
	if rc != 0 || !strings.HasPrefix(out, "Applied 3 pick(s) to "+e.rel+" on testbox; converged.") {
		t.Fatalf("exit %d:\n%s", rc, out)
	}
	if e.inProgress() {
		t.Fatal("a rebase is still in progress")
	}
	a, _ := os.ReadFile(filepath.Join(e.a, "a.org"))
	if want := "* Title\nline one\nline two remote\n" + filler("x", 8) + "line eight local\nend\n"; string(a) != want {
		t.Fatalf("a.org:\n%s", a)
	}
	b, _ := os.ReadFile(filepath.Join(e.a, "b.org"))
	if want := "* Log\nfirst entry\nremote entry\nlocal entry\n"; string(b) != want {
		t.Fatalf("b.org:\n%s", b)
	}
	if head, origin := e.git(e.a, "rev-parse", "HEAD"), e.git(e.tmp+"/origin.git", "rev-parse", "main"); head != origin {
		t.Fatalf("converge did not push: HEAD %s, origin %s", head, origin)
	}
	if log, _ := os.ReadFile(filepath.Join(e.run, "conflicts.log")); !strings.Contains(string(log), "RESOLVED testbox") {
		t.Fatalf("conflicts.log:\n%s", log)
	}
}

func TestEndToEndChangedSinceAsked(t *testing.T) {
	e := newE2E(t)
	cmd := e.ask()
	before := e.git(e.a, "rev-parse", "HEAD")
	// The remote edits the first hunk again after the question was filed.
	e.write(e.b, "a.org", "* Title\nline one\nline two remote, again\n"+filler("x", 8)+"line eight remote\nend\n")
	e.git(e.b, "commit", "--quiet", "-am", "remote edit again")
	e.git(e.b, "push", "--quiet")
	out, rc := e.answer(cmd, e2eAnswer)
	t.Logf("on-answer output (exit %d):\n%s", rc, out)
	if rc == 0 || !strings.HasPrefix(out, "Not applied: a.org, hunk 1 of 2 changed since the question; handed to the agent.") ||
		!strings.Contains(out, "The reopened conflict was aborted") {
		t.Fatalf("exit %d:\n%s", rc, out)
	}
	if e.inProgress() || e.git(e.a, "rev-parse", "HEAD") != before {
		t.Fatal("the repository was not put back as it was")
	}
}

func TestEndToEndOwnWords(t *testing.T) {
	e := newE2E(t)
	cmd := e.ask()
	before := e.git(e.a, "rev-parse", "HEAD")
	out, rc := e.answer(cmd, strings.Replace(e2eAnswer, "2. local: Keep testbox's version", "2. Other: merge them by hand", 1))
	t.Logf("on-answer output (exit %d):\n%s", rc, out)
	if rc == 0 || !strings.HasPrefix(out, "Not applied: question 2 (a.org, hunk 2 of 2) was answered in your own words; handed to the agent.") {
		t.Fatalf("exit %d:\n%s", rc, out)
	}
	if e.inProgress() || e.git(e.a, "rev-parse", "HEAD") != before {
		t.Fatal("the repository changed")
	}
}

func TestEndToEndBusy(t *testing.T) {
	e := newE2E(t)
	cmd := e.ask()
	before := e.git(e.a, "rev-parse", "HEAD")
	// A slow step of the sync is running: the answer must wait for the agent.
	if out, err := exec.Command("tmux", "new-session", "-d", "-s", e.stepJob, "sleep 120").CombinedOutput(); err != nil {
		t.Fatalf("tmux: %v\n%s", err, out)
	}
	out, rc := e.answer(cmd, e2eAnswer)
	t.Logf("on-answer output (exit %d):\n%s", rc, out)
	if rc == 0 || !strings.HasPrefix(out, "Not applied: a daily-sync job is running right now (BUSY "+e.stepJob+"); handed to the agent.") {
		t.Fatalf("exit %d:\n%s", rc, out)
	}
	if e.inProgress() || e.git(e.a, "rev-parse", "HEAD") != before {
		t.Fatal("the repository changed")
	}
}
