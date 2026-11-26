package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// Keep the real sync and Git commands. Stub the tmux probe and use a scratch
// commit-own build so neither the desktop tmux server nor the user's install
// is needed. Interactive cases get a controlling PTY and a scripted fzf.
func (e *e2e) configureTerminal(config string) {
	e.t.Helper()
	if _, err := exec.LookPath("flock"); err != nil {
		e.t.Skip("missing flock")
	}
	bin := filepath.Join(e.tmp, "bin")
	build := exec.Command("go", "build", "-o", filepath.Join(bin, "git-commit-own"), ".")
	build.Dir = "../git_commit_own"
	if out, err := build.CombinedOutput(); err != nil {
		e.t.Fatalf("go build git-commit-own: %v\n%s", err, out)
	}
	file, err := os.OpenFile(filepath.Join(e.tmp, "config.zsh"), os.O_APPEND|os.O_WRONLY, 0)
	must(e.t, err)
	_, err = file.WriteString("\nfunction tmux-job-running-p { return 1 }\n" + config + "\n")
	must(e.t, err)
	must(e.t, file.Close())
}

func (e *e2e) terminal(pty bool, args ...string) (string, int) {
	e.t.Helper()
	zsh := []string{"zsh", "-c", `daily-sync "$@"`, "zsh"}
	zsh = append(zsh, args...)
	cmd := exec.Command(zsh[0], zsh[1:]...)
	if pty {
		cmd = exec.Command("python3", append([]string{"-c", `
import errno, os, pty, sys
def read(fd):
    try:
        return os.read(fd, 8192)
    except OSError as e:
        if e.errno != errno.EIO:
            raise
        return b""
sys.exit(os.waitstatus_to_exitcode(pty.spawn(sys.argv[1:], master_read=read)))
`}, zsh...)...)
	}
	cmd.Env = e.env
	out, err := cmd.CombinedOutput()
	if exitErr, ok := err.(*exec.ExitError); ok {
		return string(out), exitErr.ExitCode()
	}
	must(e.t, err)
	return string(out), 0
}

func TestTerminalAutoCommitsAndKeepsGoing(t *testing.T) {
	e := newE2E(t)
	healthy := filepath.Join(e.tmp, "healthy")
	rel := filepath.Join(filepath.Dir(e.rel), "healthy")
	e.git(e.tmp, "init", "--quiet", "--bare", "healthy.git")
	e.git(e.tmp, "clone", "--quiet", "healthy.git", "healthy")
	e.write(healthy, "log.org", "base\n")
	e.git(healthy, "add", "log.org")
	e.git(healthy, "commit", "--quiet", "-m", "start")
	e.git(healthy, "push", "--quiet", "-u", "origin", "main")
	e.write(healthy, "log.org", "update\n")
	e.write(healthy, "new [note].org", "new\n")
	e.configureTerminal(fmt.Sprintf(`daily_sync_notes+=( %q )
nightNotes=%q
function log-to { shift; "$@"; }
function org-paper-metadata-updater {
    print refreshed > "${@[-1]}/metadata-ran"
    print updated-by-metadata >> "${@[-1]}/log.org"
}
`, rel, e.tmp))
	before := e.git(e.a, "rev-parse", "HEAD")
	out, rc := e.terminal(false, "auto")
	if rc != 1 || strings.Count(out, "CONFLICT "+e.rel+" (pull") != 1 || !strings.Contains(out, "COMMITTED "+rel) {
		t.Fatalf("auto exited %d:\n%s", rc, out)
	}
	if head := e.git(e.a, "rev-parse", "HEAD"); head != before || e.git(e.a, "status", "--porcelain") != "" {
		t.Fatalf("conflicted checkout changed: %s", head)
	}
	for _, file := range []string{"log.org", "new [note].org"} {
		if got := e.git(healthy, "show", "origin/main:"+file); got == "base" || got == "" {
			t.Fatalf("%s did not reach the remote: %q", file, got)
		}
	}
	if _, err := os.Stat(filepath.Join(healthy, "metadata-ran")); !os.IsNotExist(err) {
		t.Fatalf("metadata ran without the flag: %v", err)
	}
	out, rc = e.terminal(false, "auto", "--metadata-update")
	if rc != 1 || !strings.Contains(out, rel+": metadata updater exit 0") {
		t.Fatalf("metadata run exited %d:\n%s", rc, out)
	}
	if _, err := os.Stat(filepath.Join(e.a, "metadata-ran")); !os.IsNotExist(err) {
		t.Fatalf("metadata ran in the conflicted repo: %v", err)
	}
	if got := e.git(healthy, "show", "origin/main:log.org"); got != "update\nupdated-by-metadata" {
		t.Fatalf("the metadata update did not reach the remote: %q", got)
	}
}

func TestTerminalInteractiveResolvesNextCommit(t *testing.T) {
	e := newE2E(t)
	e.configureTerminal("")
	e.write(e.a, "next.org", "local next\n")
	e.git(e.a, "add", "next.org")
	e.git(e.a, "commit", "--quiet", "-m", "local next")
	e.write(e.b, "next.org", "remote next\n")
	e.git(e.b, "add", "next.org")
	e.git(e.b, "commit", "--quiet", "-m", "remote next")
	e.git(e.b, "push", "--quiet")
	fzf := filepath.Join(e.tmp, "bin", "fzf")
	must(t, os.WriteFile(fzf, []byte("#!/bin/sh\ncat >/dev/null\nexit 130\n"), 0o700))
	before := e.git(e.a, "rev-parse", "HEAD")
	out, rc := e.terminal(true, "interactive")
	if rc != 1 || !strings.Contains(out, "ABORTED "+e.rel) ||
		e.git(e.a, "rev-parse", "HEAD") != before || e.git(e.a, "status", "--porcelain") != "" {
		t.Fatalf("skipping the conflict exited %d:\n%s", rc, out)
	}
	must(t, os.WriteFile(fzf, []byte(`#!/bin/sh
test -s "$DAILY_SYNC_PREVIEW" || exit 1
awk -F '\t' '$1 == "local" {print}'
`), 0o700))
	out, rc = e.terminal(true, "interactive")
	if rc != 0 || !strings.Contains(out, "RESOLVED "+e.rel) {
		t.Fatalf("interactive exited %d:\n%s", rc, out)
	}
	if got := e.git(e.a, "show", "origin/main:next.org"); got != "local next" {
		t.Fatalf("next conflict's pick did not reach the remote: %q", got)
	}
}

func TestTerminalPublicApprovalCommitsOnlyReviewedPatch(t *testing.T) {
	e := newE2E(t)
	e.git(e.a, "fetch", "--quiet")
	e.git(e.a, "reset", "--hard", "origin/main")
	e.configureTerminal(fmt.Sprintf(`nightNotesPublic=%q
export TEST_EDIT=%q
`, e.a, filepath.Join(e.a, "a.org")))
	fzf := filepath.Join(e.tmp, "bin", "fzf")
	must(t, os.WriteFile(fzf, []byte("#!/bin/sh\nawk -F '\\t' '$1 == \"skip\" {print}'\n"), 0o700))
	e.write(e.a, "a.org", "approved version\n")
	out, rc := e.terminal(true, "auto", "--approve-edits")
	if rc != 1 || !strings.Contains(out, "approval skipped") {
		t.Fatalf("skipping approval exited %d:\n%s", rc, out)
	}
	if got := e.git(e.a, "show", "HEAD:a.org"); got == "approved version" {
		t.Fatal("the skipped diff was committed")
	}
	must(t, os.WriteFile(fzf, []byte(`#!/bin/sh
grep -q '^+approved version$' "$DAILY_SYNC_PREVIEW" || exit 1
printf 'newer edit\n' > "$TEST_EDIT"
awk -F '\t' '$1 == "approve" {print}'
`), 0o700))
	out, rc = e.terminal(true, "auto", "--approve-edits")
	if rc != 0 {
		t.Fatalf("approving the patch exited %d:\n%s", rc, out)
	}
	if got := e.git(e.a, "show", "origin/main:a.org"); got != "approved version" {
		t.Fatalf("the reviewed version was not pushed: %q", got)
	}
	body, err := os.ReadFile(filepath.Join(e.a, "a.org"))
	must(t, err)
	if string(body) != "newer edit\n" {
		t.Fatalf("newer working copy edit was lost: %q", body)
	}
	out, rc = e.terminal(false, "auto")
	if rc != 0 || e.git(e.a, "show", "origin/main:a.org") != "newer edit" {
		t.Fatalf("auto did not commit and sync the public edit without approval (exit %d):\n%s", rc, out)
	}
}
