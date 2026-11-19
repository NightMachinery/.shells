package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// The shipping test: daily-sync builds the conflict engine and ships it to
// another machine before a conflict command. The other machine is a scratch
// home reached through a fake ssh that runs the command locally, with HOME
// pointed at that home and a PATH holding only zsh, git and a few
// coreutils: no Go and no perl. The fake ssh knows one host and never
// contacts a real one. Like the step 1 test it needs the night zsh setup;
// `go test -short' skips it.

type shipE2E struct {
	*e2e
	remote string // the other machine's home
	bin    string // where the local build sits
	sshLog string
	hash   string
}

const fakeSSH = `#!/bin/sh
# A fake ssh for the daily-sync tests: options are skipped, the one known
# host runs the command under sh in a scratch home, anything else fails.
while [ $# -gt 0 ]; do
	case "$1" in
	-o) shift 2 ;;
	-*) shift ;;
	*) break ;;
	esac
done
host="$1"
shift
cmd="$*"
case "$cmd" in
*upload-*) kind=upload ;;
*'uname -s -m'*) kind=probe ;;
'zsh -s') kind=script ;;
*) kind=other ;;
esac
printf '%s %s\n' "$host" "$kind" >> "$FAKE_SSH_LOG"
if [ "$host" != remotebox ]; then
	echo "ssh: Could not resolve hostname $host: nodename nor servname provided, or not known" >&2
	exit 255
fi
exec /usr/bin/env -i HOME="$FAKE_REMOTE_HOME" PATH="$FAKE_REMOTE_PATH" GIT_CONFIG_GLOBAL="$GIT_CONFIG_GLOBAL" \
	GIT_CONFIG_NOSYSTEM=1 TMPDIR="$TMPDIR" LANG=C /bin/sh -c "$cmd"
`

func newShipE2E(t *testing.T) *shipE2E {
	t.Helper()
	if testing.Short() {
		t.Skip("end-to-end test skipped with -short")
	}
	for _, c := range []string{"zsh", "git", "go"} {
		if _, err := exec.LookPath(c); err != nil {
			t.Skip("missing", c)
		}
	}
	if err := exec.Command("zsh", "-c", "whence -w daily-sync h-daily-sync-engine >/dev/null").Run(); err != nil {
		t.Skip("zsh -c does not load daily-sync (with h-daily-sync-engine)")
	}
	tmp, err := filepath.EvalSymlinks(t.TempDir())
	must(t, err)
	s := &shipE2E{remote: filepath.Join(tmp, "remote-home"), bin: filepath.Join(tmp, "bin"), sshLog: filepath.Join(tmp, "ssh.log")}
	s.e2e = &e2e{t: t, tmp: tmp, a: filepath.Join(s.remote, "a"), b: filepath.Join(tmp, "b"), rel: "a",
		run: filepath.Join(tmp, "runs", "run1"), stepJob: fmt.Sprintf("ds-ship-step-%d", os.Getpid()), metaJob: fmt.Sprintf("ds-ship-meta-%d", os.Getpid())}
	must(t, os.MkdirAll(s.bin, 0o700))
	must(t, os.MkdirAll(s.run, 0o700))
	must(t, os.MkdirAll(s.remote, 0o700))
	if out, err := exec.Command("go", "build", "-o", filepath.Join(s.bin, "daily_sync"), ".").CombinedOutput(); err != nil {
		t.Fatalf("go build: %v\n%s", err, out)
	}
	out, err := exec.Command(filepath.Join(s.bin, "daily_sync"), "version").Output()
	must(t, err)
	s.hash = strings.TrimSpace(string(out))
	must(t, os.WriteFile(filepath.Join(s.bin, "ssh"), []byte(fakeSSH), 0o700))

	// The other machine's PATH: the tools the prelude and the shipping use, nothing else.
	rpath := filepath.Join(tmp, "remote-path")
	must(t, os.MkdirAll(rpath, 0o700))
	for _, c := range []string{"zsh", "git", "uname", "mkdir", "chmod", "mv", "rm", "cat", "touch", "mktemp", "grep", "tr", "tail", "sh", "true"} {
		p, err := exec.LookPath(c)
		if err != nil {
			t.Skip("missing", c)
		}
		must(t, os.Symlink(p, filepath.Join(rpath, c)))
	}

	config := filepath.Join(tmp, "config.zsh")
	must(t, os.WriteFile(config, []byte(fmt.Sprintf(`path=( %q $path )
daily_sync_local=testbox
daily_sync_peers=( remotebox )
daily_sync_server=''
daily_sync_notes=( a )
daily_sync_private_code=()
daily_sync_public=()
daily_sync_runs=%q
daily_sync_engine_cache=%q
daily_sync_step_job=%s
daily_sync_job=%s
function h-daily-sync-notify { print -r -- "(test: no notification: $1)" }
function notif-tlg-heart { : }
`, s.bin, filepath.Join(tmp, "runs"), filepath.Join(tmp, "engine-cache"), s.stepJob, s.metaJob)), 0o600))
	gitconfig := filepath.Join(tmp, "gitconfig")
	must(t, os.WriteFile(gitconfig, []byte("[user]\n\tname = Tester\n\temail = tester@example.invalid\n[commit]\n\tgpgsign = false\n[init]\n\tdefaultBranch = main\n"), 0o600))
	for _, kv := range os.Environ() {
		k, _, _ := strings.Cut(kv, "=")
		if strings.HasPrefix(k, "GIT_") || strings.HasPrefix(k, "AGENT_DESK_") || strings.HasPrefix(k, "daily_sync") || strings.HasPrefix(k, "FAKE_") || k == "PASEO_AGENT_ID" {
			continue
		}
		s.env = append(s.env, kv)
	}
	s.env = append(s.env, "GIT_CONFIG_GLOBAL="+gitconfig, "GIT_CONFIG_NOSYSTEM=1", "daily_sync_config="+config,
		"AGENT_DESK_HOME="+filepath.Join(tmp, "desk"), "FAKE_REMOTE_HOME="+s.remote, "FAKE_REMOTE_PATH="+rpath, "FAKE_SSH_LOG="+s.sshLog)

	// The same conflict as the step 1 test, in a clone in the other machine's home.
	s.git(tmp, "init", "--quiet", "--bare", "origin.git")
	s.git(tmp, "clone", "--quiet", "origin.git", s.a)
	s.write(s.a, "a.org", "* Title\nline one\nline two\n"+filler("x", 8)+"line eight\nend\n")
	s.write(s.a, "b.org", "* Log\nfirst entry\n")
	s.git(s.a, "add", ".")
	s.git(s.a, "commit", "--quiet", "-m", "start")
	s.git(s.a, "push", "--quiet", "origin", "main")
	s.git(tmp, "clone", "--quiet", "origin.git", "b")
	s.write(s.b, "a.org", "* Title\nline one\nline two remote\n"+filler("x", 8)+"line eight remote\nend\n")
	s.write(s.b, "b.org", "* Log\nfirst entry\nremote entry\n")
	s.git(s.b, "commit", "--quiet", "-am", "remote edit")
	s.git(s.b, "push", "--quiet")
	s.write(s.a, "a.org", "* Title\nline one\nline two local\n"+filler("x", 8)+"line eight local\nend\n")
	s.write(s.a, "b.org", "* Log\nfirst entry\nlocal entry\n")
	s.git(s.a, "commit", "--quiet", "-am", "local edit")
	return s
}

// zshf runs `h-daily-sync-config && <script>' with the arguments as $1....
func (s *shipE2E) zshf(script string, args ...string) (string, error) {
	cmd := exec.Command("zsh", append([]string{"-c", "h-daily-sync-config && " + script, "zsh"}, args...)...)
	cmd.Env = append(s.env, "daily_sync_run="+s.run)
	out, err := cmd.CombinedOutput()
	return string(out), err
}

func (s *shipE2E) sshCalls() string {
	b, _ := os.ReadFile(s.sshLog)
	return string(b)
}

func (s *shipE2E) remoteBins() []string {
	m, _ := filepath.Glob(filepath.Join(s.remote, ".cache", "daily-sync", "bin", "*"))
	var names []string
	for _, p := range m {
		names = append(names, filepath.Base(p))
	}
	return names
}

func TestShipEngine(t *testing.T) {
	s := newShipE2E(t)
	// Four older versions are there already; only the three newest binaries stay.
	dir := filepath.Join(s.remote, ".cache", "daily-sync", "bin")
	must(t, os.MkdirAll(dir, 0o700))
	for i := 1; i <= 4; i++ {
		p := filepath.Join(dir, fmt.Sprintf("daily_sync-%016d", i))
		must(t, os.WriteFile(p, []byte("#!/bin/sh\necho old\n"), 0o700))
		at := time.Now().Add(-time.Duration(10-i) * time.Hour)
		must(t, os.Chtimes(p, at, at))
	}

	out := s.daily("conflict", "remotebox", "a")
	t.Logf("conflict:\n%s", out)
	if !strings.Contains(out, "CONFLICT-OPEN a: 2 file(s)") || !strings.Contains(out, "### a.org, hunk 2 of 2") || !strings.Contains(out, "### b.org, hunk 1 of 1, near line 3: BOTH-ONLY-ADDED (suggested: 1=both)") {
		log, _ := os.ReadFile(filepath.Join(s.run, "stderr.log"))
		t.Fatalf("no listing from the shipped engine:\n%s\nstderr.log:\n%s", out, log)
	}
	if got, want := s.sshCalls(), "remotebox probe\nremotebox upload\nremotebox script\n"; got != want {
		t.Fatalf("ssh calls:\n%s\nwant:\n%s", got, want)
	}
	shipped := "daily_sync-" + s.hash
	if got, want := strings.Join(s.remoteBins(), " "), "daily_sync-0000000000000003 daily_sync-0000000000000004 "+shipped; got != want {
		t.Fatalf("remote binaries: %s\nwant: %s", got, want)
	}
	if v, err := exec.Command(filepath.Join(dir, shipped), "version").Output(); err != nil || strings.TrimSpace(string(v)) != s.hash {
		t.Fatalf("the shipped engine's version: %q %v, want %s", v, err, s.hash)
	}
	state, _ := os.ReadFile(filepath.Join(s.run, "engine-remotebox"))
	if lines := strings.Split(strings.TrimSpace(string(state)), "\n"); len(lines) != 3 || lines[1] != s.remote || lines[2] != s.hash {
		t.Fatalf("engine-remotebox:\n%s", state)
	}

	// The run remembers it: resolving costs no probe and no upload.
	out = s.daily("resolve", "remotebox", "a", "a.org", "1=upstream", "2=local")
	if !strings.Contains(out, "a.org hunk 2 -> local, now reads:") || !strings.Contains(out, "STILL-CONFLICTED a: b.org") {
		t.Fatalf("resolve a.org:\n%s", out)
	}
	out = s.daily("resolve", "remotebox", "a", "b.org", "1=both")
	if !strings.Contains(out, "RESOLVED a") {
		t.Fatalf("resolve b.org:\n%s", out)
	}
	if got, want := s.sshCalls(), "remotebox probe\nremotebox upload\nremotebox script\nremotebox script\nremotebox script\n"; got != want {
		t.Fatalf("ssh calls:\n%s\nwant:\n%s", got, want)
	}
	a, _ := os.ReadFile(filepath.Join(s.a, "a.org"))
	if want := "* Title\nline one\nline two remote\n" + filler("x", 8) + "line eight local\nend\n"; string(a) != want {
		t.Fatalf("a.org:\n%s", a)
	}
	b, _ := os.ReadFile(filepath.Join(s.a, "b.org"))
	if want := "* Log\nfirst entry\nremote entry\nlocal entry\n"; string(b) != want {
		t.Fatalf("b.org:\n%s", b)
	}

	// A binary that went missing: the prelude says so, the run forgets it, and the next call ships it again.
	must(t, os.Remove(filepath.Join(dir, shipped)))
	if out, _ := s.zshf(`h-daily-sync-engine-forget remotebox "ERROR: no conflict engine at 'x'"; h-daily-sync-engine remotebox`); strings.TrimSpace(out) != filepath.Join(dir, shipped) {
		t.Fatalf("reship:\n%s", out)
	}
	if !strings.HasSuffix(s.sshCalls(), "remotebox probe\nremotebox upload\n") {
		t.Fatalf("ssh calls:\n%s", s.sshCalls())
	}
	// Present already, in a new run: a probe and no upload.
	s.run = filepath.Join(s.tmp, "runs", "run2")
	must(t, os.MkdirAll(s.run, 0o700))
	if out, _ := s.zshf(`h-daily-sync-engine remotebox`); strings.TrimSpace(out) != filepath.Join(dir, shipped) {
		t.Fatalf("probe only:\n%s", out)
	}
	if !strings.HasSuffix(s.sshCalls(), "remotebox upload\nremotebox probe\n") {
		t.Fatalf("ssh calls:\n%s", s.sshCalls())
	}
}

func TestShipEngineFailures(t *testing.T) {
	s := newShipE2E(t)
	// Unreachable: an ERROR line, and nothing is opened.
	out := s.daily("conflict", "deadbox", "a")
	if !strings.HasPrefix(out, "ERROR: could not reach deadbox to ship the conflict engine") || strings.Contains(out, "CONFLICT-OPEN") {
		t.Fatalf("unreachable:\n%s", out)
	}
	if out := s.daily("resolve", "deadbox", "a", "a.org", "1=local"); !strings.HasPrefix(out, "ERROR: could not reach deadbox") {
		t.Fatalf("unreachable resolve:\n%s", out)
	}
	if log, _ := os.ReadFile(filepath.Join(s.run, "resolutions.log")); len(log) > 0 {
		t.Fatalf("a failed resolve was recorded:\n%s", log)
	}
	// The upload cannot be written: an ERROR line, and no conflict is opened.
	dir := filepath.Join(s.remote, ".cache", "daily-sync", "bin")
	must(t, os.MkdirAll(dir, 0o700))
	must(t, os.Chmod(dir, 0o500))
	t.Cleanup(func() { os.Chmod(dir, 0o700) })
	out = s.daily("conflict", "remotebox", "a")
	if !strings.HasPrefix(out, "ERROR: could not upload the conflict engine to remotebox") || s.inProgress() {
		t.Fatalf("upload refused:\n%s", out)
	}
	if m, _ := filepath.Glob(filepath.Join(dir, "*")); len(m) != 0 {
		t.Fatalf("left behind: %v", m)
	}
	// The prelude without an engine prints an ERROR line instead of a listing.
	script := "ds_engine=/nonexistent\nds-conflict-show a remotebox\n"
	cmd := exec.Command("zsh", "-c", `h-daily-sync-config && h-daily-sync-on remotebox`)
	cmd.Env = append(s.env, "daily_sync_run="+s.run)
	cmd.Stdin = strings.NewReader(script)
	b, _ := cmd.CombinedOutput()
	if !strings.Contains(string(b), "ERROR: no conflict engine at '/nonexistent'") || !strings.Contains(string(b), "ERROR: could not read a.org") {
		t.Fatalf("no engine:\n%s", b)
	}
	s.git(s.a, "rebase", "--abort")
}

func TestShipEnginePlatforms(t *testing.T) {
	s := newShipE2E(t)
	for in, want := range map[string]string{"Darwin arm64": "darwin arm64", "Darwin x86_64": "darwin amd64", "Linux aarch64": "linux arm64", "Linux x86_64": "linux amd64", "FreeBSD amd64": "", "Linux riscv64": ""} {
		out, err := s.zshf(`h-daily-sync-engine-platform ${=1}`, in)
		if got := strings.TrimSpace(out); (err != nil) != (want == "") || got != want {
			t.Errorf("platform %q: %q %v, want %q", in, got, err, want)
		}
	}
	out, err := s.zshf(`h-daily-sync-engine-build linux amd64 "$1"`, s.hash)
	if err != nil {
		t.Fatalf("cross-build: %v\n%s", err, out)
	}
	p := strings.TrimSpace(out)
	if want := filepath.Join(s.tmp, "engine-cache", "daily_sync-"+s.hash+"-linux-amd64"); p != want {
		t.Fatalf("cross-build path %q, want %q", p, want)
	}
	b, err := os.ReadFile(p)
	if err != nil || !strings.HasPrefix(string(b), "\x7fELF") {
		t.Fatalf("not an ELF binary: %v", err)
	}
	// The local machine uses its own build.
	if out, _ := s.zshf(`h-daily-sync-engine testbox`); strings.TrimSpace(out) != filepath.Join(s.bin, "daily_sync") {
		t.Fatalf("local engine: %q", out)
	}
}
