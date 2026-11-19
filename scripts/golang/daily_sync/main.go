// daily_sync: helpers for daily-sync (zshlang/auto-load/others/daily-sync.zsh)
// that run on the machine that drives the sync. The zsh commands stay the
// hands: this program calls `daily-sync conflict', `daily-sync resolve' and
// `daily-sync converge' through zsh, so the other machines still need only
// git and zsh.
//
// apply-answer <sidecar.json>, the answer on stdin: agent-desk runs it
// (`ask --on-answer') when the user answers a blocked-conflict question that
// h-daily-sync-desk-ask filed. The sidecar holds the hunks as they were
// asked. When every hunk has a plain pick and the reopened conflict still
// matches the question, it applies the picks, converges, and exits 0, so no
// agent is woken. On any doubt it puts back what it found (the reopened
// conflict is aborted, leaving the repository blocked as before) and exits
// nonzero, and the desk hands the answer and this output to the sync agent.
//
// Invoked as `daily-sync apply-answer', which builds it with go-local-dep.
//
// conflict show|resolve ...: the conflict engine (conflict.go), which the
// local machine ships to the other machines; version prints the hash of the
// source it was built from. See scripts/docs/daily-sync.md. Stdlib only.
package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"syscall"
	"time"
)

const usageText = `usage: daily_sync apply-answer <sidecar.json>   (the answer on stdin)
       daily_sync conflict show|resolve <rebase|merge> <label> <path> <name> [<hunk>=<choice>...]
       daily_sync version

apply-answer applies the user's answer to a blocked-conflict question of
daily-sync, or explains why not and exits nonzero so the sync agent takes over.
conflict lists or resolves the hunks of one conflicted file; version prints the
hash of the source this build came from.
See scripts/docs/daily-sync.md.
`

// budget is how long apply-answer may take: agent-desk stops an on-answer
// command after 10 minutes.
const budget = 9*time.Minute + 15*time.Second

// The desk shows the last 4,000 bytes of the output.
const maxOutput = 3800

func main() {
	if len(os.Args) < 2 || os.Args[1] == "-h" || os.Args[1] == "--help" {
		fmt.Fprint(os.Stderr, usageText)
		if len(os.Args) < 2 {
			os.Exit(64)
		}
		return
	}
	switch os.Args[1] {
	case "apply-answer":
		os.Exit(applyAnswer(os.Args[2:], os.Stdin, os.Stdout))
	case "conflict":
		os.Exit(runConflict(os.Args[2:], os.Stdout, os.Stderr))
	case "version":
		fmt.Println(sourceHash())
	default:
		fmt.Fprintf(os.Stderr, "daily_sync: unknown command %q\n%s", os.Args[1], usageText)
		os.Exit(64)
	}
}

// sidecar is what h-daily-sync-desk-ask wrote next to the question.
type sidecar struct {
	Machine string `json:"machine"`
	Repo    string `json:"repo"`
	Run     string `json:"run"`
	Items   []item `json:"items"`
	More    int    `json:"more"`
}

// loadSidecar reads the sidecar. A second run on the same day moves the
// finished run aside as <run>-done-<time>, sidecar included, so that is
// looked at too. It returns the path it read.
func loadSidecar(path string) (*sidecar, string, error) {
	b, err := os.ReadFile(path)
	if errors.Is(err, os.ErrNotExist) {
		dir, base := filepath.Split(path)
		m, _ := filepath.Glob(filepath.Join(filepath.Clean(dir)+"-done-*", base))
		if len(m) == 1 {
			path = m[0]
			b, err = os.ReadFile(path)
		}
	}
	if err != nil {
		return nil, path, err
	}
	var s sidecar
	if err := json.Unmarshal(b, &s); err != nil {
		return nil, path, err
	}
	if s.Machine == "" || s.Repo == "" || len(s.Items) == 0 {
		return nil, path, fmt.Errorf("%s: no machine, repository or questions", path)
	}
	for _, it := range s.Items {
		if len(it.Options) == 0 {
			return nil, path, fmt.Errorf("%s: a question without options", path)
		}
	}
	return &s, path, nil
}

// applier carries one apply-answer run.
type applier struct {
	sc       *sidecar
	run      string // the run directory: daily-sync logs land there
	deadline time.Time
	log      io.Writer
	details  []string // shown after the verdict
	zsh      string
}

func applyAnswer(args []string, stdin io.Reader, stdout io.Writer) int {
	start := time.Now()
	if len(args) != 1 {
		fmt.Fprint(stdout, "Not applied: daily_sync apply-answer needs the question's sidecar file; handed to the agent.\n"+usageText)
		return 64
	}
	path, _ := filepath.Abs(args[0])
	sc, path, err := loadSidecar(path)
	if err != nil {
		fmt.Fprintf(stdout, "Not applied: cannot read the question's record (%v); handed to the agent.\n", err)
		return 1
	}
	a := &applier{sc: sc, run: filepath.Dir(path), deadline: start.Add(budget), zsh: "zsh"}
	logPath := filepath.Join(a.run, fmt.Sprintf("answer-%s-%s.log", sc.Machine, strings.ReplaceAll(sc.Repo, "/", "_")))
	if f, err := os.OpenFile(logPath, os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0o600); err == nil {
		defer f.Close()
		a.log = f
	} else {
		a.log = io.Discard
		logPath = ""
	}
	fmt.Fprintf(a.log, "\n===== %s apply-answer %s\n", start.Format(time.RFC3339), path)
	answer, _ := io.ReadAll(stdin)
	fmt.Fprintf(a.log, "--- answer:\n%s\n", answer)

	verdict, rc := a.apply(string(answer))
	fmt.Fprintf(a.log, "--- verdict (exit %d): %s\n", rc, verdict)
	if logPath != "" {
		a.details = append(a.details, "Full log: "+logPath)
	}
	fmt.Fprint(stdout, report(verdict, a.details))
	return rc
}

// report puts the verdict first, and again last when the details are cut,
// since the desk keeps only the end of the output.
func report(verdict string, details []string) string {
	body := strings.Join(details, "\n")
	if len(verdict)+len(body) <= maxOutput {
		if body == "" {
			return verdict + "\n"
		}
		return verdict + "\n" + body + "\n"
	}
	keep := maxOutput - 2*len(verdict) - 10
	if keep < 0 {
		keep = 0
	}
	if keep < len(body) {
		body = body[:keep] + "\n..."
	}
	return verdict + "\n" + body + "\n" + verdict + "\n"
}

func (a *applier) where() string { return fmt.Sprintf("%s on %s", a.sc.Repo, a.sc.Machine) }

func (a *applier) notApplied(d *doubt) (string, int) {
	return "Not applied: " + d.msg + "; handed to the agent.", 1
}

// apply does the work and returns the verdict line and the exit status.
func (a *applier) apply(answer string) (string, int) {
	sc := a.sc
	if sc.More > 0 {
		return a.notApplied(doubtf("only the first %d of %d hunks were asked", len(sc.Items), len(sc.Items)+sc.More))
	}
	keys, d := parseAnswer(answer, sc.Items)
	if d != nil {
		return a.notApplied(d)
	}
	calls := resolveCalls(sc.Items, keys)
	var picks []string
	for _, c := range calls {
		picks = append(picks, c.File+" "+strings.Join(c.Specs, " "))
	}
	a.details = append(a.details, "Picks: "+strings.Join(picks, "; "))

	// A sync step or the metadata updater running now would race with us.
	out, _, err := a.zshRun(2*time.Minute, nil, "h-daily-sync-config && h-daily-sync-busy")
	switch {
	case err != nil || strings.Contains(out, "UNKNOWN") || !regexp.MustCompile(`(?m)^(IDLE|BUSY )`).MatchString(out):
		return a.notApplied(doubtf("could not tell whether a daily-sync job is running (%s)", firstLine(out, err)))
	case strings.Contains(out, "BUSY "):
		return a.notApplied(doubtf("a daily-sync job is running right now (%s)", strings.TrimSpace(out)))
	}

	unlock, err := a.lock(90 * time.Second)
	if err != nil {
		return a.notApplied(doubtf("another answer is being applied (%v)", err))
	}
	defer unlock()

	// Reopen the conflict, refusing one that is already open.
	out, _, err = a.daily(3*time.Minute, "conflict", sc.Machine, sc.Repo, "reopen")
	switch {
	case status(out, "ALREADY-OPEN ") != "":
		return a.notApplied(doubtf("a rebase or merge is already open in %s; someone may be working on it", a.where()))
	case status(out, "NO-CONFLICT ") != "":
		return a.notApplied(doubtf("the pull of %s goes through now, so there is nothing to resolve", a.where()))
	case status(out, "PULL-FAILED ") != "":
		return a.notApplied(doubtf("the pull of %s failed: %s", a.where(), status(out, "PULL-FAILED ")))
	case status(out, "MISSING ") != "":
		return a.notApplied(doubtf("%s does not exist on %s", sc.Repo, sc.Machine))
	case status(out, "CONFLICT-OPEN ") == "":
		return a.abortAnd(doubtf("could not reopen the conflict in %s (unreachable?): %s", a.where(), firstLine(out, err)))
	}
	if d := compareListing(sc.Items, parseListing(out)); d != nil {
		return a.abortAnd(d)
	}

	// Apply, one file at a time; the last one continues the rebase or merge.
	var resolved string
	for i, c := range calls {
		args := append([]string{"resolve", sc.Machine, sc.Repo, c.File}, c.Specs...)
		out, _, err = a.daily(3*time.Minute, args...)
		last := i == len(calls)-1
		bad := status(out, "ERROR", "CONTINUE-FAILED ", "NEXT-COMMIT-CONFLICTS ", "NOT-IN-PROGRESS ")
		if bad == "" {
			bad = lineWith(out, " still unresolved")
		}
		done := status(out, "RESOLVED ")
		switch {
		case bad != "":
			return a.abortAnd(doubtf("resolving %s said: %s", c.File, bad))
		case err != nil && done == "":
			return a.abortAnd(doubtf("resolving %s failed: %s", c.File, firstLine(out, err)))
		case done != "" && !last:
			// Should not happen: the listing named more files than were conflicted.
			a.details = append(a.details, done)
			return a.notApplied(doubtf("%s resolved before its last file %s was applied; the rest was not converged", a.where(), calls[len(calls)-1].File))
		case last && done == "":
			return a.abortAnd(doubtf("resolving the last file %s did not finish the rebase or merge: %s", c.File, firstLine(out, err)))
		case !last && status(out, "STILL-CONFLICTED ") == "":
			return a.abortAnd(doubtf("resolving %s gave an unexpected result: %s", c.File, firstLine(out, err)))
		}
		if last {
			resolved = done
		}
	}
	a.details = append(a.details, resolved)
	applied := fmt.Sprintf("Applied %d pick(s) to %s", len(sc.Items), a.where())

	// Converge through the slow-step job, waiting for it as long as the budget allows.
	wait := time.Until(a.deadline) - 45*time.Second
	if wait > 8*time.Minute {
		wait = 8 * time.Minute
	}
	if wait < 20*time.Second {
		wait = 20 * time.Second
	}
	env := []string{fmt.Sprintf("daily_sync_step_wait=%d", int(wait.Seconds()))}
	out, _, err = a.zshRun(wait+90*time.Second, env, `daily-sync converge`)
	switch {
	case status(out, "WORKING:", "The result of '") != "":
		return applied + "; converge is still running and reports to the sync agent.", 0
	case status(out, "STILL WORKING", "NOT ELEVATED", "The tmux server cannot be reached") != "":
		return applied + ", but converge did not finish here: " + firstLine(out, err) + "; handed to the agent.", 1
	case err != nil && !strings.Contains(out, "## "):
		return applied + ", but converge failed: " + firstLine(out, err) + "; handed to the agent.", 1
	}
	re := regexp.MustCompile(`(?m)^(CONFLICT|BLOCKED|PULL-FAILED|PUSH-FAILED) ` + regexp.QuoteMeta(sc.Repo) + `[ :].*$`)
	if left := re.FindAllString(out, -1); len(left) > 0 {
		a.details = append(a.details, "Converge still reports:")
		a.details = append(a.details, left...)
		return applied + ", but converge still reports a problem with it; handed to the agent.", 1
	}
	return applied + "; converged.", 0
}

// abortAnd closes the conflict apply-answer reopened, so the repository is
// left blocked as it was, and returns the doubt's verdict.
func (a *applier) abortAnd(d *doubt) (string, int) {
	ctx := time.Until(a.deadline) + 20*time.Second
	if ctx < 30*time.Second {
		ctx = 30 * time.Second
	}
	out, _, err := a.daily(ctx, "conflict", a.sc.Machine, a.sc.Repo, "abort-quiet")
	switch {
	case status(out, "ABORTED ") != "":
		a.details = append(a.details, fmt.Sprintf("The reopened conflict was aborted: %s stays blocked, as before.", a.where()))
	case status(out, "NOT-IN-PROGRESS ") != "":
		a.details = append(a.details, fmt.Sprintf("Nothing was left open: %s stays blocked, as before.", a.where()))
	default:
		a.details = append(a.details, fmt.Sprintf("WARNING: could not abort the reopened conflict (%s); it may still be open in %s.", firstLine(out, err), a.where()))
	}
	return a.notApplied(d)
}

// lock takes an exclusive lock shared by every run directory, since answers
// from different days touch the same repositories.
func (a *applier) lock(wait time.Duration) (func(), error) {
	path := filepath.Join(filepath.Dir(a.run), "apply-answer.lock")
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o600)
	if err != nil {
		return nil, err
	}
	until := time.Now().Add(wait)
	for {
		err = syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB)
		if err == nil {
			return func() { syscall.Flock(int(f.Fd()), syscall.LOCK_UN); f.Close() }, nil
		}
		if !errors.Is(err, syscall.EWOULDBLOCK) || time.Now().After(until) {
			f.Close()
			return nil, fmt.Errorf("lock %s: %v", path, err)
		}
		time.Sleep(time.Second)
	}
}

// daily runs `daily-sync <args...>' through zsh.
func (a *applier) daily(timeout time.Duration, args ...string) (string, string, error) {
	return a.zshRun(timeout, nil, `daily-sync "$@"`, args...)
}

// zshRun runs script under `zsh -c', in the run's directory for logs, with
// the arguments as $1..., and returns its stdout and stderr. The run's agent
// is the one recorded in the run, not whichever agent started the desk.
func (a *applier) zshRun(timeout time.Duration, env []string, script string, args ...string) (string, string, error) {
	if left := time.Until(a.deadline) + 25*time.Second; timeout > left {
		timeout = left
	}
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	cmd := exec.CommandContext(ctx, a.zsh, append([]string{"-c", script, "zsh"}, args...)...)
	cmd.Env = append(childEnv(os.Environ()), "daily_sync_run="+a.run)
	cmd.Env = append(cmd.Env, env...)
	cmd.WaitDelay = 5 * time.Second
	var out, errb bytes.Buffer
	cmd.Stdout, cmd.Stderr = &out, &errb
	t0 := time.Now()
	err := cmd.Run()
	if ctx.Err() != nil {
		err = fmt.Errorf("stopped after %s", timeout.Round(time.Second))
	}
	fmt.Fprintf(a.log, "--- zsh -c %q %q (%s, err=%v)\n%s", script, args, time.Since(t0).Round(time.Millisecond), err, out.String())
	if errb.Len() > 0 {
		fmt.Fprintf(a.log, "--- stderr:\n%s", errb.String())
	}
	return out.String(), errb.String(), err
}

// childEnv drops what would point the zsh commands elsewhere.
func childEnv(env []string) []string {
	var out []string
	for _, kv := range env {
		k, _, _ := strings.Cut(kv, "=")
		switch k {
		case "PASEO_AGENT_ID", "daily_sync_run", "daily_sync_inner", "daily_sync_step_wait":
			continue
		}
		out = append(out, kv)
	}
	return out
}

// firstLine is the first non-empty line of out, or the error.
func firstLine(out string, err error) string {
	for _, l := range strings.Split(out, "\n") {
		if l = strings.TrimSpace(l); l != "" {
			return l
		}
	}
	if err != nil {
		return err.Error()
	}
	return "no output"
}

// status is the first line of out that starts with one of the prefixes:
// daily-sync's status words start a line, while hunk text is indented.
func status(out string, prefixes ...string) string {
	for _, l := range strings.Split(out, "\n") {
		for _, p := range prefixes {
			if strings.HasPrefix(l, p) {
				return strings.TrimSpace(l)
			}
		}
	}
	return ""
}

// lineWith is the first line of out containing w, or "".
func lineWith(out, w string) string {
	for _, l := range strings.Split(out, "\n") {
		if strings.Contains(l, w) {
			return strings.TrimSpace(l)
		}
	}
	return ""
}
