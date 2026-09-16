package hold

import (
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"
)

// A killed agent must free its hold at once. Waiting out a 30 minute deadline
// for a process that no longer exists is the thing liveness is for.
func TestDeadAgentIsReapedBeforeItsDeadline(t *testing.T) {
	cmd := exec.Command("sh", "-c", "while :; do :; done")
	if err := cmd.Start(); err != nil {
		t.Skipf("cannot start a helper process: %v", err)
	}
	pid := cmd.Process.Pid
	s := testStore(t)
	// After testStore, which clears these so the rest of the suite is hermetic.
	t.Setenv("hold_agent_pid", strconv.Itoa(pid))
	h := mustAcquire(t, s, AcquireOpts{
		Resource: "repo:/tmp/x", Holder: "agent", TTL: time.Hour, Reason: "long job",
	})
	if h.PIDKind != pidAgent {
		t.Fatalf("pid-kind = %q, want %q", h.PIDKind, pidAgent)
	}

	// Still alive: the hold stands.
	if _, err := s.Acquire(AcquireOpts{Resource: "repo:/tmp/x", Holder: "other"}); err == nil {
		t.Error("a live agent's hold must still block")
	}

	_ = cmd.Process.Kill()
	_, _ = cmd.Process.Wait()

	if _, err := s.Acquire(AcquireOpts{Resource: "repo:/tmp/x", Holder: "other"}); err != nil {
		t.Errorf("a dead agent's hold should be reaped an hour before its deadline: %v", err)
	}
}

// The pid of the shell that ran hold-acquire is worthless -- it exits a
// millisecond later, and trusting it would reap every hold the instant it was
// taken. Only a pid we were explicitly told is long-lived may be trusted.
func TestShellPIDIsNeverTrustedForLiveness(t *testing.T) {
	// These tests run inside an agent, which exports its own pid. Without
	// clearing them the suite measures the developer's environment rather than
	// the code.
	t.Setenv("hold_agent_pid", "")
	t.Setenv("CLAUDE_PID", "")

	s := testStore(t)
	h := mustAcquire(t, s, AcquireOpts{Resource: "gpu:0", Holder: "a", TTL: time.Hour})

	if h.PIDKind != pidShell {
		t.Fatalf("pid-kind = %q, want %q when no agent pid is exported", h.PIDKind, pidShell)
	}
	h.PID = 999999 // almost certainly gone
	if h.Dead(thisHost()) {
		t.Error("a shell pid must never declare a hold dead")
	}
}

func TestHoldFromAnotherHostIsNeverDeclaredDead(t *testing.T) {
	h := Hold{PIDKind: pidAgent, PID: 999999, Host: "some-other-machine"}
	if h.Dead("this-machine") {
		t.Error("a pid means nothing on another host, and $HOME can be shared between them")
	}
}

// An agent whose next step is the held resource has nothing better to do than
// wait for it.
func TestAcquireWaitSucceedsOnceTheHoldIsReleased(t *testing.T) {
	s := testStore(t)
	mustAcquire(t, s, AcquireOpts{Resource: "gpu:0", Holder: "a", TTL: time.Hour})

	go func() {
		time.Sleep(150 * time.Millisecond)
		_, _ = s.Release("gpu:0", "a", time.Time{})
	}()

	blocked := 0
	h, err := s.AcquireWait(
		AcquireOpts{Resource: "gpu:0", Holder: "b", TTL: time.Hour},
		5*time.Second, 50*time.Millisecond,
		func(Hold) { blocked++ },
	)
	if err != nil {
		t.Fatalf("wait should have got it once released: %v", err)
	}
	if h.Holder != "b" {
		t.Errorf("holder = %q, want b", h.Holder)
	}
	if blocked != 1 {
		t.Errorf("the blocked notice fired %d times, want exactly 1", blocked)
	}
}

func TestAcquireWaitGivesUpAtTheBudget(t *testing.T) {
	s := testStore(t)
	mustAcquire(t, s, AcquireOpts{Resource: "gpu:0", Holder: "a", TTL: time.Hour})

	start := time.Now()
	_, err := s.AcquireWait(
		AcquireOpts{Resource: "gpu:0", Holder: "b"},
		200*time.Millisecond, 50*time.Millisecond, nil,
	)
	if err == nil {
		t.Fatal("waiting past the budget must fail, not block forever")
	}
	if elapsed := time.Since(start); elapsed > 2*time.Second {
		t.Errorf("waited %v, well past the budget", elapsed)
	}
}

// A session id can change mid-task -- moving the session into `claude agents'
// does it, and so does a resume -- while the process, the working directory and
// the intent stay the same. Without the pid identity the agent is denied its
// own repository by its own hold, and cannot release it either, which is
// exactly what happened to the session that wrote this.
func TestANewSessionIDDoesNotLockAnAgentOutOfItsOwnHold(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)
	t.Setenv("hold_agent_pid", strconv.Itoa(os.Getpid()))
	s := Store{Root: filepath.Join(home, ".night-holds")}
	held := filepath.Join(home, "repo")

	if _, err := s.Acquire(AcquireOpts{
		Resource: "repo:" + held, Holder: "session-before", TTL: time.Hour,
	}); err != nil {
		t.Fatal(err)
	}

	// The guard, now told a different session id for the same process.
	d := s.Guard(strings.NewReader(
		payload("session-after", "Edit", "/x", held+"/x.zsh", "")), time.Now())
	if d.Deny {
		t.Errorf("the agent was denied its own held repository after a new session id: %s", d.Reason)
	}

	// And it can still release, without being told to pass --holder.
	if _, err := s.Release("repo:"+held, "", time.Time{}); err != nil {
		t.Errorf("release after a new session id: %v", err)
	}
}

// A genuinely different agent is a genuinely different pid, so the pid identity
// must not hand it someone else's hold.
func TestPIDIdentityDoesNotLeakBetweenAgents(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)
	// Real, live pids on both sides: an unused pid would be reaped as dead
	// before the identity check ever ran.
	t.Setenv("hold_agent_pid", strconv.Itoa(os.Getpid()))
	s := Store{Root: filepath.Join(home, ".night-holds")}
	held := filepath.Join(home, "repo")

	if _, err := s.Acquire(AcquireOpts{
		Resource: "repo:" + held, Holder: "agent-one", TTL: time.Hour, Reason: "mine",
	}); err != nil {
		t.Fatal(err)
	}

	t.Setenv("hold_agent_pid", strconv.Itoa(os.Getppid()))
	d := s.Guard(strings.NewReader(
		payload("agent-two", "Edit", "/x", held+"/x.zsh", "")), time.Now())
	if !d.Deny {
		t.Error("another agent's hold must still block, pid identity or not")
	}
	if _, err := s.Release("repo:"+held, "", time.Time{}); err == nil {
		t.Error("another agent must not be able to release it")
	}
}
