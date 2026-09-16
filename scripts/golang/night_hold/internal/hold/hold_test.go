package hold

import (
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"
)

func testStore(t *testing.T) Store {
	t.Helper()
	// These tests run inside an agent, which exports its own pid. Left set,
	// every hold a test writes carries *this* process's pid and so is "mine"
	// to every other test, and impersonating a second session becomes
	// impossible. Liveness tests opt back in explicitly.
	t.Setenv("CLAUDE_PID", "")
	t.Setenv("hold_agent_pid", "")
	return Store{Root: t.TempDir()}
}

func mustAcquire(t *testing.T, s Store, o AcquireOpts) Hold {
	t.Helper()
	h, err := s.Acquire(o)
	if err != nil {
		t.Fatalf("acquire %+v: %v", o, err)
	}
	return h
}

func TestCanonicalCollapsesSpellings(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)
	target := filepath.Join(home, "scripts")
	if err := os.MkdirAll(target, 0o755); err != nil {
		t.Fatal(err)
	}

	want := Canonical("repo:" + target)
	for _, spelling := range []string{
		"repo:~/scripts",
		"repo:" + target,
		"repo:" + target + "/",
		"repo:" + filepath.Join(target, "sub", ".."),
		"repo:$HOME/scripts",
	} {
		if got := Canonical(spelling); got != want {
			t.Errorf("Canonical(%q) = %q, want %q", spelling, got, want)
		}
	}
}

func TestCanonicalLeavesNonPathsAlone(t *testing.T) {
	for _, r := range []string{"gpu:0", "service:garden", "plain"} {
		if got := Canonical(r); got != r {
			t.Errorf("Canonical(%q) = %q, want it untouched", r, got)
		}
	}
	if p := PathPart("gpu:0"); p != "" {
		t.Errorf("PathPart(gpu:0) = %q, want empty: 0 is not a filesystem extent", p)
	}
}

func TestExclusiveRefusesSecondHolder(t *testing.T) {
	s := testStore(t)
	mustAcquire(t, s, AcquireOpts{Resource: "repo:/tmp/x", Holder: "a", Reason: "first"})

	_, err := s.Acquire(AcquireOpts{Resource: "repo:/tmp/x", Holder: "b"})
	var held ErrHeld
	if !asErrHeld(err, &held) {
		t.Fatalf("second holder got %v, want ErrHeld", err)
	}
	if held.By.Holder != "a" || held.By.Reason != "first" {
		t.Errorf("ErrHeld names %q/%q, want a/first", held.By.Holder, held.By.Reason)
	}
}

func TestExclusiveReacquireByOwnerRenews(t *testing.T) {
	s := testStore(t)
	now := time.Now()
	first := mustAcquire(t, s, AcquireOpts{Resource: "gpu:0", Holder: "a", TTL: time.Minute, Now: now})
	second := mustAcquire(t, s, AcquireOpts{Resource: "gpu:0", Holder: "a", TTL: time.Hour, Now: now})

	if !second.Until.After(first.Until) {
		t.Error("re-acquiring my own hold should push the deadline out")
	}
	if !second.Acquired.Equal(first.Acquired) {
		t.Error("a renewal should keep the original start time")
	}
}

func TestSharedWelcomesOtherHolders(t *testing.T) {
	s := testStore(t)
	mustAcquire(t, s, AcquireOpts{Resource: "service:hs-reload", Holder: "a", Shared: true})
	mustAcquire(t, s, AcquireOpts{Resource: "service:hs-reload", Holder: "b", Shared: true})

	live, err := s.Live(Canonical("service:hs-reload"), time.Now(), false)
	if err != nil {
		t.Fatal(err)
	}
	if len(live) != 2 {
		t.Fatalf("got %d holders, want 2: a shared hold is what several agents suppressing one reloader needs", len(live))
	}
}

func TestModesMayNotBeMixed(t *testing.T) {
	s := testStore(t)
	mustAcquire(t, s, AcquireOpts{Resource: "service:x", Holder: "a", Shared: true})

	if _, err := s.Acquire(AcquireOpts{Resource: "service:x", Holder: "b"}); err == nil {
		t.Error("an exclusive acquire over a shared hold should be refused, not guessed at")
	}
}

func TestExpiredHoldIsIgnoredAndReaped(t *testing.T) {
	s := testStore(t)
	past := time.Now().Add(-2 * time.Hour)
	h := mustAcquire(t, s, AcquireOpts{Resource: "repo:/tmp/x", Holder: "dead", TTL: time.Minute, Now: past})

	if _, err := s.Acquire(AcquireOpts{Resource: "repo:/tmp/x", Holder: "live"}); err != nil {
		t.Fatalf("an expired hold must not block: %v", err)
	}
	if _, err := os.Stat(h.file); !os.IsNotExist(err) {
		t.Error("the expired holder file should have been reaped")
	}
}

func TestCorruptHoldIsNotAnEternalHold(t *testing.T) {
	s := testStore(t)
	dir := s.dirFor(Canonical("repo:/tmp/x"))
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "garbage"), []byte("nonsense\n"), 0o644); err != nil {
		t.Fatal(err)
	}

	if _, err := s.Acquire(AcquireOpts{Resource: "repo:/tmp/x", Holder: "live"}); err != nil {
		t.Fatalf("a file with no readable deadline is a corpse, not a hold: %v", err)
	}
}

func TestReleaseOfSomeoneElsesIsRefused(t *testing.T) {
	s := testStore(t)
	mustAcquire(t, s, AcquireOpts{Resource: "repo:/tmp/x", Holder: "a"})

	if _, err := s.Release("repo:/tmp/x", "b", time.Time{}); err == nil {
		t.Error("releasing another session's hold would open the resource under whoever is still working")
	}
	if _, err := s.Release("repo:/tmp/x", "a", time.Time{}); err != nil {
		t.Errorf("the holder must be able to release: %v", err)
	}
}

func TestRenewCarriesMatchesAndDoesNotDuplicatePathMatches(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)
	s := testStore(t)
	target := filepath.Join(home, "scripts")

	mustAcquire(t, s, AcquireOpts{
		Resource: "repo:" + target, Holder: "a", Reason: "rewrite",
		Matches: []string{"vcsh night.sh"},
	})
	for i := 0; i < 3; i++ {
		if _, err := s.Renew("repo:"+target, "a", time.Hour, time.Time{}); err != nil {
			t.Fatal(err)
		}
	}

	live, _ := s.Live(Canonical("repo:"+target), time.Now(), false)
	if len(live) != 1 {
		t.Fatalf("got %d holds, want 1", len(live))
	}
	h := live[0]
	if h.Reason != "rewrite" {
		t.Errorf("renew dropped the reason: %q", h.Reason)
	}
	if len(h.Matches) != 1 || h.Matches[0] != "vcsh night.sh" {
		t.Errorf("renew must not narrow what is protected: %v", h.Matches)
	}
	if len(h.PathMatches) != 2 {
		t.Errorf("path matches accumulated across renewals: %v", h.PathMatches)
	}
}

func TestCheckSemantics(t *testing.T) {
	s := testStore(t)
	mustAcquire(t, s, AcquireOpts{Resource: "repo:/tmp/x", Holder: "a"})

	if ok, _ := s.Check("repo:/tmp/x", "a", false, time.Time{}); !ok {
		t.Error("my own hold should not block me")
	}
	if ok, _ := s.Check("repo:/tmp/x", "b", false, time.Time{}); ok {
		t.Error("another session's exclusive hold should block me")
	}
	if ok, _ := s.Check("repo:/tmp/free", "b", false, time.Time{}); !ok {
		t.Error("a free resource should not block me")
	}
}

// Two processes both reading "free" and both writing is the bug flock exists to
// stop. Goroutines are a weaker test than processes -- they share the lock file
// descriptor table -- but they still catch a missing critical section.
func TestConcurrentAcquireProducesOneHolder(t *testing.T) {
	s := testStore(t)
	const n = 16

	var wg sync.WaitGroup
	won := make([]bool, n)
	start := make(chan struct{})
	for i := 0; i < n; i++ {
		wg.Add(1)
		go func(i int) {
			defer wg.Done()
			<-start
			_, err := s.Acquire(AcquireOpts{
				Resource: "repo:/tmp/race",
				Holder:   string(rune('a' + i)),
			})
			won[i] = err == nil
		}(i)
	}
	close(start)
	wg.Wait()

	count := 0
	for _, w := range won {
		if w {
			count++
		}
	}
	if count != 1 {
		t.Errorf("%d of %d concurrent acquires succeeded; an exclusive hold must have exactly one holder", count, n)
	}
}

func TestDeadlineIsAlsoInTheMtimeForLua(t *testing.T) {
	s := testStore(t)
	h := mustAcquire(t, s, AcquireOpts{Resource: "service:hs-reload", Holder: "a", TTL: time.Hour, Shared: true})

	fi, err := os.Stat(h.file)
	if err != nil {
		t.Fatal(err)
	}
	// hammerspoonReloadHeldBy() answers by statting, never by parsing.
	if !fi.ModTime().Equal(h.Until.Truncate(time.Second)) && fi.ModTime().Unix() != h.Until.Unix() {
		t.Errorf("mtime %v does not carry the deadline %v", fi.ModTime(), h.Until)
	}
	if !fi.ModTime().After(time.Now()) {
		t.Error("a live hold's mtime must be in the future: that is the whole test Lua does")
	}
}

func TestParseDuration(t *testing.T) {
	cases := map[string]time.Duration{
		"90s": 90 * time.Second,
		"30m": 30 * time.Minute,
		"2h":  2 * time.Hour,
		"3d":  72 * time.Hour,
		"45":  45 * time.Second,
	}
	for in, want := range cases {
		got, err := ParseDuration(in)
		if err != nil || got != want {
			t.Errorf("ParseDuration(%q) = %v, %v; want %v", in, got, err, want)
		}
	}
	if _, err := ParseDuration("banana"); err == nil {
		t.Error("ParseDuration should reject nonsense")
	}
}

func TestStatusIsQuietWhenNothingIsHeld(t *testing.T) {
	s := testStore(t)
	all, err := s.All(time.Now(), true)
	if err != nil || len(all) != 0 {
		t.Errorf("All on an empty store = %v, %v", all, err)
	}
}

func TestSlugCannotEscapeTheStore(t *testing.T) {
	for _, r := range []string{"repo:/a/../../etc", "x/../../y", "../../z"} {
		if strings.Contains(Slug(r), "/") {
			t.Errorf("Slug(%q) = %q contains a separator", r, Slug(r))
		}
	}
}

func asErrHeld(err error, target *ErrHeld) bool {
	if e, ok := err.(ErrHeld); ok {
		*target = e
		return true
	}
	return false
}

// A best-effort caller must never wait on the resource lock. The guard calls
// Acquire before every tool call to refresh its own deadline, so a process
// wedged mid-acquire would otherwise hang the whole session.
func TestBestEffortAcquireSkipsRatherThanBlocking(t *testing.T) {
	s := testStore(t)
	dir := s.dirFor(Canonical("gpu:0"))
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}

	held := make(chan struct{})
	release := make(chan struct{})
	go func() {
		_ = withResourceLock(dir, true, func() error {
			close(held)
			<-release
			return nil
		})
	}()
	<-held
	defer close(release)

	done := make(chan error, 1)
	go func() {
		_, err := s.Acquire(AcquireOpts{Resource: "gpu:0", Holder: "a", BestEffort: true})
		done <- err
	}()

	select {
	case err := <-done:
		if err == nil {
			t.Error("a best-effort acquire should report that it skipped, not claim success")
		}
	case <-time.After(3 * time.Second):
		t.Fatal("a best-effort acquire blocked on the resource lock")
	}
}

// An ordinary acquire still waits: correctness there matters more than latency.
func TestOrdinaryAcquireStillWaitsForTheLock(t *testing.T) {
	s := testStore(t)
	dir := s.dirFor(Canonical("gpu:1"))
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}

	held := make(chan struct{})
	release := make(chan struct{})
	go func() {
		_ = withResourceLock(dir, true, func() error {
			close(held)
			<-release
			return nil
		})
	}()
	<-held

	done := make(chan error, 1)
	go func() {
		_, err := s.Acquire(AcquireOpts{Resource: "gpu:1", Holder: "a"})
		done <- err
	}()

	select {
	case <-done:
		t.Error("an ordinary acquire returned while the lock was held elsewhere")
	case <-time.After(200 * time.Millisecond):
	}

	close(release)
	select {
	case err := <-done:
		if err != nil {
			t.Errorf("acquire failed once the lock was free: %v", err)
		}
	case <-time.After(3 * time.Second):
		t.Fatal("acquire never completed after the lock was released")
	}
}

// An until-live hold has no deadline at all: it ends when its holder releases
// it or dies, and no amount of elapsed time touches it. This is the default,
// so it is the case that matters most.
func TestUntilLiveHoldOutlivesAnyDeadline(t *testing.T) {
	s := testStore(t)
	// testStore clears these; until-live needs an agent pid to be meaningful,
	// so opt back in with a pid that is certainly alive.
	t.Setenv("hold_agent_pid", strconv.Itoa(os.Getpid()))

	h := mustAcquire(t, s, AcquireOpts{Resource: "repo:/tmp/x", Holder: "agent", Reason: "refactor"})
	if h.HasDeadline() {
		t.Fatalf("default acquire got a deadline of %v, want until-live", h.Until)
	}
	if h.TTLFallback != "" {
		t.Errorf("ttl-fallback = %q, want none with a real agent pid", h.TTLFallback)
	}

	// Round-trips through the file, rather than only living in the returned
	// struct: `until: 0` must read back as no deadline, not as 1970.
	live, err := s.Live(Canonical("repo:/tmp/x"), time.Now(), false)
	if err != nil || len(live) != 1 {
		t.Fatalf("Live = %v, %v; want one hold", live, err)
	}
	if live[0].HasDeadline() {
		t.Errorf("re-read hold has deadline %v, want until-live", live[0].Until)
	}

	// A year on, still held.
	far := time.Now().Add(365 * 24 * time.Hour)
	live, err = s.Live(Canonical("repo:/tmp/x"), far, true)
	if err != nil || len(live) != 1 {
		t.Fatalf("a year later Live = %v, %v; want the hold to stand", live, err)
	}
	if _, err := s.Acquire(AcquireOpts{Resource: "repo:/tmp/x", Holder: "other", Now: far}); err == nil {
		t.Error("an until-live hold must still block a year later")
	}
}

// Without an agent pid nothing can ever declare the holder dead, so until-live
// would be a lock that expires by no mechanism at all. A deadline comes back,
// and the hold records why so status can explain itself.
func TestWithoutAnAgentPidUntilLiveBecomesADeadline(t *testing.T) {
	s := testStore(t) // clears the agent pid; the caller is a bare shell

	h := mustAcquire(t, s, AcquireOpts{Resource: "repo:/tmp/x", Holder: "shell", Reason: "by hand"})
	if !h.HasDeadline() {
		t.Fatal("a shell-pid hold must get a deadline; until-live cannot work for it")
	}
	if h.TTL != DefaultTTL {
		t.Errorf("ttl = %v, want the default %v", h.TTL, DefaultTTL)
	}
	if h.TTLFallback != "no-agent-pid" {
		t.Errorf("ttl-fallback = %q, want no-agent-pid", h.TTLFallback)
	}
	if !strings.Contains(h.Window(time.Now()), "no agent pid") {
		t.Errorf("status line %q does not explain the imposed deadline", h.Window(time.Now()))
	}
}

// An explicit --ttl is now a hard deadline: nothing renews it behind the
// caller's back, which is the whole reason the keepalive went.
func TestExplicitTTLExpiresUnextended(t *testing.T) {
	s := testStore(t)
	t.Setenv("hold_agent_pid", strconv.Itoa(os.Getpid()))

	now := time.Now()
	h := mustAcquire(t, s, AcquireOpts{
		Resource: "repo:/tmp/x", Holder: "agent", TTL: 10 * time.Minute, Now: now,
	})
	if !h.HasDeadline() {
		t.Fatal("an explicit ttl must produce a deadline")
	}

	// Still alive as a process, and still held before the deadline.
	if live, _ := s.Live(Canonical("repo:/tmp/x"), now.Add(9*time.Minute), false); len(live) != 1 {
		t.Error("the hold should stand before its deadline")
	}
	// Past it, gone -- liveness does not keep a dated hold alive.
	if live, _ := s.Live(Canonical("repo:/tmp/x"), now.Add(11*time.Minute), true); len(live) != 0 {
		t.Error("an explicit ttl must expire even though its holder is alive")
	}
}
