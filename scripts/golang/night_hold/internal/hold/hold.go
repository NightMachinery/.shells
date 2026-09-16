// Package hold implements self-expiring advisory holds over a named resource.
//
// State is a directory per resource and a file per holder, so one mechanism
// carries both semantics: an exclusive hold refuses a second holder, a shared
// hold welcomes one. See scripts/docs/holds.md.
//
// Stdlib only, like agent_session, so `go build` needs no network.
package hold

import (
	"errors"
	"fmt"
	"os"
	"os/user"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

const (
	ModeExclusive = "exclusive"
	ModeShared    = "shared"
)

// DefaultTTL matches the agent banner's and the Hammerspoon holds', for the
// same reason: long enough to be useful, short enough that forgetting is not a
// lasting problem.
const DefaultTTL = 30 * time.Minute

// Hold is one holder's claim on one resource.
type Hold struct {
	Resource    string
	Holder      string
	Mode        string
	Until       time.Time
	Acquired    time.Time
	PID         int
	PIDKind     string
	Host        string
	TTL         time.Duration
	Reason      string
	Matches     []string // tested as plain substrings; the caller asked for this text
	PathMatches []string // tested only where a path boundary sits on each side

	file string
}

// Left is how long this hold has to run.
func (h Hold) Left(now time.Time) time.Duration { return h.Until.Sub(now) }

// Store is a hold directory.
type Store struct{ Root string }

// New returns the store at $hold_dir, defaulting to ~/.night-holds.
//
// Under $HOME rather than $TMPDIR: every reader has to agree on the path, and
// the readers are a login shell, a BrishGarden shell, a Hammerspoon Lua reader
// and this binary. $TMPDIR is per-context on macOS and unset in some of them,
// which would split the state silently.
func New() Store {
	if d := os.Getenv("hold_dir"); d != "" {
		return Store{Root: d}
	}
	home, err := os.UserHomeDir()
	if err != nil {
		home = os.Getenv("HOME")
	}
	return Store{Root: filepath.Join(home, ".night-holds")}
}

var pathKinds = map[string]bool{"repo": true, "path": true, "dir": true, "file": true}

// Canonical is the spelling every reader must agree on.
//
// `repo:~/scripts` and `repo:/Users/evar/scripts` are the same resource, and a
// guard that did not know that would silently protect nothing. Non-path kinds
// pass through untouched, so `gpu:0` means whatever its callers agree it means.
//
// Dynamic named directories (`~[nt]/x`) are not handled here: path-unabbrev
// resolves those through aliasdir, which is zsh-only, so the zsh wrapper
// resolves them and passes a real path down.
func Canonical(resource string) string {
	kind, rest, found := strings.Cut(resource, ":")
	if !found || !pathKinds[kind] {
		return resource
	}

	if rest == "~" {
		rest = homeDir()
	} else if strings.HasPrefix(rest, "~/") {
		rest = filepath.Join(homeDir(), rest[2:])
	} else if strings.HasPrefix(rest, "$HOME/") {
		rest = filepath.Join(homeDir(), rest[6:])
	}

	if abs, err := filepath.Abs(rest); err == nil {
		rest = abs
	}
	// Resolve symlinks when the path exists, so two spellings of one directory
	// do not become two resources. A path that does not exist yet is still a
	// legal thing to hold.
	if resolved, err := filepath.EvalSymlinks(rest); err == nil {
		rest = resolved
	}
	rest = filepath.Clean(rest)
	if rest != "/" {
		rest = strings.TrimSuffix(rest, "/")
	}
	return kind + ":" + rest
}

func homeDir() string {
	if h, err := os.UserHomeDir(); err == nil {
		return h
	}
	if h := os.Getenv("HOME"); h != "" {
		return h
	}
	if u, err := user.Current(); err == nil {
		return u.HomeDir
	}
	return ""
}

// PathPart is the filesystem extent of a resource, or "" when it has none.
// `gpu:0` has none: its path part would be the string "0", and testing tool
// paths against that would be nonsense rather than protection.
func PathPart(canonical string) string {
	kind, rest, found := strings.Cut(canonical, ":")
	if !found || !pathKinds[kind] {
		return ""
	}
	return rest
}

// Slug is a filename that cannot escape the store. Lossy, so two resources can
// in principle collide; Acquire catches that by comparing the resource line it
// finds against its own rather than trusting the name.
func Slug(canonical string) string {
	var b strings.Builder
	for _, r := range canonical {
		switch {
		case r >= 'a' && r <= 'z', r >= 'A' && r <= 'Z', r >= '0' && r <= '9',
			r == '_', r == '.', r == '@', r == '-':
			b.WriteRune(r)
		default:
			b.WriteByte('-')
		}
	}
	return b.String()
}

// Holder names whoever is asking: stable across calls, distinct between
// concurrent sessions.
//
// The agent ids are per *session*, so a compaction or a resume changes the
// answer mid-task and a hold becomes unreleasable by name. The deadline is the
// real backstop; $hold_holder overrides for the rare caller that has to release
// someone else's.
func Holder() string {
	for _, env := range []string{
		"hold_holder",
		"CLAUDE_CODE_SESSION_ID",
		"CODEX_THREAD_ID",
		"ANTIGRAVITY_CONVERSATION_ID",
		"TERM_SESSION_ID",
	} {
		if v := os.Getenv(env); v != "" {
			return sanitizeHolder(v)
		}
	}
	host, _ := os.Hostname()
	return sanitizeHolder(fmt.Sprintf("%d@%s", os.Getpid(), host))
}

func sanitizeHolder(id string) string {
	var b strings.Builder
	for _, r := range id {
		switch {
		case r >= 'a' && r <= 'z', r >= 'A' && r <= 'Z', r >= '0' && r <= '9',
			r == '_', r == '.', r == '@', r == '-':
			b.WriteRune(r)
		default:
			b.WriteByte('-')
		}
	}
	if b.Len() == 0 {
		return "-"
	}
	return b.String()
}

func (s Store) dirFor(canonical string) string {
	return filepath.Join(s.Root, Slug(canonical))
}

// Live returns the unexpired holds on one resource, reaping any it finds
// expired. Reaping is the writing side's job; Guard never deletes.
func (s Store) Live(canonical string, now time.Time, reap bool) ([]Hold, error) {
	dir := s.dirFor(canonical)
	entries, err := os.ReadDir(dir)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		}
		return nil, err
	}

	host := thisHost()
	var live []Hold
	for _, e := range entries {
		// Dotfiles are ours, not holders': `.lock` in particular is an empty
		// file that would fail to parse and be reaped -- deleting the very
		// inode withResourceLock is holding, so the next process would create a
		// fresh one and get no mutual exclusion at all.
		if e.IsDir() || strings.HasPrefix(e.Name(), ".") {
			continue
		}
		p := filepath.Join(dir, e.Name())
		h, err := readHold(p)
		// A file with no readable deadline is a corpse from a half-written
		// acquire, not an eternal hold. A hold whose agent is provably gone is
		// a corpse too, and waiting out its deadline helps nobody.
		if err != nil || !h.Until.After(now) || h.Dead(host) {
			if reap {
				os.Remove(p)
			}
			continue
		}
		live = append(live, h)
	}
	sort.Slice(live, func(i, j int) bool { return live[i].Holder < live[j].Holder })
	// The resource directory is never removed. It costs nothing to leave, and
	// removing it would race with another process that is about to flock the
	// `.lock` inside it.
	return live, nil
}

// All returns every live hold in the store.
func (s Store) All(now time.Time, reap bool) ([]Hold, error) {
	entries, err := os.ReadDir(s.Root)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		}
		return nil, err
	}

	host := thisHost()
	var out []Hold
	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		dir := filepath.Join(s.Root, e.Name())
		files, err := os.ReadDir(dir)
		if err != nil {
			continue
		}
		for _, f := range files {
			if f.IsDir() || strings.HasPrefix(f.Name(), ".") {
				continue
			}
			p := filepath.Join(dir, f.Name())
			h, err := readHold(p)
			if err != nil || !h.Until.After(now) || h.Dead(host) {
				if reap {
					os.Remove(p)
				}
				continue
			}
			out = append(out, h)
		}
	}
	sort.Slice(out, func(i, j int) bool {
		if out[i].Resource != out[j].Resource {
			return out[i].Resource < out[j].Resource
		}
		return out[i].Holder < out[j].Holder
	})
	return out, nil
}

// AcquireOpts are the knobs of Acquire.
type AcquireOpts struct {
	Resource string
	Holder   string
	TTL      time.Duration
	Reason   string
	Matches  []string
	Shared   bool
	Now      time.Time
}

// ErrHeld is returned when an exclusive resource is already held by someone
// else. It carries the blocking hold so the caller can say who and for how long.
type ErrHeld struct{ By Hold }

func (e ErrHeld) Error() string {
	return fmt.Sprintf("%s is held by %s for another %s: %s",
		e.By.Resource, e.By.Holder, FormatDuration(e.By.Left(time.Now())), e.By.Reason)
}

// AcquireWait retries Acquire until it succeeds or the budget runs out.
//
// Failing fast is right for an agent that has something else to do, but an
// agent whose next step *is* the held resource has nothing better than to
// wait. Polling rather than waiting on a notification: the state is files, the
// contention is measured in minutes, and a poll loop cannot deadlock or miss a
// wakeup.
func (s Store) AcquireWait(o AcquireOpts, budget, interval time.Duration, onBlock func(Hold)) (Hold, error) {
	if interval <= 0 {
		interval = 2 * time.Second
	}
	deadline := time.Now().Add(budget)
	announced := false

	for {
		h, err := s.Acquire(o)
		if err == nil {
			return h, nil
		}
		var held ErrHeld
		if !errors.As(err, &held) {
			return Hold{}, err // a real problem, not contention
		}
		if !announced && onBlock != nil {
			onBlock(held.By)
			announced = true
		}
		if !time.Now().Add(interval).Before(deadline) {
			return Hold{}, err
		}
		time.Sleep(interval)
	}
}

// Acquire takes or renews a hold. Re-acquiring your own live hold renews it
// rather than failing, so a long job can call this again without tracking
// whether it already holds one.
func (s Store) Acquire(o AcquireOpts) (Hold, error) {
	now := o.Now
	if now.IsZero() {
		now = time.Now()
	}
	// Second precision throughout, because that is all the file carries: without
	// this the Hold returned by Acquire is not equal to the one read back, and
	// every comparison against stored state is subtly off.
	now = now.Truncate(time.Second)
	if o.TTL <= 0 {
		o.TTL = DefaultTTL
	}
	if o.Holder == "" {
		o.Holder = Holder()
	}
	if o.Reason == "" {
		o.Reason = "unspecified"
	}

	canonical := Canonical(o.Resource)
	mode := ModeExclusive
	if o.Shared {
		mode = ModeShared
	}

	var h Hold
	err := withResourceLock(s.dirFor(canonical), func() error {
		var err error
		h, err = s.acquireLocked(canonical, mode, o, now)
		return err
	})
	return h, err
}

// acquireLocked is the critical section: everything between reading who holds
// the resource and writing that we do.
func (s Store) acquireLocked(canonical, mode string, o AcquireOpts, now time.Time) (Hold, error) {
	live, err := s.Live(canonical, now, true)
	if err != nil {
		return Hold{}, err
	}

	for _, h := range live {
		// Two different resources that slugged to the same name. Vanishingly
		// rare, but refusing is the conservative answer and beats letting the
		// caller think it holds something.
		if h.Resource != canonical {
			return Hold{}, fmt.Errorf("name collision: %s and %s share a state directory", canonical, h.Resource)
		}
		if h.Mode != mode {
			return Hold{}, fmt.Errorf("%s is already held as %s; refusing to also hold it as %s", canonical, h.Mode, mode)
		}
		if mode == ModeExclusive && h.Holder != o.Holder {
			return Hold{}, ErrHeld{By: h}
		}
	}

	acquired := now
	for _, h := range live {
		if h.Holder == o.Holder {
			acquired = h.Acquired // a renewal keeps the original start
		}
	}

	pid, pidKind := agentPID()
	h := Hold{
		Resource:    canonical,
		Holder:      o.Holder,
		Mode:        mode,
		Until:       now.Add(o.TTL),
		Acquired:    acquired,
		PID:         pid,
		PIDKind:     pidKind,
		Host:        thisHost(),
		TTL:         o.TTL,
		Reason:      o.Reason,
		Matches:     dedupe(o.Matches),
		PathMatches: derivePathMatches(canonical),
	}

	dir := s.dirFor(canonical)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return Hold{}, err
	}
	h.file = filepath.Join(dir, sanitizeHolder(o.Holder))
	if err := writeHold(h); err != nil {
		return Hold{}, err
	}
	return h, nil
}

// derivePathMatches gives a path resource its own path, absolute and
// ~-abbreviated. These are boundary-tested rather than matched as substrings: a
// hold on path:~/tmp must not deny `ls ~/tmpfoo`.
//
// A non-path resource derives nothing. There is no text in `gpu:0` worth
// matching, and inventing some would only produce false positives.
func derivePathMatches(canonical string) []string {
	p := PathPart(canonical)
	if p == "" {
		return nil
	}
	out := []string{p}
	if home := homeDir(); home != "" && strings.HasPrefix(p, home) {
		out = append(out, "~"+strings.TrimPrefix(p, home))
	}
	return dedupe(out)
}

func dedupe(in []string) []string {
	seen := map[string]bool{}
	var out []string
	for _, s := range in {
		if s == "" || seen[s] {
			continue
		}
		seen[s] = true
		out = append(out, s)
	}
	return out
}

// ErrNotHeld is returned when there is nothing of yours to release or renew.
var ErrNotHeld = errors.New("not held")

// ErrForeign is returned when the live hold belongs to someone else.
type ErrForeign struct{ By Hold }

func (e ErrForeign) Error() string {
	return fmt.Sprintf("%s is held by %s, not by you", e.By.Resource, e.By.Holder)
}

// Release drops one holder's claim.
func (s Store) Release(resource, holder string, now time.Time) (Hold, error) {
	if now.IsZero() {
		now = time.Now()
	}
	if holder == "" {
		holder = Holder()
	}
	canonical := Canonical(resource)

	live, err := s.Live(canonical, now, true)
	if err != nil {
		return Hold{}, err
	}
	if len(live) == 0 {
		return Hold{}, ErrNotHeld
	}
	for _, h := range live {
		if h.Holder == holder {
			if err := os.Remove(h.file); err != nil {
				return Hold{}, err
			}
			return h, nil
		}
	}
	return Hold{}, ErrForeign{By: live[0]}
}

// Renew pushes a holder's deadline out, carrying the reason and the explicit
// match literals forward so a renewal cannot quietly narrow what is protected.
// PathMatches are re-derived from the resource rather than carried.
func (s Store) Renew(resource, holder string, ttl time.Duration, now time.Time) (Hold, error) {
	if now.IsZero() {
		now = time.Now()
	}
	if holder == "" {
		holder = Holder()
	}
	canonical := Canonical(resource)

	live, err := s.Live(canonical, now, true)
	if err != nil {
		return Hold{}, err
	}
	for _, h := range live {
		if h.Holder == holder {
			return s.Acquire(AcquireOpts{
				Resource: canonical,
				Holder:   holder,
				TTL:      ttl,
				Reason:   h.Reason,
				Matches:  h.Matches,
				Shared:   h.Mode == ModeShared,
				Now:      now,
			})
		}
	}
	return Hold{}, ErrNotHeld
}

// Check reports whether Acquire would succeed: true when the resource is free,
// already yours, or shared. A shared resource never blocks a new holder, which
// is the whole point of it.
func (s Store) Check(resource, holder string, shared bool, now time.Time) (bool, error) {
	if now.IsZero() {
		now = time.Now()
	}
	if holder == "" {
		holder = Holder()
	}
	canonical := Canonical(resource)

	live, err := s.Live(canonical, now, true)
	if err != nil {
		return false, err
	}
	if shared {
		return true, nil
	}
	for _, h := range live {
		if h.Holder != holder {
			return false, nil
		}
	}
	return true, nil
}
