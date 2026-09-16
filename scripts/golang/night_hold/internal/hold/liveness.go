package hold

import "os"

// A hold records a pid so a dead holder can be reaped early instead of
// blocking everyone until its deadline. Which pid matters enormously.
//
// The obvious one -- os.Getpid() -- is useless: it belongs to the shell that
// ran `hold-acquire`, which exits a millisecond later, and trusting it would
// reap every hold the instant it was taken. What is wanted is the pid of the
// thing whose death should release the hold: the agent session itself.
//
// So a pid is recorded with the kind of pid it is, and only an `agent` pid is
// ever used to declare a hold dead. When there is no trustworthy pid -- a
// script, a BrishGarden shell shared between sessions, an agent that exports
// no pid -- the deadline remains the only backstop, which is exactly the
// behaviour this started with.
const (
	pidAgent = "agent"
	pidShell = "shell"
)

// agentPID returns the long-lived pid of the calling agent session, if it
// exports one.
func agentPID() (int, string) {
	for _, env := range []string{"hold_agent_pid", "CLAUDE_PID"} {
		if v := os.Getenv(env); v != "" {
			if n := atoiSafe(v); n > 0 {
				return n, pidAgent
			}
		}
	}
	return os.Getpid(), pidShell
}

func atoiSafe(s string) int {
	n := 0
	for _, r := range s {
		if r < '0' || r > '9' {
			return 0
		}
		n = n*10 + int(r-'0')
		if n > 1<<30 {
			return 0
		}
	}
	return n
}

// Dead reports that the holder is provably gone, so the hold can be reaped
// before its deadline.
//
// Three conditions, all necessary. The pid must be one worth trusting; the
// hold must have been taken on this host, because a pid means nothing on
// another one (and $HOME is shared between hosts in some setups); and the
// process must actually be gone. PID reuse can make a dead holder look alive,
// which fails in the safe direction -- the hold simply runs to its deadline as
// it always did.
func (h Hold) Dead(thisHost string) bool {
	return h.PIDKind == pidAgent &&
		h.Host != "" && h.Host == thisHost &&
		h.PID > 0 && !processAlive(h.PID)
}

func thisHost() string {
	n, _ := os.Hostname()
	return n
}

// Caller is who is asking, for Mine.
type Caller struct {
	Holder  string
	PID     int
	PIDKind string
	Host    string
}

// Mine reports whether a hold belongs to the caller.
//
// The holder id is the agent *session* id, and that is not stable: a
// compaction or a resume starts a new one while the process, the working
// directory and the intent all stay the same. Observed, not theorised -- the
// session that built this watched its own id change underneath it, and was
// then refused a release of its own hold and told to pass --holder.
//
// The agent pid survives what the session id does not, so it is the stronger
// identity and is checked as well. Both sides must be a pid worth trusting and
// on the same host, for the same reasons Dead insists on.
func (h Hold) Mine(c Caller) bool {
	if c.Holder != "" && h.Holder == c.Holder {
		return true
	}
	return c.PIDKind == pidAgent && h.PIDKind == pidAgent &&
		c.PID > 0 && h.PID == c.PID &&
		h.Host != "" && h.Host == c.Host
}

// callerFor is the identity of a command-line caller.
//
// An explicit holder -- from --holder or $hold_holder -- deliberately drops the
// pid identity. Naming a holder means "act as exactly this one", which is how a
// shell impersonates another session: in tests, and when deliberately clearing
// a hold that is not yours. Letting the pid override that would make the
// override unusable from the machine that owns the hold, which is the only
// machine it is ever used from.
func callerFor(explicit string) Caller {
	if explicit != "" || os.Getenv("hold_holder") != "" {
		if explicit == "" {
			explicit = Holder()
		}
		return Caller{Holder: explicit}
	}
	pid, kind := agentPID()
	return Caller{Holder: Holder(), PID: pid, PIDKind: kind, Host: thisHost()}
}

// GuardCaller is the identity of the PreToolUse guard.
//
// Unlike callerFor, the session id here is never an impersonation -- it is the
// current, fickle name for this very process -- so the pid identity is kept.
// It is what stops an agent being denied its own repository by its own hold
// after a compaction.
func GuardCaller(sessionHolder string) Caller {
	pid, kind := agentPID()
	return Caller{Holder: sessionHolder, PID: pid, PIDKind: kind, Host: thisHost()}
}
