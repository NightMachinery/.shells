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
