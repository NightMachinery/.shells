# Holds: extending a TTL from hooks

Removed 2026-09-16. Two mechanisms that kept a hold's deadline ahead of its
holder, plus a third that was designed and never built. See `docs/holds.md` for
how holds work now.

## What they did

**Keepalive** lived in the `PreToolUse` guard. The guard already loads every
live hold before each tool call to decide whether to deny it, so when a hold
turned out to belong to the caller it pushed that hold's deadline out — for
free, on a scan that was happening anyway. A floor of `0.5` meant it only wrote
when less than half the window remained, so a 45-minute hold cost a handful of
writes per window rather than one per tool call.

**The Stop refresh** was `night_hold refresh` on Claude Code's `Stop` hook. It
restarted the clock on every hold the session owned, at the turn boundary.

## Why they existed

To turn the TTL from *"how long do I think this will take"*, which nobody can
answer up front, into *"how long after I go quiet"*, which has an obvious
answer.

The Stop hook specifically closed a case that tool calls alone cannot, and it
is a subtle one worth keeping written down: **an agent that is alive, holds a
repository, and is waiting for the user to answer a question makes no tool
calls at all.** Keepalive never fires, and the hold lapses underneath it while
it sits there. Liveness does not save it either, because liveness only ever
ends a hold *early*.

## Why they went

They defeated the thing they were protecting. Once liveness reaps a dead
holder, the only case a deadline is still for is an agent that is **alive and
has forgotten** — and an agent that is alive and has forgotten is precisely one
still making tool calls and still hitting `Stop`. So the two mechanisms renewed
exactly the case the deadline existed to catch, and the TTL became unreachable:
a number in the file that no longer described anything.

The system already behaved like liveness-only. It just took ~120 lines and a
hook on the hot path to get there. Making `--ttl=until-live` the default says
the same thing in one flag, and lets `--ttl 45m` mean 45 minutes again.

## If it is ever wired again

**Do not restart the clock to the full TTL.** That is what the removed code
did, and it is wrong: a 45-minute hold stays 45 minutes out forever while you
work. Maintain a **floor** instead —

    if h.Until.Sub(now) < floor { h.Until = now.Add(floor) }   // never shortens

— with a floor of 10 minutes. The semantic that produces is "this hold dies
within 10 minutes of its holder going quiet", whatever the nominal TTL, and the
overrun is bounded by the floor rather than by the TTL.

**Opt-in, behind `--hooks-extend-ttl`.** Off by default, no environment knob.
A hold that did not ask for it keeps a hard deadline, which is the whole point
of having asked for a deadline. An `until-live` hold has none to top up and
ignores the flag.

**Background the hook with `&`.** It costs about 5 ms, nothing waits on the
result, and the process is not killed by the session going idle. The earlier
claim that it had to be synchronous to be safe was wrong.

**Do not route it through brishz.** `brishz_async=y` makes the work async
*inside* BrishGarden, but the hook still blocks on a curl to the garden — so
that flavour of "async" adds a wedge that a direct exec does not have. The
`PreToolUse` guard has a second, harder reason: `brishz.dash` returns curl's
exit status, so the exit 2 that *is* the denial never reaches Claude Code.

**Restore commit `203ce158`.** It added `AcquireOpts.BestEffort`, `errBusy` and
a non-blocking mode on `withResourceLock`, so that a hook on the hot path could
not stall behind a process wedged mid-acquire. It was deleted along with its
only two callers, and any new hook that calls `Acquire` before a tool call
needs it back.
