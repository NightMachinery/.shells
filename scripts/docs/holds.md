# Holds: keeping one agent out of another's way

A **hold** is a self-expiring, advisory claim on a named resource — a
repository, a GPU, a service — taken by one session so that a parallel session
does not walk into the middle of something atomic. It was built for a history
rewrite in `~/scripts`, where a second agent committing halfway through would
have been expensive to unpick.

    hold-acquire repo:~/scripts --reason "history rewrite" --match "vcsh night.sh"
    hold-status
    hold-release repo:~/scripts

Three parts. `golang/night_hold` does the work; the `hold-*` zsh functions in
`zshlang/auto-load/others/hold.zsh` are wrappers; and a Claude Code
`PreToolUse` hook running `night_hold guard` enforces it. The Hammerspoon
auto-reload holds ([agfi:hs-reload-hold]) are the same mechanism in its other
mode.

## The commands

`hold-acquire <resource> [--ttl D] [--reason S] [--match S]... [--shared]
[--wait D] [--holder ID]` takes the hold. `--ttl` is optional and usually
wrong to pass: by default a hold lasts until you release it or until the agent
holding it dies. Pass one only when you want a hard deadline. Re-acquiring your
own live hold
renews it rather than failing, so a long job can call it again without tracking
whether it already holds one. Against someone else's live exclusive hold it
fails, non-zero and loudly — unless `--wait` is given, in which case it retries
until the budget runs out. Waiting is right when your next step *is* the held
resource and you have nothing better to do; failing fast is right otherwise.

`hold-release <resource>` drops yours. Releasing someone else's is refused —
with concurrent agents that would open the resource under whoever is still
working — and the error prints the `--holder` incantation for the rare case
where you genuinely have to.

`hold-check <resource>` is the silent one, for use in a condition: it succeeds
when acquiring would succeed.

`hold-renew <resource> [--ttl D]` changes the deadline on a hold you already
have — out, in, or away entirely, since `--ttl until-live` removes it. Only
meaningful for a hold that has one.

`hold-status [<resource>]` prints who holds what, why, and for how long.
`hold-holders <resource>` prints just the live holder ids.

## Two modes, one mechanism

**Exclusive** is the default and is a lock: a second holder is refused. A
repository wants this.

**Shared** (`--shared`) is not a weaker lock, it is a different thing — a
**suppression registry**. Every holder wants the same outcome, so a second
costs nothing and refusing it would be perverse; what they need from a hold is
only the *ending*. The Hammerspoon auto-reloader is the one user: several agents
editing `.lua` files at once each hold off the reloader, and whoever finishes
first must not re-enable it under someone still typing.

Mixing modes on one resource is refused rather than guessed at.

This is why the store is a directory per resource and a file per holder: that
one shape carries both semantics.

## Three ways a hold ends

They are independent, and each answers a question the others cannot.

**Release** is the normal one. Say so when you are done, including when you
stop early or hand back unfinished.

**Death** is the automatic one, and since holds became `until-live` by default
it is the primary one. A hold records the agent session's long-lived pid and
its host, and is reaped the moment that process is provably gone. A killed
agent frees the repository at once rather than blocking everyone until a clock
runs out.

Which pid matters enormously. The obvious one, the pid of the shell that ran
`hold-acquire`, is useless — it exits a millisecond later, and trusting it
would reap every hold the instant it was taken. So a pid is recorded together
with the kind of pid it is, and only a long-lived one is ever allowed to
declare a hold dead. Claude Code exports `CLAUDE_PID`; anything else can export
`hold_agent_pid`. PID reuse can make a dead holder look alive, which fails in
the safe direction.

**The deadline (TTL)** is now opt-in, and means exactly what it says: the hold
is gone when it runs out, and nothing renews it behind your back. Reach for it
when you want a hard bound — "this must definitely not still be here at 4pm" —
rather than as a guess at how long a job will take, which is a question nobody
can answer up front.

It also comes back uninvited in one case. `until-live` is only meaningful when
something can declare the holder dead, so a hold taken **without an agent pid**
— from a plain terminal, a script, a BrishGarden shell — would otherwise expire
by no mechanism at all. Those silently get 30 minutes instead, and record why,
so `hold-status` can explain a deadline you did not ask for.

### What is no longer covered

An agent that is **alive and has simply forgotten**. With no deadline running
underneath it, that hold stands until somebody ends it. This is deliberate: a
hold only ever blocks someone when another agent is running, and another agent
is running because a person started it — so there is a person present at
exactly the moment it matters. The blocked agent says who holds what and asks;
`hold-status` names it and `hold-release <resource> --holder <id>` clears it.

The same goes for a hold taken on **another host**. A pid means nothing on a
different machine, so liveness cannot reap it from here, and some hosts share a
home directory — the CIS servers do — which makes that reachable in practice
rather than theoretical. `hold-status` flags such a hold rather than leaving
you to wait for a clock that may not exist.

### Who "you" are

The holder is the agent session id, and that is **not stable**: moving a session
into `claude agents` gives it a new one while the process, the working directory
and the intent all stay the same, and a resume does it too. Left there, an agent
gets denied its own repository by its own hold and cannot even release it. This
is not theoretical — the session that built this watched its own id change
underneath it and was told to pass `--holder`. (Do not assume a compaction does
it. That was the first guess here and it was wrong.)

So a hold is yours if the holder id matches *or* if it records the same agent
pid on the same host. The pid survives what the session id does not, and the
guard inherits `CLAUDE_PID` from the agent that spawns it, so this works at the
point it matters most.

Naming a holder explicitly — `--holder`, or `$hold_holder` — deliberately turns
the pid half off. Naming one means "act as exactly this holder", which is how a
shell impersonates another session, in tests and when deliberately clearing
someone else's hold; letting the pid override that would make the override
unusable from the one machine it is ever used from.

## What the guard actually stops

`night_hold guard` runs before every `Bash`, `Edit`, `Write`, `MultiEdit` and
`NotebookEdit`, and denies the call when:

- the tool's `file_path` is inside a held path;
- a `Bash` call's working directory is inside a held path;
- a `Bash` command names the held path, or contains one of the hold's explicit
  `--match` literals.

### Two kinds of match, on purpose

A path resource gets its own path matched for free, absolute and
`~`-abbreviated, and those are tested **with path boundaries**: an occurrence
counts only when what precedes and follows it could not be part of a longer
word. A plain substring test was wrong, and obviously so once tried — a hold on
`path:~/tmp` denied `ls ~/tmpfoo`. A guard that cries wolf before every tool
call teaches everyone to route around it, so a false positive costs more than a
miss.

`--match` literals are tested as **plain substrings**, because the caller asked
for that exact text. For a vcsh repository this is not optional: `vcsh night.sh
commit` names the path nowhere, so the path tests alone would let the most
dangerous command straight through.

Resources that are not paths — `gpu:0`, `service:garden` — derive no matches at
all, so the guard can only block on `--match` literals you supply and a bare
`gpu:0` hold is purely advisory. There is no text in `0` worth matching.

The `hold-*` commands themselves are never blocked. Without that, the guard
would deny the very command that clears a hold — and that is not hypothetical:
the holder is the agent session id, that id can change mid-task, and an agent
would be locked out of a repository by its own hold with no way to release it.

### Not a security boundary

It stops accidents, not a determined process. Nothing prevents an agent from
spelling a path in a way no literal catches, and the `Bash` tests are textual
by necessity: there is no general way to read the filesystem effects out of an
arbitrary shell command. Treat it as a seatbelt.

It also **fails open**, deliberately: an unparseable payload, a corrupt hold
file, a missing binary all allow the call. A guard that bricks every agent in
the house when a dependency goes missing is worse than the accident it
prevents. The hook line probes for the binary and exits 0 when it is absent —
it must never trigger a build, because a `go build` before a tool call would be
awful. Measured at about 2.2ms with no holds outstanding, which is the usual
case.

## Implementation notes

State is `~/.night-holds/<resource-slug>/<holder>`, `key: value` lines,
deliberately human-readable: the first thing anyone does with a hold that will
not go away is `cat` it. Expired and dead holds are reaped by the writing side;
the guard ignores them without deleting, because something on the hot path
before every tool call has no business removing files.

Four decisions worth knowing about, because each had a wrong-looking obvious
alternative.

**Why it is Go.** This was shell first, and it worked. Three of the bugs found
writing it were shell footguns rather than logic errors: a tab IFS collapsing
empty fields so an empty `file_path` shifted the command into the wrong
variable; `nobareglobqual` in the shell snapshot Claude Code hands its agents,
where `*(N)` is not a qualifier and the listing failed outright; and the
substring-versus-boundary match above. The port then found a fourth that shell
cannot express at all — see the next point.

**The acquire is serialized with flock.** Read-check-write had no critical
section, so two processes could both see "free" and both write, and an
exclusive hold could quietly have two holders. This is not a reversal of "holds
are not flocks": that objection is about the *hold*, which must outlive the
shell that took it and so cannot be descriptor-scoped. This lock lives for the
microseconds of the acquire, which is exactly what flock is for.

**Not `zsystem flock` for the hold itself,** though `~/scripts` argues well for
flock over redis elsewhere ([agfi:lock-acquire]): a flock is released when the
process holding the descriptor dies, and every agent tool call is a *new*
process.

**Hammerspoon asks the binary rather than reading the files.** The deadline
used to be mirrored into each hold's mtime so that `reload.lua` could answer
from a stat per entry, with no parsing, on Hammerspoon's main thread. That
stopped being possible when holds started ending on *death*: whether a pid is
alive is not a question a stat can answer, and a check written in Lua would
keep auto-reload suppressed by a crashed agent's leftover file. So it runs
`night_hold holders service:hs-reload` through `taskWithPath`, asynchronously,
and fails open. The mtime is still written for a hold that has a deadline, but
only so that `ls -l` is informative; nothing reads it.

## State directory

`~/.night-holds/`, and not under `$TMPDIR` as first sketched. Every reader has
to agree on the path, and the readers are a login shell, a BrishGarden shell, a
Hammerspoon Lua reader and the binary; `$TMPDIR` is per-context on macOS and
simply unset in some of them, which would have split the state silently.
`$HOME` is the one thing they all spell the same way. It is inside the vcsh
work tree, which is harmless: that repository is always read with `status
-uno`.

## Deferred

`golang/night_hold/todo.org` records two deliberate deferrals: extracting the
portable half into a standalone shareable repository, and enforcing holds for
Codex and Antigravity as well as Claude Code.

`docs/disabled/holds-ttl-extension.md` records a third, which was built and
then removed: the hooks that used to extend a hold while its holder was
working. It is written up in enough detail to wire again, including what would
have to come back.
