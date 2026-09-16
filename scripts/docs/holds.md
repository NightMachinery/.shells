# Holds: keeping one agent out of another's way

A **hold** is a self-expiring, advisory claim on a named resource — a
repository, a GPU, a service — taken by one session so that a parallel session
does not walk into the middle of something atomic. It was built for a history
rewrite in `~/scripts`, where a second agent committing halfway through would
have been expensive to unpick.

    hold-acquire repo:~/scripts --ttl 45m --reason "history rewrite" --match "vcsh night.sh"
    hold-status
    hold-release repo:~/scripts

Three parts. `golang/night_hold` does the work; the `hold-*` zsh functions in
`zshlang/auto-load/others/hold.zsh` are wrappers; and a Claude Code
`PreToolUse` hook running `night_hold guard` enforces it. The Hammerspoon
auto-reload holds ([agfi:hs-reload-hold]) are the same mechanism in its other
mode.

## The commands

`hold-acquire <resource> [--ttl D] [--reason S] [--match S]... [--shared]
[--wait D] [--holder ID]` takes the hold. Re-acquiring your own live hold
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

`hold-renew <resource> [--ttl D]` pushes your deadline out. Rarely needed by an
agent, because the guard already refreshes a hold while its holder is working.

`hold-status [<resource>]` prints who holds what, why, and for how long.
`hold-holders <resource>` prints just the live holder ids.

## Two modes, one mechanism

**Exclusive** is the default and is a lock: a second holder is refused. A
repository wants this.

**Shared** (`--shared`) is not a weaker lock, it is a different thing — a
**suppression registry**. Every holder wants the same outcome, so a second
costs nothing and refusing it would be perverse; what they need from a hold is
only the expiry. The Hammerspoon auto-reloader is the one user: several agents
editing `.lua` files at once each hold off the reloader, and whoever finishes
first must not re-enable it under someone still typing.

Mixing modes on one resource is refused rather than guessed at.

This is why the store is a directory per resource and a file per holder: that
one shape carries both semantics.

## Three ways a hold ends

They are independent, and each answers a question the others cannot.

**The deadline (TTL)** is the backstop, and it is never obsolete. It is the
only thing that works when there is no trustworthy pid — a hold taken by a
script, by a shell shared between sessions, or by an agent that exports no pid
— and the only thing that works across hosts, since a pid means nothing on
another machine and `$HOME` is shared between them in some setups. It also
covers the most common real failure, which is not a crash at all: an agent that
is perfectly alive, has moved on to something else, and forgot to release.

**Liveness** ends a hold *early*. The hold records the agent session's
long-lived pid and its host, and is reaped the moment that process is provably
gone. A killed agent frees the repository at once instead of blocking everyone
for half an hour.

Which pid matters enormously. The obvious one, the pid of the shell that ran
`hold-acquire`, is useless — it exits a millisecond later, and trusting it
would reap every hold the instant it was taken. So a pid is recorded together
with the kind of pid it is, and only a long-lived one is ever allowed to
declare a hold dead. Claude Code exports `CLAUDE_PID`; anything else can export
`hold_agent_pid`. With neither, the deadline is the only backstop, which is
exactly the behaviour this had before liveness existed. PID reuse can make a
dead holder look alive, which fails in the safe direction.

**Keepalive** extends a hold while its holder is working. The guard runs before
every tool call, and when the caller owns a hold it pushes the deadline out —
only once less than half the window remains, so this costs a handful of writes
per window rather than one per tool call.

Together these change what the TTL *means*. It stops being "how long I guess
this will take", which nobody can answer up front, and becomes "how long after
I go quiet" — a question with an obvious answer.

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
the holder is the agent session id, a compaction starts a new one, and an agent
would be locked out of a repository by its own stale hold with no way to
release it before the deadline.

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

**The deadline is written twice.** The contents are authoritative for everyone
who parses. The mtime exists for one reader that must not parse:
`hammerspoonReloadHeldBy()` in `hammerspoon/core/reload.lua` runs on
Hammerspoon's main thread and answers by iterating the directory and statting
each entry. (An earlier version of this document claimed it answered from a
single stat. It never did — but it has always done no parsing, and that is the
constraint worth keeping.) Contents are written first and the mtime last, so
the mtime is never ahead of the file.

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
