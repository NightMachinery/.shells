# Holds: keeping one agent out of another's way

A **hold** is a self-expiring, advisory claim on a named resource — a
repository, a GPU, a service — taken by one session so that a parallel session
does not walk into the middle of something atomic. It was built for the GCP
history rewrite in `~/scripts`, where a second agent committing halfway through
would have been expensive to unpick.

    hold-acquire repo:~/scripts --ttl 45m --reason "history rewrite" --match "vcsh night.sh"
    hold-status
    hold-release repo:~/scripts

Two halves, and the split is the design: [agfi:hold-acquire] and friends
*record* a hold, and `configFiles/claude-code/hooks/hold-guard.sh`, a Claude
Code `PreToolUse` hook, *enforces* it. The recording side is
`zshlang/auto-load/others/hold.zsh`.

## The commands

`hold-acquire <resource> [--ttl <dur>] [--reason <text>] [--match <literal>]...`
takes the hold. Re-acquiring your own live hold renews it rather than failing,
so a long job can call it again without tracking whether it already holds one.
It fails, non-zero and loudly, against a live hold held by anyone else.

`hold-release <resource>` drops yours. Releasing someone else's is refused —
with concurrent agents that would open the resource under whoever is still
working — and the error prints the `hold_holder=<name>` incantation for the
rare case where you genuinely have to.

`hold-check <resource>` is the silent one, for use in a condition: it succeeds
when the resource is free *or already yours*, and fails when someone else holds
it.

`hold-renew <resource> [--ttl <dur>]` pushes your deadline out, carrying the
existing reason and match list forward so a renewal cannot quietly narrow what
is protected.

`hold-status [<resource>]` prints who holds what, why, and for how long.

## Resource names

Any string. The `repo:`, `path:`, `dir:` and `file:` prefixes are special:
their remainder is run through [agfi:path-unabbrev] and resolved to an absolute
path, so `repo:~/scripts`, `repo:~[base]/scripts` and
`repo:/Users/evar/scripts` are one resource rather than three. A guard that did
not know that would silently protect nothing.

Everything else passes through untouched, so `gpu:0` and `service:garden` work
and mean whatever the callers agree they mean.

## Why it expires

This is the load-bearing property, inherited from the Hammerspoon auto-reload
holds ([agfi:hs-reload-hold]) that this generalizes: **a hold always expires on
its own.** An agent that crashes, is killed, or simply has its context
compacted must not leave a repository locked forever. The default is 30
minutes, the same as the agent banner's, for the same reason — long enough to
be useful, short enough that forgetting is not a lasting problem.

Compaction is worth calling out. The holder id comes from
`$CLAUDE_CODE_SESSION_ID` (falling back through Codex and Antigravity ids, then
`$$@$HOST`), and that is per *session*: resuming or compacting a conversation
starts a new one, and the hold becomes unreleasable by name. The deadline is
the backstop. `hold_holder=<name> hold-release <resource>` is the manual
override.

## What the guard actually stops

`hold-guard.sh` runs before every `Bash`, `Edit`, `Write`, `MultiEdit` and
`NotebookEdit`, and denies the call when:

- the tool's `file_path` is inside a held path;
- a `Bash` call's working directory is inside a held path;
- a `Bash` command contains one of the hold's `match:` literals.

A path hold gets two match literals for free: the absolute path and its
`~`-abbreviated form. `--match` adds more, and for a vcsh repository that is
not optional — `vcsh night.sh commit` names the path nowhere, so the path tests
alone would let it straight through. That is exactly the command you most want
stopped.

The `hold-*` commands themselves are always allowed, even when the command
names a held resource. Without that, the guard would deny the very command that
clears a hold — and that is not hypothetical: the holder id is the agent session
id, a compaction starts a new one, and an agent would then be locked out of a
repository by its own stale hold with no way to release it before the deadline.

### Not a security boundary

It stops accidents, not a determined process. Nothing prevents an agent from
spelling a path in a way no literal catches, and the `Bash` tests are textual
by necessity: there is no general way to read the filesystem effects out of an
arbitrary shell command. Treat it as a seatbelt.

It also **fails open**, deliberately: missing `jq`, an unparseable payload, a
corrupt hold file all allow the call. A guard that bricks every agent in the
house when a dependency goes missing is worse than the accident it prevents.

## Implementation notes

State lives in one file per resource under `~/.night-holds/`, named after a
slugged form of the canonical resource, carrying the holder, deadline, pid,
host, reason and match list. Expired files are reaped on sight by the zsh side;
the guard ignores them without deleting, because something on the hot path
before every tool call has no business removing files.

Three decisions worth knowing about, because each one had a wrong-looking
obvious alternative:

**Not `zsystem flock`.** `~/scripts` argues well for flock over redis for
ordinary locking ([agfi:lock-acquire]). It is wrong here: a flock is released
when the process holding the descriptor dies, and every agent tool call is a
*new* process. A hold has to outlive the shell that took it, so it is a
self-expiring record instead.

**The deadline is in the file contents, not the mtime.** The Hammerspoon holds
put it in the mtime so a Lua reader on the main thread could answer from a
single `stat` with no parsing. Every reader here wants the holder and reason
too, so it opens the file regardless — and a deadline in the mtime would only
be a second source of truth to disagree with. It also spares the guard, which
is `sh`, a portable-`stat` problem: BSD and GNU spell mtime differently and
both are on the PATH on this machine.

**The guard is not a brishz call**, unlike every other hook in
`configFiles/claude-code/settings.json`. Two reasons. A `PreToolUse` hook
blocks by exiting 2, and `brishz.dash` returns *curl's* exit code rather than
the command's, so a hook routed through the garden could never block at all.
And this runs before every tool call, so it has to cost nothing: measured at
about 3ms with no holds outstanding, which is the usual case, rising to ~18ms
when there is one and `jq` has to run.

The corollary of not being a brishz call is that it does **not** need
`brishz-restart` after an edit — but `hold.zsh` does, like all zshlang.

## State directory

`~/.night-holds/`, and not under `$TMPDIR` as first sketched. Every reader has
to agree on the path, and the readers are a login shell, a BrishGarden shell
and an `sh` hook; `$TMPDIR` is per-context on macOS and simply unset in some of
them, which would have split the state silently. `$HOME` is the one thing they
all spell the same way. It is inside the vcsh work tree, which is harmless:
that repository is always read with `status -uno`.
