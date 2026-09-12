# Agent sessions: one hotkey, one picker, three agents

Claude Code, Codex and Antigravity (`agy`) all keep a transcript of every
session on disk, and all three are useless to you there: a directory of JSONL
under a name you did not choose. This is the family of helpers that turns any
of them into something you can read, resume and jump to -- the same key, the
same picker, the same org file, whichever agent wrote it.

Start here:

- `cmd+shift+o` in kitty opens the session showing in the focused window as an
  org file in emacs. It works out which agent that is by itself.
- `agent-view-session-fz` picks a session from disk and opens it; add `-all-fz`
  for every project rather than this one.
- `agent-session-live-fz` picks from the sessions running right now.
- `agent-session-resume-fz` resumes one, handing off to that agent's own resume
  command.
- `ffta` picks among the tmux sessions that are running an agent, the one you
  spoke to last at the top, and goes to it. `fftaa` adds the ones `/done` has
  ended, which are still sitting there as a dead pane showing their report.

Everything below is why those five work, and what to change when they do not.

## The agent travels with the transcript path

Nothing in this family passes an agent token around. A transcript path is the
handle for a session, and [agfi:h-agent-session-agent-of] recovers the agent
from the path by asking each adapter's `owns-p` verb, which is a glob against
that agent's store.

This is not tidiness. The hotkey's real work happens in a `tmuxnewsh2` session
started by [agfi:h-agent-view-job], a different process, where zsh's dynamic
scope does not reach; the registry is a file whose contents are one line; fzf
rows cross into a `zsh -f` wrapper that cannot call functions at all. A token
would have to survive all three, and a path already does.

The one case a path cannot answer is a transcript copied out of its store, for
which [agfi:h-agent-session-sniff] reads the first record's shape and guesses.
It is the last resort, not the mechanism.

Only the functions that have no transcript yet take an agent argument: `roots`,
`list`, `live-list`, `current-*` and the hook bodies.

## Window to session, in order of how much each can promise

[agfi:h-agent-session-of-kitty-window] is what the hotkey leans on. It is given
kitty's `ls` JSON and a window id, and tries five things:

1. **The hooks already wrote the answer on the tmux session.** When the window
   shows a tmux client, the autoname hooks have recorded the identity on that
   client's session as the `@agent_session` user option -- agent, id and
   transcript, tab separated -- so the option answers directly, for any agent.
   This is tried before anything else because it is the only answer that needs
   no live listing, and the listing is where the time goes; the next section
   has the numbers. It cannot contradict the tests below either, since a window
   showing a tmux client has none of the agent's own processes in the
   foreground.

   The subtlety is which session to ask. The session has to be found from the
   client's *pid*, through one `tmux list-clients`
   ([agfi:h-agent-session-tmux-clients]), and not from the name the window's
   command line spells: the autoname hooks rename a session after the agent
   living in it, so a window still running `tmux attach -t scripts-claudework1`
   is attached to something now called `+Claude/work claude-session-helpers-refactor`.
   Matching the spelled name looks right and silently never matches.
2. **The agent runs in the window.** A foreground pid of the window equals a
   live session's pid. Nothing to go stale here.
3. **A tmux client whose session the hooks never recorded.** The session name is
   matched against the live rows' tmux column, and only an unambiguous single
   hit counts: one tmux session can host several agents in several panes. This
   is what covers an agent whose hooks are not trusted yet.
4. **The window shows an agent's own view** (`claude agents`, `claude attach`).
   Those set the window title to the attached session's name, sometimes behind
   a status glyph. That is observed rather than documented, so it counts only
   when a foreground process is one of the agents' binaries and the title
   matches exactly one live session.
5. **The registry.** A file per kitty window under
   `$XDG_STATE_HOME/agent-sessions`, written by the hooks, saying where the
   session was showing the last time it was prompted.

The first four read only what is true this instant. The fifth is a memory, so
it comes last. When all five come up empty the hotkey does not guess: it opens
the picker as a kitty overlay over the window
([agfi:h-agent-session-pick-overlay]), and `enter` there opens the choice in
the background under the same band.

## Why the lookup is fast

Pressing `cmd+shift+o` used to cost about 400ms before anything appeared on
screen, and almost none of that was reading the transcript. It was spent asking
every agent what was running, which the resolver did up front whether it needed
the answer or not.

Reading the hooks' record off the tmux session first is what removed most of
it. A hooked tmux window now resolves in 16ms instead of 365ms, because nothing
is listed at all: one `tmux list-clients`, one `tmux list-sessions`, and the
transcript path is in hand. A session the hooks never recorded -- a Codex whose
hook handlers have not been trusted yet, say -- still falls through to the live
listing and takes about 160ms, which is the honest cost of the question.

When the listing is genuinely needed, it is one call rather than three.
`agent_session live-all claude=<root> codex=<root> agy=<root>` runs the three
adapters concurrently and shares one process table, one `lsof` and one
`tmux list-panes` between them; an agent that fails contributes no rows instead
of failing the batch. Three sequential per-agent calls cost about 950ms, mostly
paid three times over for the same process listing. Batched that came to about
205ms, of which `claude agents --json` was 190ms; reading Claude Code's own
session records instead of running that CLI took the batch to about 105ms,
which is what one `ps` over some 1500 processes costs on its own. Sharing that
one listing is now the whole of what batching buys.

Where a caller already knows which transcripts it cares about, it says so:
`list -only <transcript>` (repeatable) resolves those paths directly instead of
walking a corpus, which took `codex list` from 276ms to 6ms, and the pickers
pass `-only` per agent to attach a name and a timestamp to rows they already
have. Codex's liveness probe restricts `lsof` to pids in Codex's own process
ancestry and does one glob over the rollout tree rather than fourteen, which
took it from 541ms to 158ms.

The rest was repetition. Reads that used to happen once per kitty window are
batched: one `kitty @ ls` through one `jq` ([agfi:h-agent-session-windows], one
line per window carrying its id, title, foreground pids and their command
lines), one `tmux list-sessions` for every identity, one `tmux list-clients`
for every client. And [agfi:h-agent-session-dep], which guards every render,
name, listing and preview, probes with `whence -p agent_session` rather than
reading `$commands`: the latter makes zsh hash the whole `PATH`, 65ms in a
garden shell that forks per call and so never has a warm hash. That guard went
from 124ms to about 1ms.

Every number here is measured through the BrishGarden fork, which itself costs
about 62ms, so the floor is not zero. The corpus picker is the one path still
measured in hundreds of milliseconds ([agfi:h-agent-session-pick-rows], about
685ms for 27 rows); it reads every root and is deliberately not on the hotkey's
path.

## The adapter contract

An adapter is a set of zsh functions named `h-<agent>-session-<verb>`, and
[agfi:h-agent-session-call] dispatches to them. It returns 2 when a verb is not
defined, so "this agent cannot do that" stays distinguishable from "that
failed".

The verbs, all of them arguments in and lines out, tab separated where a line
has fields:

- `roots` -- one store root per line, and failure when the agent is not
  installed on this host, which is how such a host contributes no rows.
- `owns-p <path>` -- exit status only, a glob against the roots.
- `id-of <transcript>` -- the agent's own id for that session.
- `resolve <id-or-prefix>` -- matching transcripts, one per line. The core's
  [agfi:h-agent-session-resolve] takes a path as itself, else collects across
  agents and complains about none or several.
- `current-id` -- the id of the session this shell is inside, from the agent's
  environment.
- `live-list` -- eight columns: pid, id, name, cwd, transcript, tmux session or
  `-`, status, kind (`interactive`, `background`, or `-`).
- `resume <transcript> [args]` -- executes the agent's resume.
- `hook-transcript [payload]` -- the transcript a hook payload is about, with
  the payload in `$1` or on stdin.
- `name <transcript>` and `tmux-name <transcript>` are optional; an agent that
  omits `name` gets the Go binary's answer.

### Resuming lands in the session's own directory

None of the three agents restores the working directory when you resume:
Claude Code's helper used to only *warn* that "tools will run in `$PWD`", and
Codex and agy did not even warn. Resuming a session into a directory it knows
nothing about is the kind of thing you notice three tool calls later, when a
relative path or a project instruction file has quietly gone missing.

So every resume goes through [agfi:h-agent-session-resume-run], which runs the
agent's launcher in the directory the session was working in. That directory
comes from [agfi:h-agent-session-dir], and it is read rather than guessed:
every agent records the cwd in its transcript and `agent_session <agent> meta`
prints it as the third field. Behind that sit two fallbacks — Claude Code's
project directory name, which encodes the starting directory with every
non-alphanumeric character replaced by a dash and is therefore accepted only
when the inversion is really a directory, and then whatever the caller offers.

`agent_session_resume_cd_p=n` restores the old behaviour, warning instead of
moving, for when resuming somewhere else is the point. The move happens in a
subshell, so an interactive caller is not left in another directory once the
session exits.

`current-id` is also what [agfi:agent-done] leans on: `/done` is one skill
shared by all three agents, and the only agent-specific thing it needs is which
session it is in. See `agent-done.md`.

### Adding a fourth agent

1. A row in [agfi:h-agents-table]: token, label, glyph, the binary names its
   process runs under, and its launcher function.
2. A `<agent>-session.zsh` with the verbs above, plus the everyday aliases,
   which are one line each because they are the shared commands with
   `agent_session_agents=<token>` in front.
3. A Go package `internal/<agent>` satisfying `session.Adapter`, and one line
   each in `agentNames` and `adapters` in `main.go`. See
   `golang/agent_session/readme.org`.
4. A hook that calls `agent-session-register <token>`, if the agent has hooks.
5. A line in [agfi:h-agent-skills-dirs] naming where it reads user skills, so
   the shared `/done` skill reaches it too.

Nothing else. The pickers, the previews, the band, the toggle, the reaper and
the resolver are already agent-neutral, and `go install` is the step people
forget: the zsh side calls the installed binary, so a new adapter that is not
installed shows up as an agent whose rows are simply missing.

## Where each agent keeps its sessions, and how liveness is decided

**Claude Code** writes one JSONL per session under
`~/.claude*/projects/<mangled-cwd>/<uuid>.jsonl`, one projects directory per
profile. Liveness comes from Claude Code's own records: a running session
writes `~/.claude*/sessions/<pid>.json` -- pid, session id, name, cwd, kind,
busy/idle status, the tmux location it launched in -- and removes it on exit.
The Go side reads those files and drops any record whose pid is no longer
alive, which is the one thing the file cannot say for itself after a crash.
Measured against `claude agents --json`, the two agree exactly, minus the
CLI's finished background agents that carry no pid.

**Codex** writes a rollout per thread at
`~/.codex/sessions/YYYY/MM/DD/rollout-<local-timestamp>-<uuid>.jsonl`. Names
live in `~/.codex/session_index.jsonl`, which is append-only, so the last line
for an id wins and an empty name clears rather than sets. Liveness is exact
without being documented: a running Codex holds
`~/.codex/thread-writer-locks/<id>.lock` *open*, so one `lsof` says which
process owns which thread. The holder is the native binary, and the pid kitty
can see is the `node .../codex.js` launcher above it, so the adapter walks up
to the topmost Codex ancestor. The ChatGPT desktop app holds these locks too,
which is exactly why that walk is not optional.

**Antigravity** gives each conversation a directory,
`~/.gemini/antigravity-cli/brain/<id>/`, with the readable record at
`.system_generated/logs/transcript_full.jsonl` and a truncated
`transcript.jsonl` beside it. Titles live in `conversation_summaries.db`,
mirrored into `cache/conversation_metadata.json`. Liveness is the weakest of
the three: a running `agy` is paired to a conversation through its cwd and
`cache/last_conversations.json`, the same map agy itself uses to resume in a
directory. That is right whenever a workspace hosts one conversation at a time,
and when it does not, the hooks' record on the tmux session settles it.

The rule all three follow is that a live row names only a process that was
really traced. A session that cannot be paired to one gets no row rather than a
plausible pid, and a session with a pid but no tmux session carries `-` in that
column, which no window name can match. This is load-bearing: the resolver
matches windows against these rows, so an invented pid would not show up as a
miss, it would open the wrong transcript silently. A missing row opens the
picker instead.

The reading itself is all in one Go binary, run as
`agent_session <agent> <subcommand>`, whose `readme.org` covers the record
models, the org and markdown writer, the parallel pandoc path and the
performance work. The zsh side keeps the policy; Go does the work.

## Picking a tmux session that runs an agent

[agfi:fftmux-agent], `ffta`, is [agfi:fftmux] narrowed to the sessions that
have an agent in them, with the session pickers' rich preview attached. You get
one row per live agent session that sits in a tmux session, showing the tmux
name, the agent's own name for the conversation when it adds anything, the
glyph of which agent it is, when the conversation last moved and its first
prompt; picking one hands its session *id* to the engine, which by default is
[agfi:tmux-session-goto].

The rows come from [agfi:h-agent-session-tmux-rows], over
[agfi:h-agent-session-live-list] rather than over `tmux ls`. A tmux session
runs an agent exactly when a live agent process sits in it, and that is not
something a name can tell you: an autoname hook may not have reached the
session yet, and a session named after an agent may be one the agent has since
left. The live listing already knows, because it traced the process.

It prints ids, `$374` and not `+Claude/default whatever`, for the reasons in
`docs/tmux-session-rename.md`: a name beginning with a sigil is not a usable
`-t` target, and the autoname hooks rename on every prompt, so a name captured
during the pick can be gone by the time you act on it. [agfi:h-fftmux-act],
which `fft` and `ffta` share, takes the id from the first column and asks tmux
for a name only to say what it is doing.

That staleness bit the tmux column itself for a while. Claude Code records the
tmux session it launched in, in `sessions/<pid>.json`, and the live listing used
to report that name -- so it aged exactly as fast as the hooks renamed things.
Measured while writing this: five of seventeen live sessions named a tmux
session that no longer existed, including the one doing the measuring.

The listing answers with the session the process *sits in* now. `tmuxOf` in
`golang/agent_session/internal/claude/live.go` walks the agent's pid up its
parents to the tmux pane holding it, over one `ps` and one `tmux list-panes -a`
that the whole run shares (`proc.ListShared`, `proc.PanesShared`), which is what
the Codex and Antigravity adapters already did. A pane does not get renamed, so
the answer is current whatever the session is called. The record is only the
fallback, for a session started outside tmux, a process the table no longer has,
or a host with no tmux running.

So [agfi:h-agent-session-tmux-rows] does no walking of its own: it maps the
column's name to a session id through one `tmux list-sessions`, takes the label
back off that id so it reads as tmux spells it right now, and skips a name that
matches nothing -- a session that ended between the two calls.

The order is by the last *message*, not by the transcript's mtime: an agent
appends bookkeeping records to a transcript long after the conversation ends,
so mtime floats a session you have not spoken to in hours above the one you
just left. `list` reports
that timestamp, and [agfi:h-agent-session-annotate-rows] sorts on it when
`agent_session_rows_sort` is set: `last` for either side's last message, `user`
for your last one, empty for the caller's own order. `ffta` sets it to `last`
through `fftmux_agent_sort`, and [agfi:fftmux-agent-sort-by-user] is the same
picker with `user`, which is what you want when the question is "where did I
leave off" rather than "what has just finished".

The engine is `fft`'s, so everything that works there works here:
`ftE=(ec) ffta` prints the picked ids instead of going to them,
`ftE=(tmux-session-processes-kill) ffta` kills the session behind a
conversation, and any function taking a session target can stand there.

The preview is the same Go previewer the other pickers use, and it goes compact
on a narrow terminal -- Termux over ssh, a phone-sized split -- by itself: fzf
exports `FZF_PREVIEW_COLUMNS` and `FZF_PREVIEW_LINES` to the preview process,
and the binary reads them, since a zsh knob cannot reach a process fzf spawns
on its own. `agent_session_preview_compact_p` overrides the guess, `y` or `n`,
and its default `auto` is what leaves the decision to the binary.

### The sessions `/done` has ended

[agfi:fftmux-agent-all] (`fftaa`) is the same picker with one more kind of row:
the tmux sessions whose pane the `/done` skill killed, each marked with a
💀 ahead of the agent glyph. Plain `ffta` is unchanged and still means
"sessions running an agent"; the skulls live only in the `-all` spelling, the
way [agfi:agent-clean-all-fz] stands beside [agfi:agent-clean-fz].

They belong in a picker because `/done` does not take the session away. It
leaves the pane dead with the report on screen and a generated
`<report>.pane.sh` in the pane's one command slot, so `prefix-r` brings the
whole conversation back -- see `agent-done.md`. That is a handle to a finished
session, and before this the only way to find it was to remember which tmux
session it had been.

**Picking one only attaches.** It goes through [agfi:tmux-session-goto] like
every other row, and resuming stays `prefix-r` in the dead pane. The split is
deliberate: the report you would be deciding from is on that pane's screen, and
a picker that resumed on your behalf would replace it with a running agent
before you had read it.

The rows cannot come from [agfi:h-agent-session-live-list], because there is no
process left to trace. [agfi:h-agent-session-tmux-dead-rows] asks tmux instead,
with one `list-panes -a -f '#{pane_dead}'`, and keeps only the panes holding an
`sh <report>.pane.sh` under [agfi:h-agent-done-dir]. Being dead is not enough on
its own: `~/.tmux.conf` sets `remain-on-exit` globally, so every pane whose
command exits stays around, and on a working machine most of them are finished
`agent-view` conversions and abandoned shells.

The transcript is read back out of that generated script's resume line, and not
off the report's filename or the tmux session's `@agent_session` option. Both of
those outlive the agent and both are easy to reach, but they record what *was*
in the session rather than what this pane will do, so either could offer a row
whose `prefix-r` resumes something else or nothing at all. The script is what
`respawn-pane` runs, so a session `agent_done_resume_cmd` redirected elsewhere
is simply not offered, and neither is one whose script found no transcript to
resume -- which is the "that can be resumed" filter, arrived at for free.

Undoing the quoting is zsh's own lexer rather than a regex: the path was written
through `${(q)}` inside a `${(qq)}`, so `${(z)}` and `${(Q)}` twice over are the
exact inverse, and a transcript path holding a space or a quote survives it.

Two smaller rules. The 💀 rows are annotated in the same pass as the live ones,
so `agent_session_rows_sort` orders the two against each other and a session you
ended ten minutes ago sits where its last message puts it rather than in a block
at the bottom. And a conversation that was ended here and then resumed by hand
somewhere else is both live and holding a dead pane; it appears once, as the
live row, which is the one you can still talk to.

`tzkill` ([agfi:tmuxzombie-kill]) clears every dead pane on the machine without
looking at what it was, so it throws these away along with the real zombies.
That is not a bug in either -- it is the reason being able to find them first is
worth something.

## Closing the subagents a skill launched

The `tmux-subagents` skill (`~/code/skills/tmux-subagents`) starts child agents
in detached tmux sessions named `ag--<project>--<run>--<lineage>--<model>--<role>`
and registers each one in a JSON file keyed by node id, which is also the
session name. Cleaning them up afterwards is
`zshlang/auto-load/others/agent-subagents.zsh`: [agfi:agent-subagents-list],
[agfi:agent-subagents-reconcile], [agfi:agent-subagents-close],
[agfi:agent-subagents-preview] and the picker [agfi:agent-clean-fz], with
[agfi:agent-clean-all-fz] as the same picker showing busy children too.

The tooling lives here rather than in the skill on purpose. The skill is
public, portable and installed on machines that have none of this; a picker
built on `fz`, the Go previewer and the brish garden would be an undeclared
dependency on a personal scripts checkout. The skill therefore documents the
*procedure* a parent follows, and this is that procedure implemented.

### State is derived on every read, never stored

The registry writes `process_state: running` and `task_outcome: unknown` at
launch and never updates them. On 2026-09-09 two finished Codex children still
read `running`, which is exactly the failure a stored lifecycle field invites:
a second source of truth that disagrees with the first and no way to tell which
one is stale. So nothing here reads those fields, and nothing here writes a
state anywhere. Every row is recomputed, each time, from five things that
cannot lie -- whether the tmux session exists, whether its pane is dead,
whether the task's result file exists *and names this very node*, the newest
line the child's turn-end hook appended to `status.jsonl`, and the agent's own
live listing ([agfi:h-agent-session-live-list]).

The cost of deriving is one fixed set of processes for the whole registry, not
a few per entry: one `tmux list-sessions`, one `tmux list-panes -a`, one `jq`
over the registry, one over the status log, one live listing, and one
`agent_session list` per agent that still has a row to date -- batched with
`-only` the way [agfi:h-agent-session-annotate-rows] batches, so no corpus is
walked.

### The order the states are decided in

`gone` first: the tmux session is not there any more. Then `exited`: the pane
is dead, which `remain-on-exit` keeps around on purpose so the child's last
screen is still readable. Then `done`: a result file exists whose front matter
names this task *and* this node. A result file naming someone else is
`mismatch`, never quietly believed -- each assignment gets a new task id and a
new result path precisely so an old file cannot satisfy a new one. Then
`needs-input`, when the needs-input sibling exists.

Only then is the session considered alive with nothing published, and the
question becomes what it is doing. `busy` when the live listing says so. `-`
there is unknown, not idle: an adapter that reports no status has said nothing
about the agent. Otherwise the child is dated -- newest `status.jsonl` line for
its task, else its transcript's last message -- and it is `stuck` past
`agent_subagents_stuck_after` (600s) and `idle` under it. `unknown` when even
that could not be read.

`stuck` sits in the middle of that ordering rather than at either end because
that is what it means: a live session that has not written a status line, a
result, or a transcript message in ten minutes is usually finished in a way
tmux cannot see -- waiting on a prompt nobody will answer, or retrying an API
forever -- but it is not *known* finished the way a result file is known.

The picker offers them in that order, newest activity first inside each state:
done, mismatch, needs-input, stuck, idle, unknown, busy.

### What the picker will not show you

`gone` rows never reach it, because [agfi:agent-subagents-reconcile] runs first
and forgets them: there is no process to kill and no session to close, so the
entry is all that is left. `exited` rows are skipped too, and that is a
division of labour rather than an omission -- a dead pane is
[agfi:tmuxzombie-kill]'s job, and it clears every one of them in a single pass,
which this picker would only be a slower way to do. Close such a child by
running `tzkill` and then `agent-subagents-reconcile`.

`busy` rows are hidden unless `agent_clean_fz_all_p=y`, so that the fastest
thing anyone does with a multi-select picker -- select all, press enter --
cannot close a child in the middle of a turn.

### The lock, and why the check happens twice

Every writer of the registry holds the stable `agents.json.lock` sidecar across
the whole read-update-replace cycle and publishes through a unique temporary
file in the same directory, then renames. The sidecar, not `agents.json`
itself: that file is replaced on every write, so two writers locking it would
hold locks on two different inodes and both win. [agfi:h-agent-subagents-lock-do]
does this with `zsystem flock` for the same reason [agfi:h-ddc-lock-do] does --
the lock dies with its holder for free -- but it refuses on timeout instead of
proceeding unserialised, because a registry write that races loses an agent.

`flock` is per file descriptor, so a second lock from the same process would
deadlock it against itself. That is why closing a subtree recurses into the
already-under-lock helper and never re-enters the locking one.

A picker's rows are as old as the person reading them, so
[agfi:agent-subagents-close] re-derives the state *inside* the lock before
touching anything, and the picker's own listing is only ever a proposal.

### Closing is verified by pid

Killing the pane proves nothing about what the agent forked. So the close
collects every pane pid of the session and every process below each one
*before* the kill, sends TERM through [agfi:kill-withchildren], waits up to five
seconds for them to go, escalates to KILL, kills the session, and only then
checks the collected pids again. Right after a `kill-session` on 2026-09-09 a
child `claude` was still listed and exited a second later; a session that has
vanished is no evidence its processes have. A failed verification leaves the
registry entry alone and prints the survivors, so a half-closed agent stays
visible instead of becoming a ghost.

Then the entry is deleted and a `closed` line is appended through the skill's
own `tmux-subagent-status.sh`, so that log keeps one writer and one format.
Task directories are never touched: a result file is the record of what the
child did, and closing a terminal is not deleting a report.

### The two knobs that let you do it anyway

`agent_subagents_close_force=y`, or `-f`, closes a `busy` child.
`agent_subagents_close_subtree=y` closes a parent's live descendants with it,
deepest first, each one going through the same verification. Without them a
busy child and a parent with live descendants are both refused with the list of
what is in the way. Neither is on by default because the whole value of the
check is that it happens after the list you were looking at went stale. A child
that is `gone`, `exited` or `done` is not a live descendant and blocks nothing.

## Background sessions

Claude Code can run a session with no terminal: `claude --bg "task"` starts
one, `←` in an interactive session backgrounds the current one, and the agent
view (`claude agents`) lists and attaches them (`claude attach <short-id>`,
`claude logs`, `claude stop`, `claude rm`). Such a session's process runs under
the daemon's pty host, parented to init. Its Bash tool still gets
`CLAUDE_CODE_SESSION_ID` and `CLAUDE_PID`, but no `TMUX_PANE`, and no kitty
window shows it -- only an agent-view TUI that happens to be attached, whose
foreground process is `claude agents`, not the session.

What that means here, and what was done about it:

- **The live list knows them.** The Go adapter reads the session record like
  any other, so a running background session is a live row; a finished one
  has no pid and is dropped. The eighth column, `kind`, says `background`.
  The record's `tmux` field only names where `claude --bg` was *typed*, which
  is not where the conversation lives, so `tmuxOf` leaves the tmux column `-`
  for a background session rather than falling back to it (`live.go`); the
  shell fallback `h-claude-code-session-live-list-sh` does the same. Before
  this, a session backgrounded from inside tmux was listed under a tmux
  session it was not in, and `ffta` and the window resolver acted on it.
- **`ffta` offers them with a 🔌 badge**, `bg:<session id>` in the caller
  column instead of a tmux id, and picking one runs `claude attach` in the
  current terminal through [agfi:h-claude-code-bg-attach], under the session's
  own config home (the personal one means `CLAUDE_CONFIG_DIR` unset, which
  `claude attach` needs to find the id). `agent_session_tmux_bg_p=n` hides
  them. Pane discovery ([agfi:h-agent-session-tmux-panes]) still correctly
  excludes them: there is no pane to type into.
- **Resume refuses a running session.** [agfi:h-agent-session-resume-run]
  now checks the live list and refuses to `--resume` a transcript whose
  session is still running -- two writers on one file, and for a background
  session a second, interactive copy in your pane while the original runs on
  under the daemon. It names the attach command or the tmux session instead;
  `agent_session_resume_force_p=y` goes ahead anyway. The import path had this
  guard already ([agfi:h-agent-session-live-list-offerable]); resume did not.
- **`/done` stops them properly.** [agfi:agent-done] recognises a background
  session through [agfi:h-claude-code-bg-find] and ends it with `claude stop`,
  which keeps the conversation attachable and resumable, rather than by
  signal; and since there is no screen to leave the report on, it sends a
  notification naming the report file. `agent-done-reports` lists it as usual.
- **`claude-daemon-reap` counts them.** Its guard counted sessions by tty, and
  a background session's is `??`; it now asks [agfi:h-claude-code-bg-list] too,
  since `claude daemon stop` takes background sessions down for certain.
- **Reaching one from outside** -- typing `Continue.` into it when a usage
  limit resets -- is the `claude-bg:<id>` target in `agent-usage-armed.md`,
  which attaches a scratch tmux session for the duration.

Still open: kitty-window resolution for an agent-view window works only by
title (strategy 3 above), which `CLAUDE_CODE_DISABLE_TERMINAL_TITLE=1` defeats;
and [agfi:agent-session-register] can record a background session's transcript
against an agent-view window that lists several sessions, since the viewer
counts as an agent process for its guard.

## Hooks, the registry, and trust

The registry is insurance, not the mechanism. [agfi:agent-session-register]
records the one fact a hook has that nothing else does: when a prompt hook
fires, Enter was just pressed, so the kitty window focused right now is the one
showing this session -- whatever the attach mechanism, including ones that do
not exist yet.

Two guards keep the record honest, plus a third for agy. No focused kitty
window means the prompt did not come from one, so nothing is written. A focused
window that resolves to a *different* session means the prompt was injected
while you sat elsewhere, so nothing is written. And a focused window running no
agent at all is not showing this session either, which matters because
Antigravity's `PreInvocation` fires once per model call rather than once per
prompt, by which time the focus may have moved to an ordinary shell.

The wiring, per agent:

- **Claude Code**: `configFiles/claude-code/settings.json`, `SessionStart` and
  `UserPromptSubmit`, still calling the `claude-code-session-register` shim.
  `UserPromptSubmit` also runs `bell-claude-ack`, which takes back the
  session's stored bell notifications; see `docs/bell-auto.md`.
- **Codex**: `configFiles/codex/hooks.json`, the same two events, as a second
  handler object beside the autoname one. Codex trusts handlers *by hash*, so
  every edit to that file makes its handlers untrusted until you accept them
  once in the TUI's startup review. Worse, a handler's non-JSON stdout on exit
  0 is injected into the model's context as extra context, which is why every
  command there ends in `>/dev/null 2>&1`.
- **Antigravity**: `configFiles/antigravity/hooks.json`, under its own
  top-level group `kitty-register`, on `PreInvocation` and `Stop`. That file is
  keyed by group name, and for a non-tool event the handler object goes
  *directly* in the list: wrapping it in a matcher group makes agy discard the
  whole file silently. Each command ends `; echo '{}'`.

All of them go through the brish garden with `brishz_async=y`, so no agent ever
waits on this, and `agents-md-doctor` checks that the symlinks are still
symlinks and not files an app has replaced.

The same files carry the `/auto-continue` kick, `agent-auto-continue-hook`
(`docs/agent-auto-continue.md`): Claude Code on `StopFailure` with matcher
`rate_limit`, Antigravity on `Stop` under its own group `auto-continue`, and
nothing for Codex, whose hooks do not fire on a usage-limit turn.

## Knobs

Dynamically scoped, so they go in front of a call. Every one of them also
honours its old `claude_code_*` spelling, because these functions were the
Claude helpers first: `agent_session_max_block_lines`,
`agent_session_render_jobs`, `agent_session_render_jobs_max`,
`agent_session_diff_p`, `agent_session_subagents_p`, `agent_session_fz_scope`,
`agent_session_fz_subagents_p`, `agent_session_fz_header`,
`agent_session_fz_opts`, `agent_session_live_fz_opts`,
`agent_session_live_fz_no_multi_p`, `agent_session_live_fz_extra_rows`,
`agent_session_live_rows_scope`, `agent_session_live_list_cache`,
`agent_session_preview_bytes`, `agent_session_preview_color_p`,
`agent_session_registry_dir`, `agent_session_kitty_socket_glob`,
`agent_session_resume_scope` and `agent_view_session_prefix`.

`agent_session_resume_cd_p` (default on) is the exception to that list: it has
no `claude_code_*` spelling, because the behaviour it switches off did not
exist before.

These are new and belong to this design rather than to Claude:

- `agent_session_rows_sort` -- how [agfi:h-agent-session-annotate-rows] orders
  what it emits: empty for the caller's order, `last` for the newest message
  first, `user` for the newest message of yours. `fftmux_agent_sort` is the
  same value in `ffta`'s spelling, and defaults to `last` there.
- `agent_session_tmux_dead_p` -- whether
  [agfi:h-agent-session-tmux-rows] also emits the sessions `/done` has ended.
  `fftmux_agent_dead_p` is the same value in `ffta`'s spelling, and
  [agfi:fftmux-agent-all] is that spelling with it on.
- `agent_session_dead_badge` -- the marker those rows carry, `💀`. It goes ahead
  of the agent glyph rather than inside the label, through the optional fifth
  input column of [agfi:h-agent-session-annotate-rows].
- `agent_session_preview_compact_p` -- `y` or `n` to force the preview's
  compact layout, `auto` (the default) to let the binary read the preview size
  fzf gives it.
- `agent_session_agents` -- whitespace separated agent tokens, narrowing every
  picker and resolver. This is the whole implementation of the per-agent
  commands: `codex-resume-fz` is `agent_session_agents=codex
  agent-session-resume-fz`, and the Claude compat names are the same trick.
- `agent_subagents_state_dir`, `agent_subagents_skill_dir`,
  `agent_subagents_stuck_after` and `agent_subagents_lock_timeout` -- the
  cleanup family's own. The first is empty by default rather than resolved at
  load time, so `TMUX_SUBAGENTS_STATE` in the environment still decides, which
  is what lets a test run point one command at a scratch registry.
- `agent_subagents_close_force` and `agent_subagents_close_subtree` -- close a
  busy child, and close a parent's live descendants with it. See the section
  above.
- `agent_session_exclude_pcres` and `agent_session_exclude_p` -- an array of
  PCREs naming sessions no picker should offer, and the switch that turns the
  whole thing off. See the section below.
- `agent_clean_fz_all_p` -- show busy children in [agfi:agent-clean-fz];
  [agfi:agent-clean-all-fz] is that spelling.
- `agent_launch_glyph` and `agent_launch_sync_p` -- the tab-title glyph and
  whether to sync the instruction files, for the shared launcher preamble
  [agfi:h-agent-launch] that the three launchers now share instead of each
  carrying its own copy.
- `agent_launch_decset_rewrite_p`, `<agent>_decset_rewrite_p`,
  `agent_launch_decset_map`, `agent_launch_decset_trace` and
  `agent_launch_echo_p` -- the same preamble's DECSET 1003 downgrade, which
  runs the agent behind a pty proxy so the mouse works on the phone, and
  whether it echoes the command line it runs. See
  `./termux-mouse-decset-1003.md`.

## Sessions the pickers should not offer

Not every live session is a place you would ever want to go. One started for a
side effect and then abandoned -- a Claude Code session opened purely so that it
refreshes an OAuth token, say -- is live, has a transcript, and is never the
answer to "which session did you mean". It is noise in every picker, and worse
in the resume-target picker, where picking it by accident types into a session
nobody is reading.

`agent_session_exclude_pcres` is an array of PCREs for exactly those. A session
is dropped when any pattern matches **either** its own name **or** the tmux
session it sits in. Both, because the two routinely disagree: the autoname hooks
name a tmux session after its agent session, but a session started by hand for a
side effect keeps whatever name the agent gave it, so it can be `tmp-2e` sitting
in a tmux session called `claude-work-refresh-token`, and the only recognisable
string is the tmux one. Set `agent_session_exclude_p=n` to offer everything
again without emptying the list.

Two things about where the filter is applied are load-bearing.

It is **not** applied inside [agfi:h-agent-session-live-list]. A session hidden
from a menu must still count as live everywhere else, or
[agfi:claude-code-session-import] would fork a running session believing it had
quit. Only the picker row builders filter.

And for the kitty picker it is applied to the *pairs*, not to the live listing
they were built from. [agfi:h-agent-session-live-pairs] resolves each kitty
window on its own and consults that listing only as a cache, so a row taken out
of the cache still resolves by the slower route and still reaches the picker --
filtering the cache there looks like it works and does nothing. The tmux picker
builds its rows straight from the listing, so it filters the listing. Because a
pair carries no tmux name, the kitty path looks that name up by transcript.

## The names that were kept

The Claude-only spellings all still work, as `aliasfn`s that set
`agent_session_agents=claude` where the old name meant Claude alone:
`claude-code-view-session` and its `-fz`, `-all-fz`, `-md-fz`, `-md-all-fz`,
`-raw-fz`, `-raw-all-fz`, `-focused`, `-bg` and `-toggle` variants,
`claude-code-view-sessions`, `claude-code-view-reap`,
`claude-code-session-live-fz`, `claude-code-session-register`,
`claude-session-selftest`, the `h-claude-code-session-*` helpers, and
`claude_session` for `agent_session claude`. The profile machinery
(`claude-code-session-import`, `claude-code-session-resume`, the
`claude-resume*` family) stayed Claude-only on purpose: profiles are a Claude
Code idea, and Codex and Antigravity have nothing to map them onto.

`docs/agent-usage-armed.md` covers the usage limit notifier, whose resume
target pickers are built on this family:
`h-agent-usage-continue-targets-kitty-fz` over `agent-session-live-fz`,
spanning all three agents, and `h-agent-usage-continue-targets-tmux-fz` over
`h-agent-session-tmux-panes`, the pane-id sibling of
`h-agent-session-tmux-rows` that walks each live agent pid to the pane holding
it. Where each agent's reset time comes from is in
`docs/claude_code_usage.md` and `docs/codex_status.md`.
`docs/tmux-session-rename.md` covers the autoname hooks that write the
`@agent_session` option this family reads.

## Checking it by hand

The Go tests are synthetic and run themselves. What is worth pressing after a
change here, in rough order of how much it would hurt to lose:

- `cmd+shift+o` in a bare Claude Code window, behind a `tmux a`, in a Codex
  TUI, and in an agy window.
- The same key on a plain shell window, which should give the overlay picker,
  with `alt+enter` opening a row in the background and a second press
  cancelling it.
- A double-tap of `cmd+shift+o` on an agent window, which must cancel rather
  than start a second conversion.
- `codex-resume-fz` and `agy-resume-fz`, and `agent-session-live-fz` with more
  than one agent running, to see the glyph column and per-row previews.
- `ffta` from inside tmux and from outside it, and `ftE=(ec) ffta` to read the
  ids back; check that a session you renamed a moment ago is still in the list
  and still lands in the right place.
- `agent-session-selftest`, which runs the Go tests and then the pandoc parity
  check over your real transcripts.
- `agents-md-doctor`, after any hook or settings change.

`brishz-restart` after every zsh edit: the garden holds persistent shells and
sees none of it otherwise, and every hook here runs inside the garden.

## What the header says

A converted transcript opens with its name, then a subtitle naming the agent,
the session id and which seat it came from. For Claude Code that is the profile
and the account of that profile, `profile work · someone@example.com`, because
two profiles are two accounts and nothing else in a rendered document says
which one you are reading. Codex reports the signed-in ChatGPT account and plan,
read from the claims of the id token it already stores, and Antigravity reports
the Google account from the file it shares with Gemini CLI. No token is printed
and nothing goes to the network: a document header must not wait on an API.

Each agent answers that through an optional `account` adapter verb, so an agent
that cannot say who is signed in simply contributes no line.

## When a converted transcript opens as octal escapes

If an org file shows `\302\267` where a `·` should be, and the prose around it
is mangled wherever it was not ASCII, the file is not broken: emacs decoded it
as binary. One NUL byte anywhere in a file is enough for that, and tool output
gets one whenever a command printed a binary file. Invalid UTF-8 has the same
effect one step down, giving `Â·` from a latin-1 fallback.

Two things now prevent it. The renderer drops the control bytes that are never
text and turns invalid UTF-8 into a replacement character, and the converter
writes a `-*- coding: utf-8 -*-` cookie as the file's first line, which settles
the encoding whatever emacs would have sniffed. Either one alone is enough.

For a file this pipeline did not write, `inhibit-null-byte-detection` set to
`t` makes emacs stop treating a NUL as a reason to open something as binary,
and `C-x RET r utf-8 RET` re-reads the buffer you already have.
