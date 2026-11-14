# daily-sync

`daily-sync` is the mechanical half of a scheduled agent job that commits and
synchronises a set of git repositories across several machines every day, and
then refreshes paper metadata in the notes. The agent keeps the parts that need
judgement: grouping changes into commits, checking public repositories for
private information, resolving trivial conflicts and repairing paper links.
Everything else is a command here, so a whole run is about ten tool calls
instead of ninety, and a weaker model can carry it.

Source: `zshlang/auto-load/others/daily-sync.zsh`.

## Roles

- The **local machine** runs the job and is the only one that needs this code.
- **Peers** are other full clones of every repository.
- The **server** has real clones of some notes repositories, read-only clones
  of the public repositories (they only fast-forward there), and **mirrors**:
  private repositories without a remote, which the local machine pushes to
  over ssh.

Peers and the server are driven over ssh with scripts generated locally. They
need `zsh`, `git`, `vcsh` and `git-commit-own`, but not this file, so a change
here takes effect everywhere on the next run.

## Commands

Each command prints a `NEXT:` line saying what to run afterwards, so the agent
only has to follow them from `begin` to `ALL DONE`.

- `daily-sync begin`: arms a watchdog heartbeat, exports the Paseo schedule
  definitions on the Macs, and prints the status of every repository on every
  machine.
- `daily-sync status`: the status alone. Repositories that are clean and in
  sync are folded into one `clean:` line. In code repositories each changed
  file is tagged `commit-ok` or `too-new(Nh)`, by whether it was last modified
  at least `daily_sync_min_age_hours` (default 7) ago, and untracked files are
  tagged `leave`.
- `daily-sync git <machine> <repo> <git arguments...>`: git in one repository
  on one machine, with the quoting handled. It refuses the verbs and flags the
  job must never use (`reset`, `stash`, `checkout`, plain `commit`, forced
  pushes, `--amend`, ...), and allows `add` only while a rebase or merge is in
  progress.
- `daily-sync hold <machine> <repo> <file> <reason>`: records a file held back
  by the privacy check and sends one notification naming only the repository
  and machine.
- `daily-sync converge`: pull and push everywhere, in an order that leaves every
  machine with everyone's commits: local, peers, server, local again, then a
  fast-forward on the peers and the server, then the mirror pushes. A pull is a
  rebase, or a merge when tracked files are still modified. A conflict is
  aborted and reported as `CONFLICT`. It clones onto a peer a repository the
  peer lacks, and creates a missing mirror.
- `daily-sync conflict <machine> <repo> [abort]`: repeats the conflicted pull on
  that machine and keeps it open, then prints every conflicted hunk as three
  parts (upstream, base, local) with a class and a suggested choice; see
  Conflicts below. `abort` leaves the repository blocked and notifies.
- `daily-sync resolve <machine> <repo> <file> <hunk>=<choice>...`: resolves the
  hunks of one file by choice, so the agent never edits files on another
  machine. Once no file is conflicted it continues the rebase, and prints the
  next commit's hunks if that one conflicts too. A rebase the agent leaves open
  is aborted when the metadata updater starts and at `finish`, so none
  outlives the run.
- `daily-sync metadata start`: starts `org-paper-metadata-updater` over the
  clean notes repositories as the tmux job `daily-sync-metadata`. When it
  finishes, the job runs the review that `daily-sync next` would print and
  sends it to the agent through `paseo send`. An hourly heartbeat is the
  fallback.
- `daily-sync next`: where the run stands and what to do. While the updater
  runs it says so; once it is done it prints exit codes, counts, today's
  failures sorted by cause (with the number of earlier daily logs showing the
  same failure), and the changed files, after fixing the trailing whitespace
  the updater leaves.
- `daily-sync metadata rerun <directory>...` and
  `daily-sync metadata commit <repo> [<held-back file>...]`.
- `daily-sync finish`: a last `converge`, then removes the heartbeats. If that
  converge finds a conflict for the first time (a peer whose earlier pull
  failed, say), the run stays open once: the NEXT line leads to `daily-sync
  conflict`, and the next `finish` ends the run whatever is still open.

## Slow steps

The agent harness cuts a command off after about 30 seconds, and a model
handles a cut-off command badly: in the first test it never saw the output.
So `begin`, `status`, `converge` and `finish` run as the tmux job
`daily-sync-step`. A call waits up to `daily_sync_step_wait` (default 20)
seconds. Past that it prints `WORKING` and the agent ends its turn; the job
sends the step's output to the agent through `paseo send` when it is done,
so the agent never polls. Exactly one of the two delivers it, whichever
creates `step.claim` first. If the send fails, the next `daily-sync next`
prints the output. Outside Paseo the call prints `STILL WORKING` and says to
run `daily-sync next` instead. Set `daily_sync_inner=y` to run a step in the
foreground.

The agent is the caller's `PASEO_AGENT_ID`; `begin` records it in the run
directory for the jobs. A run finished by another agent is not resumed: `next`
tells a new agent to `begin`, which sets the finished run aside as
`<date>-done-<time>`.

Every command first checks for the access it needs. Inside Codex's sandbox the
run directory, tmux and ssh are blocked, so the command does nothing, prints
`NOT ELEVATED` and says to rerun it with elevated access. Without this check, a
sandboxed `begin` could not set the earlier run aside, and the new run then
reported the earlier run's ALL DONE as its own.

## Updates are self-contained

The model this job is meant for keeps poor track of earlier turns. So every
hint, whether printed or sent as a message, ends with the run so far (commits
made through `daily-sync git ... commit-own`, files held back, conflicts
resolved or left blocked, unreachable machines), the five steps of the run
with the current one marked, and the `NEXT` line. The reports are written
from that, not from memory.

## Conflicts

`daily-sync conflict` asks git for `zdiff3` markers, so each hunk carries its
base. The classes:

- `SAME-IGNORING-WHITESPACE`: the two sides differ only in whitespace; take
  upstream.
- `UPSTREAM-ONLY-WHITESPACE`, `LOCAL-ONLY-WHITESPACE`: one side changed only
  whitespace; take the other side.
- `BOTH-ONLY-ADDED`: neither side changed or deleted a base line; take `both`.
- `ONE-EDITED-OTHER-ADDED`: one side changed or deleted base lines, the other
  only added; `both` is still right.
- `BOTH-EDITED`: both sides changed or deleted base lines; a judgement call.
- `BASE-NOT-SHOWN`: the markers carry no base; a judgement call.

`both` is upstream's lines, minus the base lines local deleted, followed by
the lines local added. A line local added counts as already present only when
it matches as part of a run that holds a real line. A blank line, a short
line or bare org markup such as `#+begin_verse` proves nothing alone, so a
copy of it elsewhere does not swallow local's. `both-local-first` puts local's
additions first. The result is never lossy, but can be untidy: an extra blank
line, or a stub left by an editor in a file both sides created.

## State

A run keeps its state in `~/.agents/runs/daily-sync/<date>/`: `stage`, `agent`,
the ids of its heartbeats, `converge.log`, `status.log`, `held`, `commits.log`,
`conflicts.log`, the updater's `rc-*` files and `DONE`, the pending slow step
(`step.*`), and `stderr.log` with the stderr of every generated script. Set
`daily_sync_trace=y` to add an xtrace to that log.

The watchdog heartbeat fires every ten minutes while the agent should be
working and tells it to run `daily-sync next`. A model that ends its turn early
therefore loses minutes, not the run.

## Configuration

Which machines and repositories are synced is private, so it lives outside this
repository, in the file `daily_sync_config` points at (by default
`${nightNotesPrivate}/configs/daily-sync/config.zsh`). Repositories are named
relative to `~`; `NIGHTDIR` is the vcsh repository rooted at `~`.

- `daily_sync_peers`, `daily_sync_server`: ssh names.
- `daily_sync_notes`, `daily_sync_private_code`, `daily_sync_public`: the
  repositories, by kind.
- `daily_sync_server_notes`: the notes the server has real clones of.
- `daily_sync_server_no_mirror`: private repositories that never go to the
  server.
- `daily_sync_extra_push`: entries `repo remote[:branch] [machine]`, a second
  remote to push to, from one machine only when one is named.
- `daily_sync_metadata`: repositories the updater runs over (default: the
  notes).

The scheduled prompt is kept as `prompt.md` next to the configuration. After
editing it, apply it with
`paseo schedule update <id> --prompt "$(<prompt.md)"`.
