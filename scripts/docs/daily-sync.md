# daily-sync

`daily-sync` commits and synchronises configured Git repositories across
machines. Run it directly from a terminal or use the scheduled agent workflow.
The scheduled agent groups changes, reviews public diffs for private
information, resolves conflicts and repairs paper links.

Source: `zshlang/auto-load/others/daily-sync.zsh`.

## Run it from a terminal

```sh
daily-sync interactive
daily-sync auto
daily-sync interactive --approve-edits --metadata-update
```

- Both modes commit eligible working-tree edits in one commit per repository
  and machine, then run the usual pull and push passes. Notes include new
  files. Code uses the configured age threshold (seven hours by default) and
  the 100-tracked-change limit. Untracked code files qualify only if a
  tracked file names them. The vcsh repository considers only tracked files.
- `interactive` syncs the other repositories before asking about conflicts.
  Each `fzf` prompt shows the upstream, base and local versions. Use the
  arrow keys and Enter to pick a version or combine them. Esc or **Skip this
  repository** aborts that checkout's rebase or merge. Resolved checkouts are
  synced again.
- `auto` aborts conflicting pulls and skips those checkouts for the rest of
  the run. Other checkouts continue. Both modes exit 1 when work is left
  outstanding, for example because of a conflict, skipped approval, or
  unreachable machine.
- `--approve-edits` asks you to review the selected patch for each public
  repository before committing it. A skipped patch stays uncommitted and the
  repository can still sync existing commits. The flag is off by default, so
  eligible public edits can be committed and pushed without a privacy review.
  This flag needs a terminal and `fzf` in either mode.
- `--metadata-update` refreshes paper metadata in clean local notes after
  syncing. The terminal run commits successful tracked Org updates and
  syncs again if they changed. A failed updater leaves its edits for review.
  The refresh can take hours. It is skipped by default.

Both modes need `flock` on the machine running daily sync. Terminal runs
print their log directory under `~/.agents/runs/daily-sync/`. They share the
configured checkouts with the scheduled workflow and keep their own run state.

## Roles

- The **local machine** runs the job and is the only one that needs this code
  (and Go, to build `golang/daily_sync`: the answer helper and the conflict
  engine).
- **Peers** are other full clones of every repository.
- The **server** has real clones of some notes repositories, read-only clones
  of the public repositories (they only fast-forward there), and **mirrors**:
  private repositories without a remote, which the local machine pushes to
  over ssh.

Peers and the server are driven over ssh with scripts generated locally. They
need `zsh`, `git`, `vcsh` and `git-commit-own`, but not this file, and neither
Go nor perl: the conflict engine is shipped to them (see The conflict engine).
So a change here takes effect everywhere on the next run.

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
  tagged `leave`. An untracked file that a tracked file names (found with `git
  grep`, for up to 20 files per repository) is tagged by age like a tracked one
  and marked `(named in <file>)`: a readme that links a new doc gets the doc
  committed with it. Code repositories list untracked files one by one for
  this; four or more in one directory still fold into a single line.
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
  Conflicts below. `abort` leaves the repository blocked and asks the user on
  agent-desk, one multiple-choice question per hunk (Telegram when agent-desk
  is missing). `reopen` and `abort-quiet` are for `apply-answer`.
- `daily-sync resolve <machine> <repo> <file> <hunk>=<choice>...`: resolves the
  hunks of one file by choice, so the agent never edits files on another
  machine. Once no file is conflicted it continues the rebase, and prints the
  next commit's hunks if that one conflicts too. A rebase the agent leaves open
  is aborted when the metadata updater starts and at `finish`, so none
  outlives the run.
- `daily-sync apply-answer <sidecar>`: run by agent-desk when the user answers
  a conflict question; applies plain picks itself and hands anything else to
  the agent. See Asking the user.
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

### The conflict engine

The classes, the listing and the picks come from `daily_sync conflict
show|resolve`, a Go program in `golang/daily_sync` (`conflict.go`). The prelude
calls it through `ds-engine`, at the path the generated script puts in
`ds_engine`. Every machine runs the local machine's build:

- Before `conflict` (show or reopen) and `resolve`, `h-daily-sync-engine
  <machine>` makes the engine available there. Nothing else calls it, so a run
  without conflicts never ships anything.
- On the local machine it is `daily_sync` from `PATH`, rebuilt by
  `go-local-dep` when older than its source. A build that stays older (a
  failed rebuild) is refused rather than used.
- On another machine, one ssh call reads `uname -s -m`, its home and the
  version of the engine there. When that is not the local build's version, the
  local machine cross-builds a static binary (`CGO_ENABLED=0`; Darwin or Linux,
  arm64 or amd64) into `~/.cache/daily-sync/build/` and uploads it with a
  second ssh call: written to a temporary file, made executable and renamed
  into place as `~/.cache/daily-sync/bin/daily_sync-<version>`. That call keeps
  the three newest binaries there and runs the new one's `version`, which must
  match. The run records what it found in `engine-<machine>` in the run
  directory, so later conflict commands in the run cost no extra ssh call.
- `daily_sync version` prints a hash of the source the build came from (the
  module's `go.mod` and non-test `.go` files, embedded in the binary), the
  same on every platform. To check a machine, compare
  `ssh <machine> '~/.cache/daily-sync/bin/daily_sync-<version> version'` with
  the local `daily_sync version`.
- When the engine cannot be built, reached, uploaded or run, the command
  prints an `ERROR:` line and opens nothing; `apply-answer` treats that as a
  doubt. There is no fallback. If the prelude finds no engine at `ds_engine`
  (the binary was removed mid-run), it prints `ERROR: no conflict engine at
  ...`, and the run forgets `engine-<machine>`, so the next command ships it
  again.

The engine replaced a perl script that ran on every machine, and gives exactly
its outputs. `testdata/corpus` holds 115 conflict files, real ones from staged
repositories and handmade edge cases, each shown and resolved under both
orders with 16 sets of picks; the perl outputs of those 3910 calls are the
goldens that `TestCorpus` checks. The perl is kept as `testdata/reference.pl`
to check or regenerate them (`corpus_test.go` says how).

### Asking the user

A blocked repository becomes one agent-desk question, filed under the project
`daily-sync` and the run `sync-<date>`. Each hunk is a question with the
options `upstream`, `local`, `both` and `both-local-first` (only the first two
for a whole-file conflict); the suggested choice is marked recommended, and the
hunk text is shown in a code block. The answer is one line per hunk
(`N. <choice>: <label>`, `(no answer)`, or `Other: <the user's own words>`),
then any general message.

Next to the question, `h-daily-sync-desk-ask` writes a **sidecar**,
`answer-<machine>-<repo>-<time>.json` in the run directory: the machine, the
repository, the run directory and, for each question in order, the file, the
hunk number, the file's hunk count and the hunk's listing as it was asked. The
question carries `--on-answer`, so when the user answers, agent-desk runs
`daily-sync apply-answer <sidecar>` on the machine that asked, with the answer
on stdin, before any agent hears of it. That runs the Go helper
`golang/daily_sync`, which drives the zsh commands:

1. It reads the answer. It goes on only when every hunk has a plain pick: no
   `(no answer)`, no `Other:`, no `Note:`, no general message, and every line
   one of its question's options.
2. It refuses while a daily-sync job runs (the slow-step job or the metadata
   updater), and takes a lock (`apply-answer.lock` next to the run
   directories) so two answers never run at once.
3. `daily-sync conflict <machine> <repo> reopen` repeats the conflicted pull.
   `reopen` is `show` that refuses a rebase or merge already in progress
   (`ALREADY-OPEN`): someone else's. The hunks must match the sidecar exactly:
   the same files, hunk numbers, counts and text.
4. `daily-sync resolve` per file with the picks; the last file continues the
   rebase or merge and must print `RESOLVED`.
5. `daily-sync converge`, as a slow step that it waits for as long as the
   desk's 10 minutes allow. If it is still running then, the step job reports
   to the run's agent as usual.

Every zsh call runs with `daily_sync_run` set to the sidecar's run directory,
so an answer given on a later day logs to the run that asked. On any doubt
(an answer that is not plain picks, a hunk that changed since the question, a
pull that now goes through or fails, an `ERROR`, `CONTINUE-FAILED` or
`NEXT-COMMIT-CONFLICTS` while resolving) it closes what it reopened with
`daily-sync conflict <machine> <repo> abort-quiet`, which aborts without
asking again, and exits nonzero. The desk then hands the answer and the
helper's output to the run's agent, which applies it by hand as before: it
reopens the conflict with `daily-sync conflict`, checks the hunks, resolves
with `daily-sync resolve` and converges. A clean run exits 0 and wakes no
agent; it also exits nonzero when converge still reports the repository as
`CONFLICT`, `BLOCKED`, `PULL-FAILED` or `PUSH-FAILED`.

The output's first line is the verdict the dashboard shows on hover, such as
`Applied 3 pick(s) to <repo> on <machine>; converged.` or `Not applied: <file>,
hunk 2 of 3 changed since the question; handed to the agent.` The full log of
every call is `answer-<machine>-<repo>.log` in the run directory.

Run the helper's tests with `go test ./...` in `golang/daily_sync`. The
end-to-end tests stage real rebase conflicts between two clones of a bare
repository under `$TMPDIR` and drive the real zsh functions with a scratch
configuration and a stub `agent-desk`; `go test -short ./...` skips them. The
shipping tests reach "another machine" through a fake `ssh` that runs the
command locally, with `HOME` pointed at a scratch home and a `PATH` without Go
or perl; it knows one host name and never contacts a real one.

## State

A run keeps its state in `~/.agents/runs/daily-sync/<date>/`: `stage`, `agent`,
the ids of its heartbeats, `converge.log`, `status.log`, `held`, `commits.log`,
`engine-<machine>` (the conflict engine shipped there),
`conflicts.log`, the conflict questions' `hunks-*`, `choices-*` and `answer-*`
files, the updater's `rc-*` files and `DONE`, the pending slow step
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
- `daily_sync_public_notes`: any additional entries from `daily_sync_notes`
  that need approval with `--approve-edits`. The repository at
  `nightNotesPublic` is recognised automatically.
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
