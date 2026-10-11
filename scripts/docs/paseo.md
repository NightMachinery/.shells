# Move a terminal session to Paseo

Install or update the CLI with [agfi:paseo-install]:

```zsh
paseo-install
paseo-install 0.10.2                 # optionally choose a version
npm-install @getpaseo/cli            # uses pnpm when available
```

The installer uses [agfi:npm-install], which selects pnpm when available and
npm otherwise. It allows the native dependency builds for `esbuild`,
`msgpackr-extract`, and `node-pty`. See
[global npm installation issues](npm-global-installs.md).

When migrating an active standalone daemon from npm to pnpm, install its current
version first and put `PNPM_HOME` on `PATH`, using pnpm's own shim directly.
Do not symlink that shim into another directory: its paths are relative to its
location. Keep the old npm
package until the daemon has been restarted from pnpm in a planned maintenance
window. Installing or replacing the CLI alone does not move a running daemon.

To install the desktop app alongside the package-manager CLI:

```sh
brew install --cask --no-binaries paseo
```

The cask normally links its bundled CLI as `paseo`, conflicting with the
existing binary. `--no-binaries` installs `/Applications/Paseo.app` without
replacing that link. The desktop's bundled CLI remains available at
`/Applications/Paseo.app/Contents/Resources/bin/paseo`.

## Official agent skills

The upstream [`paseo` skill](https://github.com/getpaseo/paseo/tree/main/skills/paseo)
and its `paseo-help` companion are installed under
`~/code/skills/paseo/skills/`. [agfi:agent-skills-link] exposes these sources to
Codex, the installed Claude profiles, and Antigravity without copying them.
Use `paseo` for agent/workspace operations and `paseo-help` for product questions
and troubleshooting. These are upstream skills, separate from [agfi:2paseo].

## Terminal handoff

Run this from Claude Code's shell shortcut, or a shell inside Codex:

```zsh
! 2paseo
```

[agfi:2paseo] resolves the current native conversation's exact UUID,
transcript, working directory, and profile. It starts an independent tmux
worker, then returns so the shell operation can finish. The worker prepares
the local Paseo daemon and provider before gracefully stopping the source
process. It imports that same conversation ID and runs `paseo attach` in its
tmux session. Within tmux, the command switches to that session. Elsewhere it
prints the `tmux attach-session` command; the worker still performs the handoff.

The UUID keeps the case it was created in. Claude Code names a transcript after
the session ID it was given, so a session started with macOS `uuidgen` has an
uppercase one, and Paseo opens `<id>.jsonl` exactly as spelled. A mixed-case
ID is refused.

This resumes saved history in a new provider process. It does not adopt the
original process. Source history is retained. Right after the import, the
worker sets the source's model and effort and sends the agent one fixed
briefing, so it resumes on its own (see [Follow-up after import](#follow-up-after-import)).

`paseo attach` streams output and does not provide an interactive coding-agent
prompt. Use the Paseo desktop/mobile client, or another shell for commands such
as `paseo send <agent-id> "continue"` and `paseo permit ls`. Ctrl+C detaches the
output observer. The bundle's `status/result.json` contains the imported Paseo
agent ID, and `status/followup.json` what the post-import follow-up did.
Re-running `python3 "$NIGHTDIR/python/paseo_handoff.py" run
<bundle>/plan.json` attaches that saved result without importing again. If
`status/import-request.json` exists without a result, inspect Paseo before
retrying: the import's outcome is unknown, so automatic replay is refused.
If already inside a Paseo agent, `2paseo` opens an observer for that agent
without importing or stopping it.

## Follow-up after import

`prepare` reads the model and effort the source session last used from its
transcript, and stores them in the plan as `model` and `effort`:

- Claude: the model comes from the latest `model` attachment
  (`attachment.identity.modelId`), which keeps a context suffix such as
  `[1m]`. Only a transcript without such an attachment falls back to the
  latest reply's `message.model`, which drops that suffix. The effort is the
  latest reply's top-level `effort`. Synthetic replies are skipped, and a real
  reply without an effort (Haiku's) means none applies.
- Codex: the latest `turn_context` record's `payload.model` and
  `payload.effort`.

A value that is missing, or does not look like a model id or a known effort
level, becomes null with a warning. It never blocks the handoff.

After a successful import and before attaching, the worker runs three steps:

1. When the model is known, it sets it with `python/paseo_set_model.mjs`. The
   CLI has no model flag, so this helper loads the installed `@getpaseo/cli`
   package, found through the `paseo` executable or its pnpm shim at run time
   rather than a fixed version path. It connects the way
   `paseo agent update --home` does and calls the daemon client's
   `setAgentModel`. The daemon stores a model without checking it, and a wrong
   one can break the conversation, which is why only the source's own model
   is ever sent.
2. When the effort is known, it runs
   `paseo agent update <id> --thinking <effort> --home <home>`. The model goes
   first because Paseo checks an effort against the agent's current model.
   `auto` is skipped: no provider accepts it, and it means the default.
3. It always sends one fixed briefing with
   `paseo send --no-wait --prompt-file`. The briefing tells the agent that it
   now runs in Paseo and that the original process was stopped, and asks it to
   repair what the move broke before continuing: scheduled tasks and cron jobs
   (recreated as Paseo schedules or heartbeats), in-process subagents and
   background shells, stopped workflows, monitors, holds and screen banners.
   It then asks the agent to confirm its model and effort with the
   `paseo-model-update` skill, and says which of the first two steps
   succeeded. After a move to another account (`--to`, below), it also says
   so, and with `--skip-subagents` that the subagent transcripts were not
   imported and where they remain. The text is kept in
   `status/followup-prompt.txt`.

Each step is best effort. A failure is recorded and the next step still runs;
none of them fails or rolls back the handoff. `status/followup.json` records
each step's value and outcome: `ok`, `failed` with a generic reason (CLI output
stays withheld), or `skipped`. A re-run that finds a saved result but no
`followup.json` does the follow-up once before attaching. Once `followup.json`
exists, a re-run only attaches. A bundle prepared before this follow-up existed
has no `model` key in its plan and is never briefed, so re-attaching to an old
agent sends it nothing.

To check that the helper can reach the local daemon without changing anything:

```sh
node python/paseo_set_model.mjs --paseo "$(command -v paseo)" --home "$HOME/.paseo" --check
```

## Moving to another account

Each Claude account is a config directory, such as `~/.claude` and
`~/.claude-work`. Without options, `2paseo` imports a session into the account
it came from. `--to` moves it to another one instead, for example to spend a
quota that is about to reset:

```zsh
! 2paseo --to ~/.claude
! 2paseo --to=~/.claude --wait-exit --no-switch
```

The value is an absolute path or a `~/` path (a quoted `~/` is expanded too).
It must be an existing Claude config directory with a `projects/` folder,
other than the session's own and not sharing its `projects/`. Only Claude
sessions can move; `--to` is refused for Codex, and inside a Paseo agent.

The destination is the source's own layout under the target:
`<target>/projects/<project folder>/<id>.jsonl`, plus the session's sidecar
directory `<id>/` beside it (subagent transcripts, tool results, workflow
state). Paseo finds the copy there because it derives the same project folder
name from the same working directory.

`prepare` refuses, before anything is stopped, when either destination path
already exists, or when the target has no usable provider entry. It resolves
the target's provider exactly as it would for a source home: the daemon's
default Claude config (`~/.claude`) gets the built-in `claude` entry, and any
other directory gets or reuses its `handoff-claude-<hash>` alias. The provider
diagnostic must report the target as the entry's config directory. The worker
repeats both checks before it stops the source, since either can change in
between.

### The copy

The worker copies once the source has exited and the checks that no writer
remains have passed, so the copy is the final transcript, and before it
imports:

- Every file is created exclusively, so nothing is ever overwritten. Files get
  mode 0600 and directories 0700, like the bundle's private files, and files
  keep the source's times.
- The transcript is checked byte for byte against its source by SHA-256.
- Symbolic links and special files in the sidecar are skipped and counted, not
  followed.
- Project memory (`projects/<project folder>/memory/`) belongs to the account
  and is not copied.
- The source transcript and sidecar are never modified or deleted.

With `--skip-subagents`, the copy leaves out `<id>/subagents/`, the subagent
transcripts (see [Large sessions](#large-sessions)); everything else in the
sidecar is copied as usual.

`status/copy.json` records both paths, the transcript's size and SHA-256, and
the sidecar's file, directory, byte and skipped counts. With
`--skip-subagents` it also has `skipSubagents: true` and, under
`sidecar.skippedSubagents`, the path, file count and bytes left out. The import then runs
with the target's provider, and `status/import-request.json` names the copied
transcript.

### What changes for the agent

Model and effort follow as for any handoff. Everything that belongs to the
account does not: memory directories, account-level instructions such as a
profile overlay `CLAUDE.md`, connectors and MCP servers, installed skills,
settings and hooks, and quotas. The briefing tells the agent it moved from
the source config directory to the target, and asks it to re-read its project
instructions and check that the tools and skills it relies on are present.

### When the copy fails

A failed copy removes only what it created, imports nothing, and names both
paths in its error. By then the source has stopped, so resume the original
natively in its own account with the printed recovery command, and run
`2paseo --to` again from there if you still want to move. Its transcript is
exactly as it was.

Ctrl+C in the worker's pane cleans up the same way. Only a worker killed
outright in the middle of a copy (by a signal, or with its tmux session) leaves
its partial destination behind. `2paseo` will not overwrite it: inspect it,
and delete it yourself before retrying.

A re-run of the same plan after a completed copy neither stops nor copies
again. It checks that no writer has appeared and that both transcripts still
match the recorded SHA-256, then imports. An import whose outcome is unknown
(`status/import-request.json` without a result) is never retried, as for any
handoff.

The original transcript always stays where it was. Resuming it natively after
the move works, but forks the conversation: Paseo continues the copy in the
target account.

## Large sessions

To load a Claude session, Paseo 0.10 reads these files, each whole into memory
as a string, and then parses them:

- the transcript, `<id>.jsonl`;
- every `*.jsonl` and `agent-*.meta.json` at any depth under `<id>/subagents/`,
  which holds the subagent transcripts, including the workflow agents' under
  `subagents/workflows/<run>/`;
- every `*.json` directly in `<id>/workflows/`.

It does this at import, and again whenever it resumes or reloads the agent:
after a daemon restart, a settings change, or reopening an archived agent. A
long session with many workflow runs can hold far more than the daemon's heap
allows. One with a 21 MB transcript and 745 MB of subagent transcripts in 314
files crashed the Paseo 0.10.2 daemon ("Reached heap limit", at about 3 GB)
during `paseo agent import`, which stops every Paseo agent with it. The tool
results under `<id>/tool-results/` are not read.

So `prepare` measures what Paseo would read, for every Claude handoff with or
without `--to`, and refuses before anything is stopped when it exceeds the
limit, 200 MB (decimal megabytes) by default. The refusal states the sizes of
the transcript, the subagent transcripts (with their file count) and the
workflow files, and prints the commands to run instead. The worker measures
again before it stops the source and once more after the source has exited,
because the session can keep growing in between (`--wait-exit` leaves it
running for up to ten minutes). A refusal after the exit imports nothing and
prints the native recovery command, like any failure at that point. A Codex
session is not measured: Paseo's Codex provider reads no sidecar.

```zsh
! 2paseo --to ~/.claude --skip-subagents     # leave the subagent transcripts behind
! 2paseo --max-import-mb 900                 # the daemon has more heap
```

`--max-import-mb <n>` raises the limit. Use it only when you know the daemon
has the heap for it (Node's `--max-old-space-size`).

`--skip-subagents` leaves the subagent transcripts out of the import. Claude
Code resumes the conversation from the main transcript alone; the subagent
transcripts only feed Paseo's history view, which then lacks the subagents'
detail. The briefing tells the agent so, and where the originals are.

`--skip-subagents` works only with `--to`. The copy in the target account
leaves `<id>/subagents/` out, the originals stay where they are, and since
Paseo only ever reads the copy, they stay out of every later reload too.
Within one account Paseo reads `<id>/subagents/` where it lies, so 2paseo
would have to move the user's files to keep them out. A temporary rename for
the duration of the import was considered and rejected: the next reload of the
agent, which can be the model update right after the import, would read them
again and crash the daemon anyway. So 2paseo refuses `--skip-subagents`
without `--to` and says why.

If you cannot move accounts, you can set the folder aside yourself, knowing
that Paseo then never shows those subagents: after the session has exited,
`mv <id>/subagents <id>/subagents.set-aside` in its project folder, and run the
handoff from a fresh `claude --resume`. Renaming it back later makes Paseo
read it again on its next reload.

## Controls and recovery

```zsh
! 2paseo --no-switch                 # leave the current tmux client in place
! 2paseo --wait-exit --no-switch     # queue import, then exit the source yourself
! 2paseo --to ~/.claude              # move to another account (see above)
! 2paseo --to ~/.claude --skip-subagents   # ... without the subagent transcripts
! 2paseo --max-import-mb 900         # raise the import size limit (see Large sessions)
```

Automatic termination requires a source process belonging to the current user,
in the invoking shell's ancestry, with matching live-session identity. A shared
process owning other sessions is refused. A background session is refused.
When the live listing identifies a `decset-rewrite` proxy or a Node launcher,
the handoff resolves the native executable on the caller's ancestry. It retains
the launcher's identity for the live-session checks and signals the native
process, so a surviving child cannot keep writing the transcript. Both native
`claude` and the pnpm-installed `claude.exe` are recognized.
The worker uses SIGTERM for the exact validated PID, checks its start time to
avoid signalling a reused PID, and waits for exit. Source termination never
escalates to SIGKILL.
Failure before termination leaves the source running; failure afterwards prints
the native UUID for recovery. An import is never attempted while another process
still owns that transcript.

`--wait-exit` sends no signal and waits up to ten minutes. This is useful when
you want to end the source yourself. Automatic shutdown waits up to thirty
seconds. Both modes allow five seconds for exited native-writer records to
clear, and refuse import if a writer persists.

The selected daemon is local. `paseo_home` or `PASEO_HOME` chooses its home;
inherited `PASEO_HOST` is overridden. Profile aliases bind nondefault Claude
configuration directories and Codex homes to provider environment settings.
Aliases are merged into the local daemon's configuration atomically, preserving
existing settings. A conflicting alias or concurrent configuration edit is
refused before source termination.
Default Claude imports verify the daemon's default configuration directory.
The daemon cannot obtain an account profile merely from the import client's
environment. Model and effort follow the source session when its transcript
records them (see [Follow-up after import](#follow-up-after-import)).
Permission mode still follows the provider entry's default mode
(`paseo provider ls`), and other launch overrides of the original process are
not restored.

Private plans and results live under
`${XDG_STATE_HOME:-$HOME/.local/state}/agent-handoffs/paseo.*`.
`paseo_state_dir` overrides that parent directory. They contain paths and IDs,
not copied credentials, and remain for inspection and recovery. Worker output
stays in the named tmux session.

The implementation uses Paseo's [native import command](https://github.com/getpaseo/paseo/blob/main/packages/cli/src/commands/agent/import.ts)
and [provider profiles](https://paseo.sh/docs/custom-providers).
Terminal attachment is an [output observer](https://github.com/getpaseo/paseo/blob/main/packages/cli/src/commands/agent/attach.ts).

Provider labels can be customized in `agents.providers` in the private Paseo
configuration, followed by `paseo reload`. Handoffs preserve custom labels while
still checking that the provider's profile configuration matches.

When an imported provider duplicates a built-in entry, check both resolved
binaries, configuration homes, and active/archived session references before
consolidating. Existing conversations retain their provider entry ID. Preserve
an entry that serves a live conversation rather than deleting it or rewriting
the conversation's provider in stored state.

An enabled custom entry may extend a disabled built-in provider. Keeping the
imported entry, giving it a normal display label, and setting the duplicate
built-in entry's `enabled` to `false` leaves one enabled choice while preserving
resume compatibility. Save a private configuration backup, use `paseo reload`,
then verify provider availability and the live conversation. Provider changes
can apply without a daemon restart; unrelated restart-required paths reported
by reload are a separate issue.

Run the isolated checks with:

```sh
zsh -f zshlang/tests/paseo.zsh
python3 -m unittest discover -s python/tests -p test_paseo_handoff.py
```

The Python suite drives every external CLI through fakes that only log their
arguments. It also runs the model helper against a fake `@getpaseo/cli`
package, and skips those helper tests when Node.js is missing. The size
preflight tests use sparse files, so their hundreds of megabytes take no disk
space and are never read.
