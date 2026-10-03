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
original process. Source history is retained. The importer sends no task prompt;
continue the conversation through Paseo when ready.

`paseo attach` streams output and does not provide an interactive coding-agent
prompt. Use the Paseo desktop/mobile client, or another shell for commands such
as `paseo send <agent-id> "continue"` and `paseo permit ls`. Ctrl+C detaches the
output observer. The bundle's `status/result.json` contains the imported Paseo
agent ID. Re-running `python3 "$NIGHTDIR/python/paseo_handoff.py" run
<bundle>/plan.json` attaches that saved result without importing again. If
`status/import-request.json` exists without a result, inspect Paseo before
retrying: the import's outcome is unknown, so automatic replay is refused.
If already inside a Paseo agent, `2paseo` opens an observer for that agent
without importing or stopping it.

## Controls and recovery

```zsh
! 2paseo --no-switch                 # leave the current tmux client in place
! 2paseo --wait-exit --no-switch     # queue import, then exit the source yourself
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
environment. Model, permission mode, and effort may follow Paseo provider
defaults; every original launch override is not automatically restored.

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
