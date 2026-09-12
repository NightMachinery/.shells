# Agent Session Zsh Plugin

Portable exact-conversation resume and tmux identity helpers. This directory is
shared by the local shell and the tmux-subagents skill; it does not load the
personal shell, profile tables, pickers or the full basic stack.

## Dependencies and loading

Requires zsh; managed panes also need Python 3 and tmux. Authenticate the provider
CLI separately. Loading the plugin only defines functions.

```zsh
export AGENT_SESSION_PLUGIN=/path/to/.shells/scripts/zshlang/plugins/agent-session/agent-session.plugin.zsh
source "$AGENT_SESSION_PLUGIN"
```

Local loading is registered explicitly in `zshlang/load-others.zsh`. The loader
is idempotent and resolves its implementation relative to itself.

## Commands

```zsh
agent-session-resume-exact claude UUID /project claude-work --model opus
agent-session-resume-exact codex UUID /project codex --model MODEL
agent-session-resume-exact agy UUID /project agy --model MODEL
agent-session-register-current claude # or codex / agy; inside the child
agent-tmux-identity-get              # optional explicit pane ID
```

`agent-session-resume-exact PROVIDER ID CWD LAUNCHER [ARGS...]` restores the
specified directory and invokes the chosen launcher with the provider's exact-ID
resume syntax. It does not select a profile or search for a conversation. Pass
profile settings and desired permission/model flags explicitly. The local
transcript-based resume helpers retain their discovery and live-session checks
and share [agfi:h-agent-session-resume-argv] with this command.

The direct command does not prevent unrelated callers from opening the same
conversation. The managed pane runner adds a per-pane process lock. Local
transcript-based helpers additionally retain their existing live-list checks.

## Managed panes

The tmux-subagents launcher writes owner-only state: `launch.json` (provider,
cwd, initial/resume shell commands and selected environment), `hooks.json`
(notification argv), and subsequently `identity.json`. The pane option
`@agent_session_state` points at this directory. Run the pane with:

```sh
python3 /path/to/plugin/pane.py run /private/pane-state /path/to/plugin/agent-session.plugin.zsh
```

The first run records a marker and executes the initial command. Later runs
require an exact identity and execute the saved resume command. That command
calls `agent-session-resume-exact` with `$AGENT_SESSION_ID` and
`$AGENT_SESSION_CWD`; saved notification arguments are reapplied automatically.
An inherited lock prevents overlapping runs, waiting at most five seconds for
the previous holder. Missing identity fails visibly without starting fresh.
The runner uses interactive zsh for configured launcher functions, then restores
the captured environment and cwd after startup; standalone users need a minimal
`.zshrc` if their system otherwise opens zsh's first-run setup wizard.

[agfi:h-agent-tmux-identity-set] records provider, ID and transcript in the tmux
session's existing `@agent_session` option and in managed private state. A managed
pane cannot switch to another conversation ID. Claude startup hooks, Codex
notify and explicit child registration can also populate the private state.

Local `/done` retains its report and picker-compatible transcript resume line.
For managed panes it additionally passes `AGENT_SESSION_REUSE_PANE`; the local
resume wrapper validates the transcript ID before restoring the saved command.
An explicit `agent_done_resume_cmd` still takes precedence.

## Plugin-manager examples

Use the same monorepo subdirectory pattern as the Paqet plugin. For Zinit, when
adding to an existing `nightsh` entry, append this entrypoint to its `multisrc`:

```zsh
zinit ice id-as"nightsh" aliases \
  pick"scripts/zshlang/basic/basic.plugin.zsh" \
  multisrc"scripts/zshlang/plugins/agent-session/agent-session.plugin.zsh"
zinit light NightMachinery/.shells
```

The plugin itself needs no basic helpers; `aliases` and the basic entrypoint
preserve compatibility with other plugins loaded in the same entry. Set
`AGENT_SESSION_PLUGIN` to the installed entrypoint for the skill's launcher.
Subdirectory distribution avoids a second implementation and a separate release
repository, but remains an explicit dependency. Nothing downloads during launch.

See `zshlang/plugins/paqet/README.md` for Antidote, Zimfw, Sheldon and Zplug
syntax, substituting `agent-session` for `paqet`.
