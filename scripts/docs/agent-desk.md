# agent-desk on these machines

[agent-desk](https://github.com/NightMachinery/agent-desk) is a store where
coding agents file questions, notices and current-state slots for the user.
Its core runs the commands named in `~/.config/agent-desk/config.toml`, with
the event as JSON on stdin. This repository supplies those commands, in
`zshlang/auto-load/others/agent-desk.zsh`.

## Setup, once per machine

    go install github.com/NightMachinery/agent-desk@latest
    notif-app-install desk          # then allow its notifications in System Settings
    agent-desk-config-install       # writes ~/.config/agent-desk/config.toml

Codex's workspace sandbox would block every `agent-desk` command, because
the store is outside the workspace and SQLite in WAL mode writes even to read.
`configFiles/codex/config.base.toml` therefore lists `~/.agents/desk` in
`sandbox_workspace_write.writable_roots`; [agfi:codex-config-sync] merges it
into `~/.codex/config.toml` at each Codex launch.

The config is generated rather than tracked because it names this machine's
zsh by absolute path: a notification click runs under launchd, where `zsh`
would be Apple's `/bin/zsh`, which lacks the PCRE module our startup needs.

`agent-desk-config-install` also writes `base_url` (the dashboard's address,
so notifications link to the item), `away_command`, `remote_label` and
`ssh_hosts`. Their values are private, so they come from options or
variables rather than this repository: `--base-url` or `agent_desk_base_url`
(else the desk app's address from html-reports' `apps.py`, where that is
installed), `--remote-label` or `agent_desk_remote_label` (default
Telegram), and `--ssh-host HOST=TARGET` or the associative array
`agent_desk_ssh_hosts`. Rerun it with `--force` after changing them.

## What each command does

The core decides which channels an event goes to (the user's `notify.desktop`
and `notify.remote` settings) and calls `notify_command` once per channel,
with `channel` in the payload. Whatever the command prints is the handle the
core keeps, and hands back to `dismiss_command` when the item is read or
answered.

- [agfi:agent-desk-notify] (`notify_command`):
  - `desktop`: one Agent Desk notification per item. Its group is the event
    id, so one question never replaces another, and the group is the handle.
    The title is the item's title; the body is its path and level, plus
    `@host` for an item merged from another machine. All of them respect Do
    Not Disturb, blocked questions included. Clicking runs
    `agent-desk open <id>`.
  - `remote`: one rich Telegram message through [agfi:tsend-tracked]: the
    item's title, its path, what it is, the agent's title, a link to its
    page on the dashboard, and the body folded under "Details" (cut at
    `agent_desk_tlg_body_max` characters, 3000 by default). It goes to the
    `notify.remote.dest` setting, else to `tlg_notifs`. The handle is the
    destination and the message id.
- [agfi:agent-desk-dismiss] (`dismiss_command`): a desktop card is removed
  with [agfi:notif-os-remove] (by its handle, or by the event id for a card
  raised before handles existed). A Telegram message is deleted with
  [agfi:tsend-retract-handle]; past the 48 hours in which a bot may delete,
  it is edited down to one line such as "✓ answered: Which split?".
- [agfi:agent-desk-away-p] (`away_command`): exits 0 when the user is away:
  the screen is locked, or the keyboard and mouse have been idle for
  `agent_desk_tlg_idle_min_s` (600 s by default). The core asks it for
  `notify.remote = away`.
- [agfi:agent-desk-open] (`open_command`): opens a done report's report file
  with [agfi:h-agent-done-open-report], as Agent Done's card does; else the
  item's page on the dashboard; else a new kitty window running
  `agent-desk inbox <path>`, scoped to the item's node.
- [agfi:agent-desk-task-launch] (`[tasks] launch_command`): starts a Paseo
  agent on a deferred task with `paseo run --no-wait`, outside any agent
  (`PASEO_AGENT_ID` removed), in the task's directory, titled with its id and
  title and labelled `agent-desk.task=<id>`. The task's profile is
  `provider[/model][@thinking]`; without one, `agent_desk_task_profile`, else
  `codex/gpt-6.1-terra@high`. Codex runs in `auto-review` mode. Set
  `agent_desk_paseo` to another binary to test it without launching.
- [agfi:agent-desk-quota-json] (`[tasks] quota_command`): every account's
  usage through the quota-management skill's readers, for
  `agent-desk task nudge`. That command needs a scheduler; the crontab line
  is `20 */3 * * * "$HOME/go/bin/agent-desk" task nudge >/dev/null 2>&1`.
- `[quota] stopped_command` is [agfi:paseo-auto-continue-stopped] (the Paseo
  agents stopped at a usage limit), and [agfi:agent-desk-auto-continue]
  (`auto_continue_command`) lists, registers and unregisters agents with the
  Paseo auto-continue watcher, for the dashboard's "Out of quota" section.
- [agfi:agent-desk-screen-banner] (`[screen] banner_on`, `banner_off`): the
  Hammerspoon agent banner (`agentBannerOn` with the agent's reason and how
  long it holds the screen, and `agentBannerOff`) for agent-desk's screen lock,
  `agent-desk screen take`. The installer writes it only where `hs` exists.
- The prompt hook: `UserPromptSubmit` in Claude Code's `settings.json` and
  Codex's `hooks.json` runs `agent-desk hook prompt`, which keeps the user's
  messages to a session on the desk word for word (`agent-desk
  instructions`). Codex asks to trust the new handler at its next launch.
- `agent-done` lists open blocked questions in its report and files the
  session's end on the desk; see "agent-desk" in [agent-done.md](agent-done.md).

[agfi:tsend-tracked] and [agfi:tsend-retract] are the same pair the bell hooks
use to take back a Telegram message once its session is answered (see
[bell-auto.md](bell-auto.md)).

## The turn-end bell

[agfi:h-bell-agent-hook] does not ring at the end of a turn (Claude Code's
Stop, Codex's turn-complete) for a session that `agent-desk whoami` places on
a desk node, as long as the desk has a `notify_command`: such a session's
questions and done report already notify, so the bell was a duplicate. Hook
events that carry a message, such as a permission request, still ring, and
so does every session not on a node. `agent_desk_bell_on_node=y` brings the
old behaviour back. The check is [agfi:h-agent-desk-on-node-p], which names
the session to `whoami` explicitly, since the hook runs in BrishGarden
without the agent's environment.

The core runs `notify_command` and `dismiss_command` detached, so a slow
notifier never holds up an agent; their output goes to
`~/.agents/desk/hooks.log`.

## Tests

    zsh -c 'source "$NIGHTDIR/zshlang/tests/agent-desk.zsh"'

All transports are inert fixtures.
