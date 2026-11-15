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

The config is generated rather than tracked because it names this machine's
zsh by absolute path: a notification click runs under launchd, where `zsh`
would be Apple's `/bin/zsh`, which lacks the PCRE module our startup needs.

## What each command does

- [agfi:agent-desk-notify] (`notify_command`): one Agent Desk notification
  per question, `tell --notify` and done report. Its group is the event id, so
  one question never replaces another. The title is the item's title; the
  body is its path and level, plus `@host` for an item merged from another
  machine. A blocked question ignores Do Not Disturb; the rest respect it.
  Clicking runs `agent-desk open <id>`.
- A blocked question asked on this machine while the user is away (screen
  locked, or idle for `agent_desk_tlg_idle_min_s`, 600 s by default) also goes
  to Telegram through [agfi:tnotif], title only. The idle check is
  [agfi:h-agent-usage-idle-s].
- [agfi:agent-desk-dismiss] (`dismiss_command`): when a question is answered
  or an item is marked read, removes its notification with
  [agfi:notif-os-remove].
- [agfi:agent-desk-open] (`open_command`): opens a new kitty window running
  `agent-desk inbox <path>`, scoped to the item's node.

The core runs `notify_command` and `dismiss_command` detached, so a slow
notifier never holds up an agent; their output goes to
`~/.agents/desk/hooks.log`.

## Tests

    zsh -c 'source "$NIGHTDIR/zshlang/tests/agent-desk.zsh"'

All transports are inert fixtures.
