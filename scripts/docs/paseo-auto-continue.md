# Auto-continue for Paseo

The shared `/auto-continue` skill recognizes `PASEO_AGENT_ID` and registers
that Paseo agent instead of a terminal. It supports Claude (Default), Claude
(Work), and Codex providers, including aliases with their own `CODEX_HOME`.
Nothing is enabled until you opt in.

Inside a Paseo agent, use `/auto-continue`, `/auto-continue status`, and
`/auto-continue off`. The underlying functions are
[agfi:agent-auto-continue-on], [agfi:agent-auto-continue-status], and
[agfi:agent-auto-continue-off].

From a shell on the daemon's computer, pass the exact Paseo agent UUID:

```zsh
paseo-auto-continue-on AGENT_UUID
paseo-auto-continue-status AGENT_UUID
paseo-auto-continue-off AGENT_UUID
```

These are [agfi:paseo-auto-continue-on],
[agfi:paseo-auto-continue-status], and [agfi:paseo-auto-continue-off].
`--home PATH` selects a local daemon home; `--poll SECONDS` changes the
five-minute default (minimum 30 seconds). Run the watcher on the daemon host,
where its agent state and provider credentials are available. Remote
`PASEO_HOST` connections are rejected. `--frontmost` is unsupported for Paseo.

## When it resumes

A private registration binds the exact agent ID, daemon home, provider
configuration and native profile directory. Labels may change without
invalidating it; provider launch settings or profile changes disable it and
require explicit re-registration.

The worker watches that agent's persisted error state. It only checks account
usage after that agent's own failed turn contains a quota/rate-limit error.
An idle or running agent, a permission request, an unrelated network error,
or another session exhausting the account does not trigger a continuation.

Quota comes from the existing `claude_code_usage.py` or `codex_status.py`
reader, with the selected provider's profile directory. Claude checks its
session, account-wide weekly and applicable model-family weekly windows.
Codex uses the active account's existing `.quota.blocked` verdict, not the
quota of whichever other account has room. Claude authentication refresh via
print-mode `/usage` is disabled; a credential/read error simply postpones the
check. Targeted profile token files work as in `claude-code-usage`.

Once usage is possible again, the worker checks that the same failed turn is
still current and no permission request or user continuation appeared. It
then records that turn as claimed and calls:

```sh
paseo send AGENT_UUID 'Continue. ...' --home DAEMON_HOME --no-wait --json
```

The message asks a finished task to turn auto-continue off. One failed turn
gets at most one automatic send, including across watcher restarts. If a send
fails or times out, its outcome may be uncertain, so the watcher disables
itself rather than sending again. Inspect the agent before explicitly arming
it again. Repeating `on` for an already enabled registration preserves the
claimed turn.

Delivery uses Paseo directly, so it needs no keyboard focus, idle-time gate,
unlocked display or attached TUI. Manual messages sent between the final
check and the CLI send can still race the watcher; the CLI has no conditional
send operation. Turn it off before intentionally abandoning a blocked task.

## Worker and state

Each registration has a detached tmux worker named
`paseo-auto-continue-<hash>`, which outlives the originating shell. State lives
under `${XDG_STATE_HOME:-~/.local/state}/agent-sessions/auto-continue-paseo/`
in private files. `status` reports the worker name so its output can be
inspected with `tmux attach` or `tmux capture-pane`.

`off` disables the registration under the same lock used for sends before it
returns. The idle worker exits at its next check; it is not killed. A reboot
stops workers, and another `on` starts the worker again. No daemon restart is
needed.

The watcher supports local Claude default/work subscription profiles and
Codex account quota. It does not automatically choose another provider or
account, and it cannot recover an error that Paseo fails to persist as a
quota-failed turn. Credentials are read at check time from the bound profile;
turn the watcher off before signing that profile into a different account.

Code: `python/paseo_auto_continue.py`,
`zshlang/auto-load/others/paseo-auto-continue.zsh`, and the shared dispatch in
`zshlang/auto-load/others/agent-auto-continue.zsh`.
