# Opening a tmux session where `z` lands

`codex-t scripts --resume` means "open tmux, jump to the directory `z scripts`
finds, run `codex-m --resume` there". The same shape works for the other
agents and for anything else:

- `claude-t`, `claude-work-t`, `codex-t`, `agy-t` — the agent launchers, in
  `zshlang/auto-load/others/agents.zsh`.
- `tz <query>[@<tag>] [VAR=val ...] [cmd ...]` — the general form.
- `tma-z <query>[@<tag>]` — the same thing with no command, so an interactive
  shell. This is what `tma-z` always did.

All of them are [agfi:tmuxnewsh2-attach-z], in the `tmux-z` plugin.

## The `@` is the only separator

`tz scripts` jumps to whatever `z scripts` finds. `tz agent-session` jumps to
whatever `z agent-session` finds. A tag, when you want one, comes after an
`@`: `tz scripts@main`.

The old `tma-z` instead derived the query by stripping the last dash segment
off the session name, so `tma-z agent-session` silently meant `z agent`, and
the behaviour changed depending on whether the name happened to contain a
dash. Splitting on `@` is unambiguous, and `@` does not occur in directory
names.

## No tag means a new session

Without a tag you get a *new* session every time, named with a generated
two-word tag that it prints:

```
❯ tz scripts
tmuxnewsh2-attach-z: zsh scripts@prairie-olive
```

That tag is the handle. `tz scripts@prairie-olive` later comes back to it.
Two launches in one directory therefore never collide, which matters because
[agfi:tmuxnew] kills the processes of a session it is replacing — the tag
generator retries on a collision for the same reason.

With a tag, an existing session is attached to rather than replaced, and you
are told that nothing ran:

```
❯ codex-t scripts@main --resume
tmuxnewsh2-attach-z: attaching to existing session, nothing was run
```

That message matters: the `--resume` you typed did *not* happen. Pick a new
tag if you want a fresh agent.

## Why the handle is a tmux option, not the name

The session is named `<cmd> <query>@<tag>`, e.g. `codex-m scripts@main` — but
that name does not survive. The agent autoname hooks rename it to
`+Codex <task title>` at the agent's first prompt; see
`docs/tmux-session-rename.md`.

So the durable handle is the tmux session user option `@tz_key`, holding
`<cmd>`, `<query>` and `<tag>` tab-separated, and lookups scan for it with
`list-sessions -F '#{session_id}\t#{@tz_key}'` ([agfi:h-tmux-session-by-key]).
Without it, a second `codex-t scripts@main` would start a second agent in the
same directory every time the first one had reached a prompt.
`zshlang/auto-load/others/agent-tmux.zsh` keeps `@agent_session` for exactly
the same reason.

## `z` runs in your shell, not in the session

The directory is resolved before the session exists:

```zsh
dir="$(cd "${HOME}" && FORCE_INTERACTIVE=y z "${query}" >/dev/null && print -r -- "${PWD}")"
```

So a query that matches nothing, or an `Esc` at the picker, aborts with the
error in front of you and no session created. The old arrangement ran `z`
*inside* the session, where a failure short-circuited the `&&` chain and the
session died taking its error message with it.

The `cd` is load-bearing. `z` is [agfi:ffz] here, whose corpus includes the
current directory's subtree, but zoxide on the minimal servers, which never
matches the current directory at all. Pinning to `$HOME` and reading `$PWD`
back out of the subshell is the one form both implementations spell the same
way.

## Knobs

- `tmuxnewsh2_attach_z_force_interactive` — what `FORCE_INTERACTIVE` is set to
  while `z` runs. Any non-empty value makes [agfi:isI] true, which is what lets
  the picker open; set it to the empty string to defer to the real tty state.
- `tmuxnewsh_pwd` — the directory [agfi:tmuxnewsh] puts a session in, which is
  how the resolved path is handed over. Useful on its own.
- `tmuxnewshenv`, `tmuxnewsh_proxy_forward_p` — as before.

## Things worth knowing

- The session ends when its command does. There is no shell left behind, so
  an agent that exits takes its scrollback with it.
- Resolving `z` in a subshell means [agfi:ffz-get] cannot persist
  `ffz_last_query`, so a bare `z` afterwards will not repeat a query made
  through `tz`. The redis choice cache is still written, so the query stays
  fast.
- A session literally named `<cmd> <query>@<tag>` that carries no `@tz_key`
  will not be found by the tagged path, and `tmuxnew` will replace it.
- `agy-t` sits one dash away from `agyt`
  ([agfi:agy-status-continue-tmux-fz]), which is a different thing.

## Where the code is

`zshlang/plugins/tmux-z/` — the session primitives, `tmuxnewsh`/`tmuxnewsh2`,
and these launchers. It is a plugin so that minimal remote setups load the
same code; `setup/minimal_proxy/.shared.sh` used to carry a second, drifted
copy. See `zshlang/plugins/tmux-z/README.md` and `docs/zsh_plugins.md`.
