# tmux-z

tmux session helpers, and launchers that put a session in whatever directory
`z` finds.

Extracted from NightMachinary's local stack so that minimal remote setups get
the same code instead of keeping their own, drifting copy.

## Commands

- `tma <session> [cmd ...]` (`tmux-ensure-attach`): create the session if it is
  not already alive, then attach. Inside tmux it switches the client rather
  than refusing to nest.
- `tsh <session> [VAR=val ...] <cmd ...>` (`tmuxnewsh2`): run a command in a
  fresh detached session, collecting any leading `VAR=val` arguments into its
  environment. Replaces a session of the same name.
- `tmux-session-id`, `tmux-session-name-of`, `tmux-session-goto`,
  `tmux-alive-p`, `tmuxnew-ensure`: the session primitives the above are built
  from. A session *name* is not a safe tmux `-t` target; resolve it to an id
  first.

## Dependencies

- `tmux`
- `zshlang/basic/basic.plugin.zsh`, which the loader sources for you.

`z` is only needed by the `z`-aware launchers, and any implementation will do:
the local `ffz` and zoxide both work, because the directory is resolved by
`cd`-ing in a subshell and reading back `$PWD` rather than through any one
implementation's print mode.

## Configuration

- `tmuxnewsh_proxy_forward_p` — forward the caller's proxy variables into the
  new session. Default off.
- `tmuxnewshenv` — extra environment entries for the new session, as an array.

## Direct sourcing

```zsh
source /path/to/.shells/scripts/zshlang/plugins/tmux-z/tmux-z.plugin.zsh
```

## Plugin managers

Replace `NightMachinery/.shells` if you install from another fork or mirror.

### Zinit / Zi

Zinit must load this with `aliases`, because the basic plugin defines global
aliases (`@RET`, `@TRET`) that this plugin's functions use.

```zsh
zinit ice id-as"nightsh" \
  aliases \
  pick"scripts/zshlang/basic/basic.plugin.zsh" \
  multisrc"scripts/zshlang/plugins/tmux-z/tmux-z.plugin.zsh"
zinit light NightMachinery/.shells
```

### Antidote

```text
NightMachinery/.shells path:scripts/zshlang/plugins/tmux-z
```

### Sheldon

```toml
[plugins.tmux-z]
github = "NightMachinery/.shells"
use = ["scripts/zshlang/plugins/tmux-z/tmux-z.plugin.zsh"]
```
