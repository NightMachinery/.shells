# Psiphon Zsh Plugin

A Zsh wrapper around `psiphon-tunnel-core` that can install the client, generate configs, and manage a `tmux` session.

## Dependencies

Load the basic plugin before this plugin:

```zsh
source /path/to/.shells/scripts/zshlang/basic/basic.plugin.zsh
source /path/to/.shells/scripts/zshlang/plugins/psiphon/psiphon.plugin.zsh
```

External commands:

- Required for normal use: `zsh`, `jq`, `tmux`, `curl`
- Required for source-build fallback in `psiphon-install`: `git`, `go`

## Commands

After loading the plugin:

```zsh
psiphon-install
psiphon-init-config
psiphon-up
psiphon-up-us
psiphon-up-us96
psiphon-down
psiphon-toggle
psiphon-status
psiphon-logs
psiphon-test
```

Configuration is done through `psiphon_*` variables before calling commands. Common overrides include:

```zsh
psiphon_config_dir=~/.config/psiphon
psiphon_region=US
psiphon_upstream_socks_port=1096
psiphon_local_socks_port=1080
psiphon_local_http_port=2080
```

## Plugin-manager examples

Replace `NightMachinery/.shells` if you install from another fork or mirror. Load `night-basic` first, then `psiphon`.

### Antidote

```text
NightMachinery/.shells path:scripts/zshlang/basic
NightMachinery/.shells path:scripts/zshlang/plugins/psiphon
```

### Zimfw

```zsh
zmodule NightMachinery/.shells --root scripts/zshlang/basic --source basic.plugin.zsh
zmodule NightMachinery/.shells --root scripts/zshlang/plugins/psiphon --source psiphon.plugin.zsh
```

### Sheldon

```toml
[plugins.night-basic]
github = "NightMachinery/.shells"
use = ["scripts/zshlang/basic/basic.plugin.zsh"]

[plugins.psiphon]
github = "NightMachinery/.shells"
use = ["scripts/zshlang/plugins/psiphon/psiphon.plugin.zsh"]
```

### Zinit / Zi

Zinit must use `aliases`; otherwise aliases such as `@RET` are not expanded while plugin files are parsed.

```zsh
zinit ice id-as"nightsh" \
  aliases \
  pick"scripts/zshlang/basic/basic.plugin.zsh" \
  multisrc"scripts/zshlang/plugins/psiphon/psiphon.plugin.zsh"
zinit light NightMachinery/.shells
```

### Zplug

Use one package entry that sources both files from the monorepo in order:

```zsh
zplug "NightMachinery/.shells", use:"scripts/zshlang/basic/basic.plugin.zsh scripts/zshlang/plugins/psiphon/psiphon.plugin.zsh"
```

### Oh My Zsh

Oh My Zsh does not directly install arbitrary remote monorepo subdirectories. Clone the repository and source the two plugin files manually, or symlink the plugin directories into `$ZSH_CUSTOM/plugins`.

