# NightMachinary Zsh Basic Plugin

`zshlang/basic` is both the local basic-library directory and an installable Zsh plugin directory.

The `.plugin.zsh` files are loaders only; implementation lives in modular `.zsh` files next to them.

## Entrypoints

- `basic.plugin.zsh`: minimal public helper surface for other public plugins.
- `basic-full.zsh`: full/opinionated local stack used by NightMachinary's own shell loader.

Use `basic.plugin.zsh` unless you specifically want the full local environment.

## Manual loading

```zsh
source /path/to/.shells/zshlang/basic/basic.plugin.zsh
```

For the full local stack:

```zsh
source /path/to/.shells/zshlang/basic/basic-full.zsh
```

## Plugin-manager examples

Replace `NightMachinery/.shells` if you install from another fork or mirror.

### Antidote

```text
NightMachinery/.shells path:zshlang/basic
```

### Zimfw

```zsh
zmodule NightMachinery/.shells --root zshlang/basic --source basic.plugin.zsh
```

### Sheldon

```toml
[plugins.night-basic]
github = "NightMachinery/.shells"
use = ["zshlang/basic/basic.plugin.zsh"]
```

### Zinit / Zi

```zsh
zi ice id-as"night-basic" pick"zshlang/basic/basic.plugin.zsh"
zi light NightMachinery/.shells
```

### Zplug

```zsh
zplug "NightMachinery/.shells", use:"zshlang/basic/basic.plugin.zsh"
```

## Public helper surface

`basic.plugin.zsh` provides the small helper set needed by public plugins, including:

- `ec`, `ecn`, `ecerr`
- `bool`, `ensure-array`, `ensure-cmd`
- `gquote`, `reval`, `reval-ec`, `reval-ecgray`, `assert`
- `mkdir-m`, `trs-rm`, `tmuxnew`
- global aliases `@RET`, `@TRET`, `@STRUE`

