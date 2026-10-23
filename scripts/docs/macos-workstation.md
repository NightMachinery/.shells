# A new macOS workstation

The `workstation` startup profile runs BrishGarden and the shared history index
on a new Mac without depending on the older machines' host predicates. Enable it
in the host-local `~/.nightsh_private_first`:

```zsh
export NIGHT_DARWIN_PROFILE=workstation
return 0
```

Keep `return 1` there while installing dependencies. It defers the full shell;
`nsh` loads it explicitly for a bounded smoke test. Use the destination account's
home directory. Do not copy another user's absolute `/Users/...` paths.
Restart BrishGarden after enabling full shell loading: workers started while
loading was deferred also need the full shell. Verify an actual `brishzgo` call.

Install Homebrew's core GNU tools, Zsh, vcsh, Redis, Go, uv, Python and tmux.
Resolve the GNU parallel/moreutils conflict with the link order in `setup_misc`.
Compile `setup/terminfo-24bit.src` with `tic -x -o ~/.terminfo`, then run
`setup/setup_go` in the loaded Night shell. Build `golang/omni-history` too. Install the
Go command `github.com/NightMachinary/prefixer` as well, since shell error
formatting uses it. Record the source binary's module version with `go version -m`
when matching an existing installation.
Install BrishGarden as an isolated uv tool as described in
[python-envs.md](python-envs.md). The old `setup.basic` and `setup_macOS.zsh`
include large optional and obsolete installs; review them before running.

## Development environment

A working shell does not verify the development environment. Track runtimes,
repository helper programs, Python libraries and isolated tools separately.
Install the build prerequisites explicitly:

```zsh
brew install go node uv ruby cmake ninja pkgconf pnpm
npm install --global bun
```

Install stable Rust through rustup using its minimal profile and
`--no-modify-path`, then add `rustfmt`, `clippy` and `rust-src`. The shared shell
already includes `~/.cargo/bin`; avoid a second Homebrew-managed Rust toolchain.
Review `setup/setup.cargo` and build its required programs, recording each failure.
Its legacy `input_lang_get_icon` depends on a nightly-only crate and can fail
with stable Rust. The current shell uses [agfi:input-lang-get-icon]; do not
install another toolchain solely for the unused legacy binary.
Beyond the local programs in `setup/setup_go`, the shell uses `ntagcolor`,
`jalalicli`, `ntom`, `possiblycat` and `prefixer` from `setup/ins_go`.

Follow [python-envs.md](python-envs.md) for libraries in Miniforge's `py314`
environment and commands in isolated uv tool environments. Download the native
Apple Silicon Miniforge installer and its **versioned** checksum asset from the
same release; verify it before batch installation with `-b -p ~/miniforge3`.
Create `py314` with Python 3.14 and pip from conda-forge. Avoid shell-init commands
that rewrite tracked dotfiles. In host-local configuration, ensure
`~/.local/bin` and `~/miniforge3/envs/py314/bin` precede Homebrew. A
`~/.nightsh_private_first` that prepends Homebrew on every load can make
`python` and `python3` select different environments.

Install library manifests one requirement at a time into that environment,
including the applicable Jupyter and script-specific manifests. Record failed
requirements, then run `uv pip check --python ~/miniforge3/envs/py314/bin/python`.
Install `python/uv-tools.txt` with managed interpreters through [agfi:ins-uv-tools].
Entries with editable local paths require those source repositories first;
record them separately instead of silently skipping them. Keep a working
BrishGarden environment intact while installing unrelated tools.

Validate from a fresh interactive login shell: print `sys.executable` for both
`python` and `python3`, import the required libraries, and run a small compiled
program with Go, Rust, C/C++ and Java. Check Node, Bun, pnpm and Ruby too. Verify
actual helper commands and isolated-tool entry points, not only package-manager
exit codes. Keep installed, validated and deferred entries distinct.

For remote scripts, upload the whole file before executing it with SSH stdin
closed. An installer reading a script's shared stdin can consume later commands.
Require a final completion marker and validate outputs, especially when the
remote path does not propagate nonzero exit status correctly.

## Services and login startup

Redis must run before a full shell load. Use a user LaunchAgent, loopback bind,
protected mode, and the password from `~/.redis-auth` (mode 600). Keep its config,
data and log paths under the user's Library. Verify authenticated `PING` and
rejection of an unauthenticated request before enabling startup. Never print the
password in diagnostics. Keep the service independent of shell initialization.

The profile reads `~/.startup..private..zsh` before starting services. Suggested
host-local settings:

```zsh
export NIGHT_BRISH_WORKERS=8
export NIGHT_WORKSTATION_OPEN_APPS=y
```

Paseo Desktop manages its bundled daemon by default. A host deliberately using
standalone mode may set `NIGHT_PASEO_STANDALONE=y`; disable Desktop daemon
management in that case. Do not run both supervisors for the same home.

For login startup, create a user LaunchAgent invoking the Homebrew Zsh with
`-lc 'source "$HOME/scripts/launchers/various-darwin.zsh"'`. Run it once at login,
not with KeepAlive: its child services have their own lifecycle. Capture logs.
The profile leaves scheduled maintenance to a separate, deliberate setup.

Hammerspoon's config needs Lua 5.4 rocks `penlight`, `luaposix` and
`lrexlib-PCRE2`. Use the per-user rock tree and the matching Lua version. Link
`~/.hammerspoon/init.lua` to `../scripts/hammerspoon/boot.lua`. Accessibility,
Automation and input permissions are granted separately through macOS.

After cloning vcsh, audit tracked symlinks for paths into the source user's
home. Back up conflicting files before checkout. Fetch updates from the same
GitHub repository used by the source machine, even if the transport is HTTPS
until the new host has its own GitHub authentication.

Kitty's configuration uses Fira Code and Victor Mono, plus helper commands in
`/usr/local/bin`. Install those fonts, link `bicon_zsh.dash`, `ffscrollback.sh`,
`emacs_scrollback.sh` and `wait4user.sh` from `zshlang/wrappers`, and expose the
Homebrew Zsh as `/usr/local/bin/zsh`. The terminal wrapper uses `/bin/sh` because
macOS has no `/bin/dash`. Compile Kitty's bundled `kitty.terminfo` into
`~/.terminfo`, and verify `TERM=xterm-kitty infocmp` and an interactive login
shell. Recheck the symlink targets before launching the GUI.

Generate agent instructions with [agfi:agents-md-sync] and install the available
skill links with [agfi:agent-skills-link]. Keep generated global instruction
files managed by those helpers. Provider sign-ins and private skill/config
repositories remain separate, deliberate migration steps.
