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

Miniforge supplies `conda` and `mamba`; it does not require a second micromamba
installation. Verify both managers and run Python from `py314` through the
manager as well as through the shell's PATH. The older Unix bootstrap's
micromamba recipe is an alternative environment layout, not an additional
required install on a Mac already using Miniforge.

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

## Unix command-line baseline

Audit the basic CLI list in the Unix bootstrap separately from language runtimes.
In addition to the GNU tools and existing shell dependencies, install the
applicable portable tools:

```zsh
brew install mosh tmate git-delta eza ugrep less aria2 htop bandwhich \
  fribidi pv progress ncdu tealdeer tag wireguard-tools gnupg sox sdcv rm-improved
```

Check current formula metadata before installing old entries. A missing formula
must be recorded explicitly; `hping` is no longer available under that name in
Homebrew. `tmate` remains installable but is marked deprecated, so retain it
only where that workflow is needed. Installing a networking CLI does not require
enabling a server or changing the machine's network configuration.

Homebrew's `util-linux` is keg-only. Expose just its `flock` in the user's bin
directory if needed, and test mutual exclusion with a temporary lock file.
Avoid force-linking the whole keg over macOS utilities.

The active URL helpers also use the Nim programs from `setup/setup_misc`:
install Nim and build `htmlmetadata` and `unalix` with Nimble, then test their
entry points. Unalix can fail to build with Nim 2.2 because of its older
sequence/tuple assumptions. Use a compatible compiler or transfer a working
native binary after checking its libraries and checksum. Dereference Nimble
launcher symlinks when transferring just an executable. Verify URL cleaning
on an inert fixture before treating it as installed. Audit Dart, Julia, Lisp and
other specialist runtime installs against the intended workflows; keep them explicitly optional when no required
command depends on them. A current Homebrew Ruby does not require running the
old RVM/Rails installer too.

## Full workstation CLI profile

The baseline above is not enough for daily use: media, OCR, document and macOS
control commands (`nightlight`, `brightness`, `blueutil`, `tesseract`,
`ocrmypdf`, `mpv`, ...) are called from shortcuts and helpers and fail only when
used. `setup/macOS/workstation-tools.json` is the profile: the formulae to
install, and the command names that must resolve afterwards, grouped by use.
`setup/macOS/workstation-tools.py` audits it, and with `--apply` installs what
is missing, writing one log per formula and an `audit.json` under
`~/.local/state/mac-bootstrap/`:

```zsh
~/scripts/setup/macOS/workstation-tools.py           #: audit only
~/scripts/setup/macOS/workstation-tools.py --apply   #: install, then audit
```

It exits non-zero while any formula or command is missing. It does not install
GUI apps, provider logins, or the native helpers (`pbv`, `input_lang_get_objc`,
the legacy `xml2` family); their recipes are in the profile's
`native_helpers`, and the command audit reports them until they are built.

Three traps it handles, all seen on a fresh Mac:

- The shared `.gitconfig` rewrites `https://github.com/` to SSH. Before the new
  machine's key is registered on GitHub, and before it knows GitHub's host keys,
  every `brew tap` fails with "Host key verification failed". Homebrew strips
  `GIT_CONFIG_GLOBAL` from git's environment, so it cannot be switched off from
  outside. The helper therefore clones public taps itself over HTTPS with the
  global config disabled, and appends GitHub's published host keys (from
  `https://api.github.com/meta`) to `~/.ssh/known_hosts`. `brew update` on those
  taps still goes over SSH, so it works only after the key is registered.
- `nightlight` comes from the `smudge/smudge` tap and builds from source; it
  pulls in Homebrew's `rust` as a build dependency next to rustup. rustup's
  `~/.cargo/bin` stays first on `PATH`, as on the source Mac.
- `brightness` must be built `--HEAD` for Apple Silicon built-in panels.

Formulae left out deliberately: `gerbil-scheme` (conflicts with ghostscript over
`gsc`, and nothing calls it), `chezscheme` (unused), and `sshfs` (Linux-only in
Homebrew core; macOS needs macFUSE).

After installing, check behaviour, not just presence, on synthetic inputs: OCR a
rendered text image with `tesseract` and `ocrmypdf`, then read it back with
`pdftotext`; encode a tone and a short video with `ffmpeg` and inspect them with
`ffprobe`/`mediainfo`; play the tone with `mpv --ao=null`. For macOS controls,
only read status (`nightlight status`, `brightness -l`, `SwitchAudioSource -c`,
`displayplacer list`); toggling them changes the owner's display. `blueutil`
needs a Bluetooth privacy approval that macOS asks for in the GUI session. Over
SSH it hangs, without an error, until someone approves the prompt on the Mac's
screen; after that it works over SSH too.

Tesseract cannot read images through a symlinked directory such as `/tmp`
([tesseract#4333](https://github.com/tesseract-ocr/tesseract/issues/4333)),
which makes `ocrmypdf` fail in SSH sessions, where `TMPDIR` is unset and Python
falls back to `/tmp`. `~/.shared.sh` sets `TMPDIR` from
`getconf DARWIN_USER_TEMP_DIR` when it is missing.

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

Paseo Desktop manages its bundled daemon by default. For a standalone daemon,
first install a CLI version matching Desktop into an independent npm prefix:

```zsh
npm install --global --prefix "$HOME/.local/share/paseo-cli" @getpaseo/cli@VERSION
"$HOME/.local/share/paseo-cli/bin/paseo" --version
```

Use the version reported by the affected Desktop/daemon, rather than upgrading
one side silently. This avoids altering a shared pnpm store. Check that `paseo`
resolves to this independent installation, not Desktop's bundled launcher.
Before takeover, confirm there are no active agents, back up Desktop settings,
and quit Desktop. Set `settings.daemon.manageBuiltInDaemon` in Desktop's settings
file to false, keeping
all other settings, then start the standalone CLI with the existing `PASEO_HOME`.
Preserve its config, server identity and pairing files. Start it after login
initialization with the account's shell explicitly selected (`SHELL=/bin/zsh`),
since helper initialization can change the inherited `SHELL`. Set
`NIGHT_PASEO_STANDALONE=y` in `~/.startup..private..zsh` so this workstation's
login launcher starts the CLI supervisor; the supervisor backgrounds itself.

Verify `paseo daemon status --json` reports `desktopManaged: false`, a reachable
daemon and the same server identity. Reopen and quit Desktop once and confirm
the standalone supervisor stays alive. Repeat `paseo daemon start` and verify
it reuses that supervisor. Test boot recovery separately after FileVault unlock.
Do not run both supervisors for the same home.

Install provider CLIs in the destination account and run
`paseo provider diagnostic claude --json` (also `codex` and `antigravity`) against
the actual daemon. A binary or provider marked available does not prove an
account is signed in. Use fresh provider login flows unless credential migration
has been explicitly approved; do not copy an entire agent home to install a CLI.
See [Paseo's server install](https://paseo.sh/docs.md) and
[daemon lifecycle](https://paseo.sh/docs/cli.md).

After installing the binaries, connect their portable configuration too. Back up
existing files, then link both Claude seats' `settings.json` to
`configFiles/claude-code/settings.json`, Codex's `~/.codex/hooks.json` to
`configFiles/codex/hooks.json`, and Antigravity's `~/.gemini/config/hooks.json`
to `configFiles/antigravity/hooks.json`. Link the personal/work Claude themes
using [agfi:claude-themes-link]. Preserve installer-only Claude preferences,
such as its update channel, in that seat's `settings.local.json`.

Select portable Codex and Antigravity preferences explicitly: model/reasoning,
editor, appearance, status line and notification command. Adapt paths and leave
account credentials, project trust, hook approvals, MCP credentials and plugin
installation state on their own host. Installing hook definitions does not
establish that the provider trusts or has executed them. After owner sign-in,
review the hooks through the provider's normal interface.

The shared Claude settings use `/usr/local/libexec/claude-askpass`. Install
`pinentry-mac`, and link that path to `configFiles/claude-code/askpass.py` with
executable permissions. Its password dialog is an owner interaction; do not
invoke it during unattended setup. The shared settings declare the official
`anthropics/claude-plugins-official` marketplace so new hosts discover the same
plugin catalog. Install the enabled official clangd plugin
with `claude plugin install clangd-lsp --marketplace
anthropics/claude-plugins-official --json`, and check the installed payload plus
the `clangd` executable separately from account login. Keep the tracked settings
link intact if an installer replaces it.

For login startup, create a user LaunchAgent invoking the Homebrew Zsh with
`-lc 'source "$HOME/scripts/launchers/various-darwin.zsh"'`. Run it once at login,
not with KeepAlive: its child services have their own lifecycle. Capture logs.
The profile leaves scheduled maintenance to a separate, deliberate setup.

Hammerspoon's config needs Lua 5.4 rocks `penlight`, `luaposix`,
`lrexlib-PCRE2` and `redis-lua` (which installs LuaSocket). Use the per-user
rock tree and the matching Lua version. Link
`~/.hammerspoon/init.lua` to `../scripts/hammerspoon/boot.lua`. Accessibility,
Automation and input permissions are granted separately through macOS.
After reloading, check that both `hyper_modality` and `hyper_toggler_1` exist
and that `hammerspoonReloadHeldBy` is defined at the end of the configuration.
The IPC channel can work even when an earlier missing module stopped startup.
Finish with a physical Fn plus Hyper shortcut test; a successful HID map
readback alone does not verify the shortcut.

After cloning vcsh, audit tracked symlinks for paths into the source user's
home. Back up conflicting files before checkout. Fetch updates from the same
GitHub repository used by the source machine, even if the transport is HTTPS
until the new host has its own GitHub authentication.

Kitty's configuration uses Fira Code and Victor Mono, plus helper commands in
`/usr/local/bin`. Doom additionally requires Fira Mono and Symbols Nerd Font Mono;
Fira Code does not provide the Fira Mono family. Install those fonts, link `bicon_zsh.dash`, `ffscrollback.sh`,
`emacs_scrollback.sh` and `wait4user.sh` from `zshlang/wrappers`, and expose the
Homebrew Zsh as `/usr/local/bin/zsh`. The terminal wrapper uses `/bin/sh` because
macOS has no `/bin/dash`. Compile Kitty's bundled `kitty.terminfo` into
`~/.terminfo`, and verify `TERM=xterm-kitty infocmp` and an interactive login
shell. Recheck the symlink targets before launching the GUI.

Copy the configured font files directly into `~/Library/Fonts`, preserving
changed destination files in a small backup. The current selection includes
Fira Code, Fira Mono, Victor Mono, Symbols Nerd Font Mono and the legacy
all-the-icons families. Copy actual font files rather than another account's
absolute symlinks. A file in that directory is not proof it is registered.
Build `setup/macOS/register-fonts.m` with Foundation and CoreText, then run it
as the graphical user with the font paths. It registers at user scope and
accepts an already-registered font; JSON output reports failures per file.
When installing over SSH, run registration as a one-shot LaunchAgent in the
`gui/<uid>` domain and remove that job afterward.

A missing `Fira Mono` can interrupt Doom's `after-init-hook` before theme
loading and finalization, so both the theme and Space leader can be absent
while `night-loaded` is true. Treat that marker as configuration evaluation,
not completed UI startup. After installing the font, repair an affected live
instance with `doom-init-fonts-h`, `doom-init-theme-h` and, only when
`doom-init-time` is nil, `doom-finalize`. This preserves buffers. Verify full
startup and the intended interaction only within the owner's testing scope.

Kitty also needs `~/.kitty_custom_1.conf` linked to
`~/scripts/configFiles/kitty/me.conf`. That include selects `session.kitty`,
with the ivy workspace and terminal Emacs among the tabs, and exports the
terminal background hints. Without it, Kitty starts a plain shell. Session
changes require a full Kitty restart, which can terminate direct child jobs;
coordinate that restart rather than closing a user's terminal to test it.
The first ivy tab creates its workspace when absent, even when background
services already have a tmux server. Existing ivy workspaces keep the normal
reuse path.

The workstation include removes inherited `SSH_CLIENT`, `SSH_CONNECTION`,
`SSH_TTY`, `PROMPT_PURE_SSH_CONNECTION` and `amSSH` from local Kitty children.
An app opened through SSH can inherit these markers, causing Pure to show
`user@host` in a local tab despite the prompt's stale-utmpx guard. Actual
SSH/mosh sessions establish their own remote context. When bootstrapping an
already-running tmux server over SSH, clear stale global transport markers
there too before creating local sessions; preserve existing session state.

Audit the complete link setup rather than assuming a dotfiles checkout creates
its untracked links. `python3 setup/macOS/link-workstation.py` prints the current
state; add `--apply` to back up conflicts and install user links. Run its separate
`--system --home /path/to/user --apply` phase with privileges for `/usr/local`
wrappers. The helper covers the relevant `setup/setup_links` files without
running its old install steps, creating dangling legacy binaries or depending
on an account-name test. It preserves an existing Kitty theme selection.
Tracked links inside the repository must also be relative, including Zopen's
launcher resource. A relative link to an optional generated compilation database
can remain unresolved until that project is built; do not point it at another
user's build directory.

In particular, link `~/.gitconfig` to `symlinkme/local/.gitconfig`, copy the
owner's `~/.gitignore_global`, and install `git-lfs` for the required Git filter.
Compare source and destination newline settings before calling copied notes
modified: `core.autocrlf=input` and `core.eol=lf` can remove CRLF-only status
noise without changing the files. Keep credentials and signing keys separate;
a config link does not authenticate GitHub or supply its signing key. The MPV
config link needs its own repository at `~/code/lua/mpv`; do not create it when
that directory is absent.

Generate agent instructions with [agfi:agents-md-sync] and install the available
skill links with [agfi:agent-skills-link]. Keep generated global instruction
files managed by those helpers. Provider sign-ins and private skill/config
repositories remain separate, deliberate migration steps.

## Workflow postflight

For native preference checks, use System Settings’ `View > Menu Bar` menu.
It can reach the pane when a Settings deep link opens General or leaves a stale
search visible. Read the pane’s controls after dialogs close, including menu
visibility, Always/When Active choices and Battery percentage. Control Center
placement is a separate check on macOS versions with the Control Gallery.

Store-backed Brave extension payloads still need native activation. Open
`brave://extensions/`, switch each requested extension on and handle any
`Action Required` menu entry with `Enable Extension`. Scroll lower cards into
view before clicking. Verify that every card says `On` after activation;
payload presence or a successful automation call does not prove enablement.
Install Brave-maintained uBlock Origin through
`brave://settings/extensions/v2` and accept its native `Add extension` prompt.

A working Hyper key still needs its tools' data. Install the emoji repository at
`~/code/misc/unicode-emoji-json`, rebuild from the latest stable Unicode release
and run its consistency tests. Reload the picker and test Hyper+A search/copy.
Migrate `~/notes`, `~/base/dicts`, `~/.stardict` and `~/.sdcv_ordering` when required,
preserving symlinks and destination changes. Check actual English, Persian and
German lookups through the shell helpers after checksum verification.

The GUI launcher [agfi:emc-launch-gui] uses the existing emacs-plus installation
when available, then `/Applications/Emacs.app`. `NIGHT_EMACS_APP` can select an
explicit executable. Verify this path without launching a second GUI instance.
An absent Homebrew formula must never turn an empty cellar path into a root
glob: checking automounted directories under `/` can stall a graphical launch.

Emacs.app and Doom are separate installs. Bring over the pinned Doom base and
configuration, initialize loaded submodules, install packages and native modules,
and verify a real daemon plus GUI startup. Wait for the completed Doom startup
and its server restart before evaluating through emacsclient, then require an
artifact written by Emacs itself. An early socket and a zero client status can
precede a dropped request during server restart. For mail follow `~/doom.d/docs/email.md`:
private identity, destination Keychain password, notmuch/isync/msmtp/flock,
LaunchAgent, completed server sync and inbox access. Never use an unsolicited
outgoing test message as validation.

Install AltTab and Maccy, copy the patched Homerow bundle, and migrate their
preference domains after backing up destination settings. Adapt home paths and
verify shortcuts and macOS permissions. Maccy's settings do not require copying
clipboard history.

Private startup can run [agfi:v2-on], private `xray-run` proxy sessions,
[agfi:clipboard-remote-listen] and the html-reports `ctl-mem` loop. Install xray,
gost and the Python/Go helpers before enabling them. Check live tmux panes,
loopback listeners and repeat startup without duplicate services. Keep proxy
URLs outside this public repository.

Deploy the memory panel with its own device config and owner-only access,
private Unix sockets, Caddy and Tailscale Serve. Check both its HTTPS page and
health endpoint. Set launchd's PATH explicitly to the Python environment with
PyYAML and TOML support; a successful interactive build does not verify launchd.
Use the public-network pf installer with explicit `--tailscale --ipv6`, verify
its active rules and persistence, then test Mosh through both pf and the macOS
application firewall. See [tailscale-setup.md](tailscale-setup.md).


## App data and keyboard postflight

Install the requested desktop apps explicitly, including DeltaChat, NordVPN,
Throne, Skim and Thunderbird when those workflows are needed. Homebrew can
continue other casks after a package installer fails, so verify every bundle.
For a privileged package use the required `sudo -kA /usr/sbin/installer` path
and verify its signed app; do not infer success from a remote exit status.

Thunderbird profile and credential migration requires explicit owner approval
for each destination. With Thunderbird closed on both Macs, copy
`~/Library/Thunderbird/` directly, excluding process locks and backing up target
files privately. Check file hashes, configured profile paths, `key4.db`,
`cert9.db` and `logins.json`; keep the encryption database and encrypted logins
together. Adapt old absolute home paths. A Primary Password may still need to be
entered locally. Do not send test mail or open a nonempty Outbox automatically.

Handy's app bundle alone is insufficient: copy its settings store from
`~/Library/Application Support/com.pais.handy/` and the selected model's actual
cache. Custom GGUF models can live in the Hugging Face cache under
`~/.cache/huggingface/hub/models--OWNER--MODEL/`; preserve both blobs and snapshot
symlinks. Verify the selected file resolves after copying. Settings/models do
not require transferring recordings or transcription history.

On Touch Bar hardware, use `PresentationModeGlobal=functionKeys` in
`com.apple.touchbar.agent` and `com.apple.keyboard.fnState=true` when permanent
F1–F12 is wanted. Back up those keys, restart TouchBarServer and test the physical
row together with the Fn-to-Hyper mapping.

Enable the macOS application firewall and stealth mode alongside the pf filter,
then verify the necessary app-specific allowances, especially Mosh. Do not
select block-all when the intended workstation services need incoming access.
For broken Settings search, reproduce a real typed query and check language,
region and input-source registration before considering a disk-wide index
rebuild. A preference-refresh workaround is an experiment until actual results
appear; restore unsuccessful experiments and retain a precise open issue.

Use direct resumable transfers for ordinary data with targeted destination
backups. Consistency snapshots may be useful for live databases or images;
keep them only through verified acceptance, then remove the owned snapshot.
Retain small manifests and results, rather than permanent payload staging.
