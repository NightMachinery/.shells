# kitty-popup-window-toggle

`kitty-popup-window-toggle` (with `kitty-popup-window-show` and
`kitty-popup-window-hide` in `zshlang/auto-load/others/terminal emulators/kitty.zsh`)
replicates Hammerspoon's `hyper+z` panel-mode toggle without calling
Hammerspoon (`hs`), so it continues working when Hammerspoon hangs or is
unavailable.

## Components

- `zshlang/auto-load/others/terminal emulators/kitty.zsh`: user-facing Zsh
  functions, concurrency lock (`${TMPDIR:-/tmp}/kitty-popup-window-toggle.lock`),
  kitty launch/polling (`open -b net.kovidgoyal.kitty --args --start-as minimized`),
  and `kitten @` + `jq` slow-path fallback (`h-kitty-panel-*`).
- `swift/kitty_panel.swift`: `scriptisto`-cached Swift binary combining macOS
  `AppKit`/`CoreGraphics`/`Accessibility` queries with a direct Unix-socket
  Kitty remote-control fast path.
- `configFiles/kitty/kitty_panel_state.py`: existing `no_ui` state kitten
  reused over the Unix socket to read `win`, `active`, `firstTab`, `strays`,
  and `platformId` without invoking `kitten @ ls`.

## Fast Path vs Slow Path

On steady-state presses, `kitty-popup-window-toggle` makes a single call to
`swift/kitty_panel.swift`:

1. Inspects `NSWorkspace.shared.frontmostApplication`, `NSScreen.screens`, and
   `CGWindowListCopyWindowInfo` (front-to-back) to check whether
   `net.kovidgoyal.kitty` is frontmost and its `layer != 0` panel window is
   visible on screen.
2. On hide: queries panel state over `~/.local/state/kitty-<pid>.sock` using
   Kitty's `ESC P @kitty-cmd <JSON> ESC \` framing, sends `resize-os-window
   --action=hide`, and reactivates the topmost regular app behind the panel on
   that screen (raising the specific window via Accessibility if the app spans
   multiple screens). Explicit `hide` skips focus restoration if kitty was not
   frontmost.
3. On show: resolves the working screen's localized `output-name`, optionally
   prepares `~/Applications/ScreenDecor.app` (`night.screen-decor`) when
   showing over a fullscreen Space while another screen sits on an empty
   desktop (detecting Space types via CoreGraphics' `CGSCopyManagedDisplaySpaces`
   with an `AXFullScreen` / bounds fallback), sends `hide` -> `os-panel
   output-name=<name>` -> `show` -> `focus-window`, and runs an Accessibility
   frame-fit / Space repair if needed.

Exit codes coordinate the Zsh slow-path fallback without duplicating macOS
window logic:

- `0`: completed on the fast path.
- `10`: kitty is not running; Zsh launches kitty minimized, polls
  `kitty-socket-get`, ensures the panel exists via `kitten @`, and shows it.
- `11`: slow show needed (no panel yet, `--fold-strays` with stray tabs, or
  socket RC failed); Zsh runs `h-kitty-panel-slow-show` using
  `/Applications/kitty.app/Contents/MacOS/kitten @`, `jq`, and
  `kitty_panel.swift` subcommands (`screen`, `decor-prepare`, `fit`).
- `12`: slow hide needed; Zsh runs `h-kitty-panel-slow-hide`.

## Usage and Options

- `kitty-popup-window-toggle [toggle|show|hide]` (defaults to `toggle`)
- `--screen <name>` or `kitty_panel_screen`: override target screen localized name
- `--fold-strays` or `kitty_panel_fold_strays=y`: fold tabs from non-panel OS
  windows into the panel on every show (by default, strays are folded only when
  creating a fresh panel)
- `--no-decor` or `kitty_panel_decor_p=n`: skip `ScreenDecor` empty-desktop
  covering
- `--no-fit` or `kitty_panel_fit_p=n`: skip post-show Accessibility frame-fit
- `--quiet` / `-q`: suppress non-fatal status messages

## Differences from Hammerspoon `hyper+z`

- No background `kittyFocusWatcher`: switching to another app via `Cmd-Tab` or
  clicking elsewhere does not auto-hide the panel once the command exits.
- Focus return picks the topmost regular window behind the panel from
  CoreGraphics' on-screen window list rather than Hammerspoon's
  `recentAppsByScreen` activation history.
- Panel mode only (`wm_class` `kitty-panel`).
