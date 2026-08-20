# Hammerspoon

`~/.hammerspoon/init.lua` is a symlink to `~/scripts/hammerspoon/boot.lua`.
The boot file only sets up Lua/Hammerspoon dependencies and then loads the
ordered core module list from `~/scripts/hammerspoon/core/`.

The explicit core load order is:

- `helpers.lua`
- `modal-mode.lua`
- `agent-banner.lua`
- `redis.lua`
- `wifi-watcher.lua`
- `hyper-mode.lua`
- `purple-mode.lua`
- `mouse.lua`
- `input-language.lua`
- `popclick.lua`
- `system-keys.lua`
- `choosers.lua`
- `app-hotkeys.lua`
- `window-media-bindings.lua`
- `stt.lua`
- `reload.lua`

`reload.lua` loads every Lua file in `~/scripts/hammerspoon/auto-load/` in
alphabetical order after the core modules are ready.

Put core features in `core/` and add them to the explicit list in `boot.lua`.
Put app-specific add-ons that can run after all core modules in `auto-load/`.

App-scoped modes should use the shared `ModalMode` helpers. qView is defined in
`auto-load/qview.lua`, exposes `qview_bind_v2` and `qview_bind_v3`, and enters
while qView is the frontmost app. Its overlay is positioned in the top-left
corner. App-scoped modes are temporarily suspended while global modes such as
Hyper or Purple are active, then re-sync with the frontmost app after the
global mode stack exits.

Mode overlays are indicator groups (`ModalMode.createIndicatorGroup`): one
canvas per target screen, cached per screen. `ModalMode.screenWatcher`
invalidates the cached canvases whenever displays are added, removed, or
rearranged, so overlay positions self-heal without an `hs.reload()`. Other
modules can hook the same watcher with `ModalMode.onScreenChange(fn)` (the STT
recording indicator does this).

Which screens an overlay appears on is controlled by the `overlayScreens`
style key, resolved by `ModalMode.targetScreens`. Accepted values: `all` (every
screen), `primary` (menu-bar screen), `internal` (built-in display),
`all_external` (alias `external`), `active` (alias `main`; the screen with
keyboard focus), and `mouse` (the screen containing the pointer). Specs that
match no screen (e.g. `all_external` with no external attached, or `internal`
in clamshell mode) fall back to the primary screen so the overlay is never
invisible. App-scoped modes default to `primary`; pass `overlayScreens` in the
mode's `overlay` table to change it. The Hyper banner defaults to `all` via the
`hyper_overlay_screens` global in `core/hyper-mode.lua`.

`bind_v3` defines modal key chords. Key arrays use Hammerspoon key names plus
aliases such as `SPC`, `RET`, and `ESC`. While a chord is pending, the mode
overlay shows the pressed prefix. Valid next keys are consumed and advance the
chord; invalid next keys are consumed and cancel it. `Escape` cancels a pending
chord. Chords have no timeout, so prefix-overlapping chords are rejected.

Purple Mode is defined in `purple-mode.lua` and loaded before the mouse
bindings that use `purple_bind_v2`. Enter it with `Hyper+Cmd+P`. Current
built-in Purple bindings include:

- `Shift+Escape`: exit Purple Mode.
- `q`: send `Cmd+Z`.
- `a`: send `Cmd+Ctrl+H`.
- `d`: send `Cmd+Ctrl+M`.
- `s`: send `Cmd+Shift+M`.
- Arrow keys: move the mouse cursor using the keyboard-mouse helpers in
  `boot.lua`.

The avy grids in `core/mouse.lua` (`screenPositionAvy` and its click, drag,
text-select, and screenshot wrappers) label the screen with two-key
combinations. The label alphabet adapts to the screen: when a screen needs
more labels than the base list provides (bigger monitors), the second-char
alphabet grows minimally with comfort-ordered extension keys
(`avySecondCharExtension`), so small laptop screens always render the classic
grid unchanged. Beyond the two-key ceiling (~7.7k labels, e.g. non-HiDPI 4K),
cells enlarge just enough for full coverage. Extended lists are memoized per
alphabet size in `avyCombinationsFor`.

## Agent focus banner

`core/agent-banner.lua` shows a banner while a coding agent is driving the GUI
and needs the focus left alone, so a human and an agent can share the machine
without either guessing about the other. It is driven from the shell:

```sh
hs -c 'agentBannerOn("what it is doing", 900)'   # seconds; default 30 min
hs -c 'agentBannerOff()'
hs -c 'return agentBannerActive()'
```

It loads after `modal-mode.lua` because it reuses `ModalMode.targetScreens`
and `ModalMode.onScreenChange` rather than running a second screen watcher.
No canvas mouse events are registered, so it is inert to the pointer and
clicks pass through to whatever is underneath.

It covers each screen whole for `agentBannerFlashSeconds` (0.2 by default; 0
skips it, and a third argument to `agentBannerOn` sets it) before collapsing
to a 30px strip. The text is drawn in the strip's band at the strip's size
throughout, and shrinks rather than wraps when the message is long: the words
must not move, resize or reflow as the flash collapses, or they get yanked out
from under whoever started reading them. Only the coloured area changes.
`agentBannerOff` flashes `agentBannerReleaseFlashSeconds` of blue the same way
— the moment the machine is free again is the one worth noticing.

Re-calling `agentBannerOn` with the same message refreshes the countdown
without re-flashing, so a long task can heartbeat without strobing. A changed
message does flash again. The banner always expires on its own, so an agent
that crashes or forgets cannot leave the screen branded.

`~/.hammerspoon/init.lua` includes a `hyper+w` Wi-Fi chooser.

The chooser shows cached network names immediately when available, refreshes
nearby Wi-Fi networks in the background, ignores transient scan errors while
keeping the cache visible, marks the currently connected SSID with `*`, and
toggles the selected network. Selecting the current network disconnects from
Wi-Fi. Selecting another network runs:

```sh
networksetup -setairportnetwork <interface> <ssid>
```

This works best for open or previously remembered networks. New protected
networks may still need credentials added through macOS first.
