# Hammerspoon

`~/.hammerspoon/init.lua` is a symlink to `~/scripts/hammerspoon/boot.lua`.
The boot file only sets up Lua/Hammerspoon dependencies and then loads the
ordered core module list from `~/scripts/hammerspoon/core/`.

The explicit core load order is:

- `helpers.lua`
- `modal-mode.lua`
- `alert-engine.lua`
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
`hyper_overlay_screens` global in `core/hyper-mode.lua`, and the Purple banner
via `purple_overlay_screens` in `purple-mode.lua`.

Both global modes draw their banner this way. Hyper and Purple each used to
build one `hs.alert` per screen instead, rebuilt on every entry from
`hs.screen.allScreens()`; neither could draw over a fullscreen space
([issue 3586](https://github.com/Hammerspoon/hammerspoon/issues/3586)) and
neither followed a display being plugged in or unplugged. Hyper moved to a
canvas group behind a `hyper_alert_canvas_p` flag, which is gone: the alert
path it selected between was dead and has been removed.

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

## Alerts

`core/alert-engine.lua` draws alerts as coloured bands across the screen, one
per live alert, stacked. It replaces `hs.alert`'s single centred box for
everything that goes through `hs-alert` in zshlang: several alerts can be up at
once without hiding each other, long text wraps and the band grows rather than
being cut off, and an optional fullscreen flash makes one impossible to miss.

It is the engine for the Lua config too, which is why alerts raised from a
hotkey and alerts raised from the shell now look and stack the same way.

Callers do not name it, though. Everything goes through `alert_gateway`, with
`alert_gateway_dismiss` and `alert_gateway_exists` beside it, and only those
three functions know that the engine is `alertV2`. Changing which engine draws
an alert, or routing a subset of them somewhere else, is an edit at the bottom
of `core/alert-engine.lua` rather than a sweep over every caller. `opts` is
passed through untouched, so callers still use the option names below. The zsh
side has the same shape: `hs-alert` is a gateway over `hs-alert-v2`, with
`hs-alert-v1` still beside it.

For the same reason callers ask for colours by name (`color = "agent"`) rather
than by referencing an `alertV2*Color` table.

Stock `hs.alert` is the `alert_v1` global in `boot.lua` — named for what it is
so nothing reaches for it by accident, and kept only so the old engine stays
reachable by hand from the console.

Anything fired repeatedly passes a stable `id`, which is what keeps a held
volume key from stacking one band per repeat: re-showing an id rewrites that
band in place. The ids in use are `volume`, `input-language`,
`stt-input-device`, `stt-recorder-mode`, `emoji-chooser`, `wifi-chooser`,
`wifi-watcher`, `hyper-secure-input`, `purple-secure-input` and `nop`. A
message that can be superseded rather than repeated wants one too: the Wi-Fi
chooser shares a single id across the whole connect flow, so `Connecting` is
replaced by its own outcome rather than leaving two bands up.

Callers that used to hold an alert handle and close it now use a fixed id and
`alert_gateway_dismiss`, and pass `seconds = math.huge` where they mean "until
I clear it" — the engine clamps that to its own ceiling, so a caller that dies
cannot leave a band on screen.

It loads after `modal-mode.lua` because it reuses `ModalMode.targetScreens` and
`ModalMode.onScreenChange` rather than running a second screen watcher. No
canvas mouse events are registered, so every canvas is inert to the pointer and
clicks pass through to whatever is underneath.

```sh
hs -c 'alert_gateway("hello", { seconds = 5 })'
hs -c 'alert_gateway_dismiss("some-id")'
hs -c 'alertV2FromFile("/tmp/message.txt", { position = "bottom" })'
hs -c 'alertV2DismissAll()'
```

The first two are the gateway. The last two are engine entry points with no
gateway of their own: `alertV2FromFile` is what the shell wrapper calls, and
`alertV2DismissAll` has no caller in the Lua config, so neither was given an
indirection that nothing would use.

Options: `id` (re-showing the same id updates that alert in place), `seconds`,
`color`, `position`, `flashSeconds`, `countdown`, `pinned`, and `screens` (a
`ModalMode.targetScreens` spec). Everything expires on its own, so a caller
that crashes cannot leave the screen branded.

`alertV2FromFile` is the entry point the shell uses: it reads the message from
the file and deletes it. `hammerspoon -c` hangs on payloads of a few hundred
characters and takes the ipc port down with it until the stuck client is
killed, so the text must not travel in the command string — escaping it or
base64-encoding it makes no difference. `alertV2FromBase64` exists for short
messages typed by hand, where quoting is the only problem.

### Stacking and positions

Positions are `top` (the default), `center` and `bottom`, each an independent
stack. Two positions can overlap on a small screen; that is accepted rather
than prevented.

A stack grows away from its anchor edge with the oldest band at the anchor, so
a new alert never shifts the words someone is part-way through reading. For a
top stack that puts the newest at the bottom; a bottom stack anchors at the
bottom edge, which puts the newest on top.

### Height, wrapping and the cap

Text wraps against the actual pixel width and the band grows to fit. The font
is never shrunk. Menlo is used because it is monospaced, which makes wrapping
arithmetic rather than a measuring call per word, and makes command output line
up.

No position may use more than `alertV2MaxStackFraction` (0.45) of a screen's
usable height. Within that budget every alert gets one line before any alert
gets two, and the surplus then goes to the pinned alert and the newest one. So
a sixty-line command output does not push everything else off the screen: the
older alerts collapse to a single line reading `first line ... (+59 more
lines)`. An alert only disappears entirely when there is not room for one line,
and then a band at the anchor edge says how many are hidden. Nothing is
silently dropped.

`pinned` alerts claim their space before everyone else. The agent banner uses
it, so a wall of text elsewhere cannot push it off.

### The fullscreen flash

`flashSeconds` washes every screen in the alert's colour before the alert
settles into its band. During the flash all bands are drawn at exactly the
geometry they will keep, so when the wash drains away the words do not move,
resize or reflow — a flash that re-centred its own text would yank it out from
under whoever started reading it. Two flashes at once: last one wins.

The wash is see-through, `alertV2FloodAlpha` (0.33). It has to be impossible to
miss, but it covers every screen and should not black out what you were looking
at to do it. The bands keep their own opacity and stay readable on top of it.

### Colours

`alertV2DefaultColor` (dark slate) for ordinary alerts, `alertV2AgentColor`
(crimson) for the agent banner, `alertV2FreeColor` (blue) for its release
flash, `alertV2NoticeColor` (grey) for the hidden-alerts band. Amber, the
colour the banner shipped with, is kept commented out next to the default.

All four take their opacity from `alertV2BandAlpha` (0.8), so a band lying
across a window does not read as a hole punched in it — you can still tell what
it is covering. A colour passed in per alert keeps whatever alpha it carries.

## Agent focus banner

`core/agent-banner.lua` shows a banner while a coding agent is driving the GUI
and needs the focus left alone, so a human and an agent can share the machine
without either guessing about the other. It is a thin wrapper over the alert
engine: a crimson, pinned, counting-down alert with a fixed id. It is driven
from the shell:

```sh
hs -c 'agentBannerOn("what it is doing", 900)'   # seconds; default 30 min
hs -c 'agentBannerOff()'
hs -c 'return agentBannerActive()'
```

It washes each screen for `agentBannerFlashSeconds` (0.2 by default; 0 skips
it, and a third argument to `agentBannerOn` sets it) before settling into its
strip — see the flash notes above. `agentBannerOff` flashes
`agentBannerReleaseFlashSeconds` of blue the same way; the moment the machine
is free again is the one worth noticing.

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
