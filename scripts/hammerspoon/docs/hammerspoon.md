# Hammerspoon

`~/.hammerspoon/init.lua` is a symlink to `~/scripts/hammerspoon/boot.lua`.
The boot file only sets up Lua/Hammerspoon dependencies and then loads the
ordered core module list from `~/scripts/hammerspoon/core/`.

The explicit core load order is:

- `helpers.lua`
- `modal-mode.lua`
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
