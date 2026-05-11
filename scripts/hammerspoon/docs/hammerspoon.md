# Hammerspoon

`~/.hammerspoon/init.lua` is a symlink to `~/scripts/hammerspoon/boot.lua`.
The boot file loads every Lua file in `~/scripts/hammerspoon/auto-load/` in
alphabetical order.

App-scoped modes should use the shared `ModalMode` helpers. qView is defined in
`auto-load/qview.lua`, exposes `qview_bind_v2`, and enters while qView is the
frontmost app. Its overlay is positioned in the top-left corner.

Purple Mode is defined in `purple-mode.lua` and loaded directly by `boot.lua`
before the mouse bindings that use `purple_bind_v2`. Enter it with
`Hyper+Cmd+P`. Current built-in Purple bindings include:

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
