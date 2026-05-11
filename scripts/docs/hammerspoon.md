# Hammerspoon

`~/.hammerspoon/init.lua` includes a `hyper+w` Wi-Fi chooser.

`~/.hammerspoon/init.lua` is a symlink to `~/scripts/hammerspoon/boot.lua`.
The boot file loads every Lua file in `~/scripts/hammerspoon/auto-load/` in
alphabetical order. App-scoped modes should use the shared `ModalMode`
helpers; qView is defined there and exposes `qview_bind_v2` while qView is the
frontmost app.

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
