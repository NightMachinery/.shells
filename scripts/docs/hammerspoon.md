# Hammerspoon

`~/.hammerspoon/init.lua` includes a `hyper+w` Wi-Fi chooser.

The chooser scans nearby Wi-Fi networks in the background, marks the currently
connected SSID with `*`, and toggles the selected network. Selecting the current
network disconnects from Wi-Fi. Selecting another network runs:

```sh
networksetup -setairportnetwork <interface> <ssid>
```

This works best for open or previously remembered networks. New protected
networks may still need credentials added through macOS first.
