# Paqet Zsh Plugin

A Zsh wrapper for running a local Paqet client in `tmux`. It updates the `network:` block in a Paqet config with the current interface, local IPv4 address, and gateway/router MAC address before starting Paqet.

## Dependencies

Load the basic plugin before this plugin:

```zsh
source /path/to/.shells/zshlang/basic/basic.plugin.zsh
source /path/to/.shells/zshlang/plugins/paqet/paqet.plugin.zsh
```

External commands:

- Required for normal use: `zsh`, `paqet`, `tmux`, `curl`, `perl`, `sudo`
- Linux/Ubuntu detection: `iproute2` (`ip`), `iputils-ping` (`ping`), optional `net-tools` (`arp` fallback)
- macOS detection: built-in `route`, `ipconfig`, `arp`, `ping`

Paqet uses raw sockets, so `paqet-on` starts it with `sudo`.

## Commands

```zsh
paqet-on [config_path]
paqet-off
paqet-proxy-listen-get [config_path]
paqet-config-network-update <config_path> <interface> <local_ipv4> <router_mac>
```

Default config path:

```zsh
${HOME}/paqet/config.yaml
```

## Configuration overrides

Set these variables before calling `paqet-on` if auto-detection is wrong:

```zsh
paqet_config_path=~/paqet/config.yaml
paqet_binary=/path/to/paqet
paqet_session_name=paqet-client
paqet_interface=eth0
paqet_local_ip=192.168.1.100
paqet_gateway_ip=192.168.1.1
paqet_router_mac=aa:bb:cc:dd:ee:ff
paqet_proxy_listen=127.0.0.1:1040
paqet_ip_test_url=https://api.ipify.org
paqet_startup_sleep=1
```

Detection precedence is: explicit override, then platform-specific detection, then an error.

## Ubuntu notes

The plugin uses standard Linux commands:

```zsh
ip -4 route get 1.1.1.1       # interface and local source IP
ip -4 route show default      # gateway IP
ip neigh show <gateway> dev <interface>
arp -n <gateway>              # fallback if net-tools is installed
```

Install the common dependencies with:

```sh
sudo apt install iproute2 iputils-ping tmux curl perl
# optional arp fallback:
sudo apt install net-tools
```

## Plugin-manager examples

Replace `NightMachinery/.shells` if you install from another fork or mirror. Load `night-basic` first, then `paqet`.

### Antidote

```text
NightMachinery/.shells path:zshlang/basic
NightMachinery/.shells path:zshlang/plugins/paqet
```

### Zimfw

```zsh
zmodule NightMachinery/.shells --root zshlang/basic --source basic.plugin.zsh
zmodule NightMachinery/.shells --root zshlang/plugins/paqet --source paqet.plugin.zsh
```

### Sheldon

```toml
[plugins.night-basic]
github = "NightMachinery/.shells"
use = ["zshlang/basic/basic.plugin.zsh"]

[plugins.paqet]
github = "NightMachinery/.shells"
use = ["zshlang/plugins/paqet/paqet.plugin.zsh"]
```

### Zinit / Zi

```zsh
zi ice id-as"night-basic" pick"zshlang/basic/basic.plugin.zsh"
zi light NightMachinery/.shells

zi ice id-as"night-paqet" pick"zshlang/plugins/paqet/paqet.plugin.zsh"
zi light NightMachinery/.shells
```

### Zplug

```zsh
zplug "NightMachinery/.shells", use:"zshlang/basic/basic.plugin.zsh zshlang/plugins/paqet/paqet.plugin.zsh"
```
