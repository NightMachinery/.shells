alias bwg='sudo WG_QUICK_USERSPACE_IMPLEMENTATION=boringtun WG_SUDO=1 wg-quick'
function wgu() {
    local WG_CONF="${1:-$WG_CONF}"

    bwg up "$WG_CONF"
    # networksetup -setdnsservers Wi-Fi 1.1.1.1
}
function wgd() {
    local WG_CONF="${1:-$WG_CONF}"

    bwg down "$WG_CONF"
}
