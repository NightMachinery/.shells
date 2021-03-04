# redis-defvar wireguard_enabled

alias bwg='sudo WG_QUICK_USERSPACE_IMPLEMENTATION=boringtun WG_SUDO=1 wg-quick'
function wgu() {
    local WG_CONF="${1:-$WG_CONF}"

    bwg up "$WG_CONF"
    # networksetup -setdnsservers Wi-Fi 1.1.1.1
    wg-widget-refresh
}
function wgd() {
    local WG_CONF="${1:-$WG_CONF}"

    bwg down "$WG_CONF"
    sleep 0.1 ; wg-widget-refresh # otherwise thinks the net is down
}
##
function wg-toggle() {
    if isNet && isIran ; then
        wgu
    else
        wgd
    fi
}
function wg-widget() {
    if ! isNet ; then
        # ec "🧑🏻‍🚀"# this emoji did not display
        ec "🧞‍♀️"
        return
    fi
    if isIran ; then
        ec "🦸🏻"
    else
        ec "🦹🏻"
    fi
}
wg_widget_uuid='D6B3C80D-4678-4B0A-9F4E-1C95B6871830'
wg-widget-refresh () {
    btt-update $wg_widget_uuid "$(wg-widget)"
}
##
