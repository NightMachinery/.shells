function battery-status-darwin-full() {
    assert isDarwin @RET

    pmset -g batt | command rg InternalBattery | command column -t
    ##
    # mcli battery status
}

function battery-status-darwin() {
    battery-status-darwin-full | rget '(\d+%);'
}
