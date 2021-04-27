function battery-status-darwin() {
    assert isDarwin @RET

    pmset -g batt | command rg InternalBattery | command column -t
    ##
    # mcli battery status
}
