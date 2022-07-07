##
function battery-status-darwin-full() {
    assert isDarwin @RET

    pmset -g batt | command rg InternalBattery | command column -t
    ##
    # mcli battery status
}

function battery-status-darwin() {
    battery-status-darwin-full | rget '(\d+%);'
}
##
function power-monitor-darwin {
    sudo powermetrics -i 1000 --poweravg 1 | grep 'Average cumulatively decayed power score' -A 20
}
##
