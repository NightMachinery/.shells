function sleepnow() ( sleep "${1:-7}"; pmset sleepnow )
function sleepforce() {
    lo_s=60 lo_p=${1:-~/tmp/.sleepforce} loop sleepnow 10
}
function sleepifidle() {
    while (( $(load5) >= ${1:-7} ))
    do
        sleep 150
    done
    ecdate sleeping with load5 $(load5)
    sleepforce
}
##
redis-defvar powersaving_status
powersaving_apps=(iterm chrome Insiders scsynth Telegram java podcast Finder) # bettertouch tmux hammersp bettert
# seems that -SIGSTOP is useless for scsynth
# btt starts itself up again after a minute or two (even with sudo kill)
# emacs daemon can't handle SIGSTOP
function powersaving-off() {
    # ffkill -SIGCONT $powersaving_apps
    pgrep -i "${(j.|.)powersaving_apps}" | inargsf sudo kill -SIGCONT

    # awaysh /Applications/BetterTouchTool.app/Contents/MacOS/BetterTouchTool
    # open /Applications/BetterTouchTool.app

    # proxy-on
    # wgd
    ##
    powersaving_status_set off
    powersaving-widget-refresh
}
function powersaving-on() {
    # wgu
    # proxy-off
    
    # ffkill -SIGSTOP $powersaving_apps
    pgrep -i "${(j.|.)powersaving_apps}" | inargsf sudo kill -SIGSTOP
    # pgrep -i "bettertouch" | inargsf sudo kill
    ##
    powersaving_status_set on
    powersaving-widget-refresh
}
function powersaving-toggle() {
    if powersaving-is ; then
        powersaving-off
    else
        powersaving-on
    fi
}
function powersaving-is() {
    [[ "$(powersaving_status_get)" == on ]]
}
powersaving_widget_uuid=E366290A-FC5D-4913-B068-CE9198F0511B
powersaving_widget_on="🔋"
powersaving_widget_off="💈"
# powersaving_widget_off="🔌"
function powersaving-widget() {
    if powersaving-is ; then
        ec $powersaving_widget_on
    else
        ec $powersaving_widget_off
    fi
}
powersaving-widget-refresh() { btt-update $powersaving_widget_uuid "$(powersaving-widget)" }
##
