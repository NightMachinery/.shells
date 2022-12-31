##
aliasfn displaysleep-darwin pmset displaysleepnow
aliasfn displaysleep displaysleep-darwin
##
function display-off-brightness {
	local dur="${1:-1}"

	lo_s="$dur" loop brightness-set 0
}

function display-off {
	local after="${1:-0}"

	sleep-neon "$after"
    displaysleep
	#: causes display to go to sleep immediately.
}
##
function sleepnow {
    sleep "${1:-7}"
    pmset sleepnow
}

function sleepforce {
    lo_s=60 lo_p=${1:-~/tmp/.sleepforce} loop sleepnow 10
}

function sleepifidle {
    while (( $(load5) >= ${1:-7} ))
    do
        sleep 150
    done
    ecdate sleeping with load5 $(load5)
    sleepforce
}
##
function battery-low-power-mode-enable {
    #: [[https://apple.stackexchange.com/questions/452488/how-can-i-set-the-low-power-mode-to-only-on-battery-programmatically][macos - How can I set the low power mode to "Only On Battery" programmatically? - Ask Different]]
    ecgray "@seeAlso powersaving-on"
    ##

    sudo pmset -b lowpowermode 1
    #: The -a, -b, -c, -u flags determine whether the settings apply to battery ( -b ), charger (wall power) ( -c ), UPS ( -u ) or all ( -a ).
}

function battery-low-power-mode-disable {
    sudo pmset -a lowpowermode 0
}

function battery-low-power-mode-p {
    local s
    s="$(pmset -g | rget 'lowpowermode\s+(\d+)')" @TRET

    (( s == 1 )) #: 1: lowpowermode 0: off
}

redis-defvar powersaving_status
powersaving_apps=(chrome Insiders bettertouch ActivityWatch) # aw-watcher aw-server iterm tmux hammersp Notion Finder scsynth java podcast Telegram
# seems that -SIGSTOP is useless for scsynth
# btt starts itself up again after a minute or two (even with sudo kill)
# emacs daemon can't handle SIGSTOP
function powersaving-off {
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

function powersaving-on {
    # ffkill -SIGSTOP $powersaving_apps
    pgrep -i "${(j.|.)powersaving_apps}" | inargsf sudo kill -SIGSTOP
    ##
    powersaving_status_set on
    # powersaving-widget-refresh
}

function powersaving-toggle {
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
