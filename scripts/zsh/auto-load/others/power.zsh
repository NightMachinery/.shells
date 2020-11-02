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
powersaving_apps=(iterm chrome Insiders scsynth Telegram java podcast Finder bettertouch) # tmux hammersp bettert
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
}
function powersaving-on() {
    # wgu
    # proxy-off
    
    # ffkill -SIGSTOP $powersaving_apps
    pgrep -i "${(j.|.)powersaving_apps}" | inargsf sudo kill -SIGSTOP
    # pgrep -i "bettertouch" | inargsf sudo kill
}
##
