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
powersaving_apps=(bettert iterm chrome hammersp Insiders scsynth Telegram java) # tmux
function powersaving-off() {
    # ffkill -SIGCONT $powersaving_apps
    pgrep -i "${(j.|.)powersaving_apps}" | inargsf kill -SIGCONT
    # proxy-on
    # wgd
}
function powersaving-on() {
    # wgu
    # proxy-off
    
    # ffkill -SIGSTOP $powersaving_apps
    pgrep -i "${(j.|.)powersaving_apps}" | inargsf kill -SIGSTOP
}
##
