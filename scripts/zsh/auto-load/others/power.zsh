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

