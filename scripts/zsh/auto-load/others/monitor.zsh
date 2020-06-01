function ppgrep() {
    case "$(uname)" in
        Darwin)
            \pgrep -i "$@" | gxargs --no-run-if-empty ps -fp
            ;;
        Linux)
            \pgrep "$@" | gxargs --no-run-if-empty ps -fp
            # Linux's pgrep doesn't support -i
            ;;
    esac
}
function lsofp() {
    ppgrep "$@" | fz --header-lines 1 | awk '{print $2}' | inargsf re "lsof -p" | less
}
aliasfn fflsof lsofp
aliasfn plsof lsofp

function jprocs() {
    jah procs -c always "$@"
}
function jprocs-pic() {
    procs "$@" | convert -page  4000x4000 -font FreeMono -pointsize 20 -fill black -background white -trim +repage -bordercolor white  -border 15 text:- png:"$0 $*".png
    jdoc
}
function getidle-darwin() {
    ioreg -c IOHIDSystem | awk '/HIDIdleTime/ {print $NF/1000000000; exit}'
}
function  getlastunlock-darwin() {
    date="$(log show --style syslog --predicate 'process == "loginwindow"' --debug --info --last 1d | command rg "going inactive, create activity semaphore|releasing the activity semaphore" | tail -n1 |cut -c 1-31)" fromnow
}
function ecdate() { ec "$edPre$(color 100 100 100 $(dateshort))            $@" }
function load5() {
    # 1 5 15 minutes
    sysctl -n vm.loadavg | awk '{print $3}'
}
