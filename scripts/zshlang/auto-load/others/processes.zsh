function pkill9 {
    pgrep -i "$@"
    pkill -9 -i "$@"
}
aliasfn pk pkill9
##
function ps-children() {
    pgrep -P "$1"
}
function ps-grandchildren() {
    : "usage: ps-grandchildren <pid>; prints every descendant of <pid>, one per line: a process's children, then each child's own descendants in turn"
    #: One `ps' snapshot walked in memory. A `pgrep -P' per process scans
    #: the whole process table each time, so BrishGarden's tree (500+
    #: processes) took over a minute, and [agfi:tmux2kitty-stop] hung on it.
    ##
    local line
    local -a f
    local -A ps_grandchildren_kids
    for line in ${(f)"$(command ps -Ao pid=,ppid=)"} ; do
        f=( ${=line} )
        ps_grandchildren_kids[$f[2]]+=" $f[1]"
    done

    h-ps-grandchildren-walk "$1"
}

function h-ps-grandchildren-walk {
    #: Reads `ps_grandchildren_kids' from [agfi:ps-grandchildren]'s scope.
    local -a children=( ${(n)=ps_grandchildren_kids[$1]} )
    local pid

    arrN "$children[@]" '' # The output's ordering crucially depends on the position of this statement

    for pid in $children[@] ; do
        h-ps-grandchildren-walk "$pid"
    done
}

function kill-withchildren() {
    local sig=-15 # TERM
    if [[ "$1" =~ '^-\S+$' ]] ; then
        sig="$1"
        shift
    fi
    local pids=("$@") pid

    local children
    for pid in $pids[@] ; do
        children=("${(@f)$(ps-grandchildren "$pid")}")
        revaldbg kill $sig $pid $children[@] # `kill ''` will kill itself in noninteractive zsh (at least does so in brishz)
    done
}
##
function renice-me() {
  renice -n "${1:-10}" $$
}
function nice-get() {
  gnice
  # ps -l -p "${1:-$$}"
}
##
