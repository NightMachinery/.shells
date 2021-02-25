function time2() {
    local cmd="$(gquote "$@")"
    time2-unquoted "$cmd"
}
function time2-unquoted() {
    local cmd="$*"
    test -z "$*" && return 0
    local time2_start=$EPOCHREALTIME

    eval "$cmd"

    ec $'\n\n'"Took $(( EPOCHREALTIME - time2_start ))"
}
aliasfn time2-uq time2-unquoted
function time-p() {
    time2-unquoted "$(pbpaste)"
}
aliasfn hyp hyperfine --warmup 5
