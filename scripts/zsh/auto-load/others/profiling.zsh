function time2() {
    local cmd="$(gquote "$@")"
    test -z "$*" && return 0
    local time2_start=$EPOCHREALTIME

    eval "$cmd"

    ec "Took $(( EPOCHREALTIME - time2_start ))"
}
