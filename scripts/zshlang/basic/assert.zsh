## Small public assert. The full local debug module may override this with richer traces.
function assert {
    if (( $#@ == 0 )) ; then
        ecerr "$0: called with no arguments."
        return 1
    fi

    reval "$@" && return 0

    local ret=$?
    ecerr "$0: command failed (${ret}): $(gquote "$@")"
    return $ret
}
