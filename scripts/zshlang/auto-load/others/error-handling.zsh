function retry-eval() {
    retry-limited-eval 0 "$@"
}

function retry-limited() {
    retry-limited-eval "$1" "$(gquote "${@:2}")"
}
function retry-limited-eval() {
    local retry_sleep="${retry_sleep:-0.1}"

    local limit=0
    local ecode=0
    until {test "$1" -gt 0 && test $limit -ge "$1"} || { eval "${@:2}" && ecode=0 }
    do
        ecode="$?"
        ecerr Tried eval "${@:2}" "..."
        sleep $retry_sleep
        limit=$((limit+1))
    done
    # test $limit -lt "$1" || test "$1" -eq 0
    return "$ecode"
}
alifn retry='retry-limited 0'
##
function failnoisily() {
    reval "$@"
    local r=$?
    if (( r )) ; then
        notif "$0: $(gq "$@") (returned $r) "
    fi
}

function failnoisily-if-net {
    assert-net @RET

    failnoisily "$@"
}
##
