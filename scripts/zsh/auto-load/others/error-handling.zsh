function retry-eval() {
    retry-limited-eval 0 "$@"
}

function retry-limited() {
    retry-limited-eval "$1" "$(gquote "${@:2}")"
}
function retry-limited-eval() {
    local limit=0
    local ecode=0
    until {test "$1" -gt 0 && test $limit -ge "$1"} || { eval "${@:2}" && ecode=0 }
    do
        ecode="$?"
        ecerr Tried eval "${@:2}" "..."
        sleep 1
        limit=$((limit+1))
    done
    # test $limit -lt "$1" || test "$1" -eq 0
    (exit "$ecode")
}
alifn retry='retry-limited 0'
