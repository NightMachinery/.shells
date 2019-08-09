function aget() {
    local u="$(uuidgen)"
    local erri jufile
    mkdir -p "$u" && ec "$(realpath "$u")" >> "$deleteus"
    test -e "$ag_f" && {
        cp "$ag_f" ./"$u"/
        ecdbg ag_f: "$ag_f"
    }
    cd "$u"
    test -e "$ag_f" && jufile=(./*(D))
    ecdbg jufile: "$jufile"
    eval "$@" && {
        wait
        test -n "$ag_no_rm" || {
             cd ..
            \rm -r "$u"
        }
    } || { err="$?" && ecerr aget "$@" exited "$err"; l ; cd .. ; (exit "$err") }
}
function reval() {
    ecdbg revaling "$(gq "$@")"
    eval "$(gq "$@")"
}
function nulterm() {
    reval "$@"
    ec $'\0'
}
