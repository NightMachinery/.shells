###
alias seval='ge_ecdbg=y geval' #silent debuggable eval
alias eval-good='geval'
alias eval-quoted='reval'
alias greval=rgeval
###
function geval() {
    local cmd="$@"
    test -z "$ge_ecdbg" && {
        test -z "$ge_no_ec"  && ec "$cmd"
        test -z "$ge_no_hist" && print -r -S -- "$cmd" #Add to history
    } || ecdbg "$cmd"
    eval -- "$cmd"
}
function aget() {
    doc "aget does not wait for all forked processed. Probably unsolable unless we invoke zsh -c"
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
    # ecdbg revaling "$(gq "$@")"
    # Don't put stuff here, reval is used in ecdbg itself!
    eval "$(gq "$@")"
}
function rgeval() {
    geval "$(gq "$@")"
}
function psource()
{
    if [[ -r "$1" ]]; then
        source "$1"
    fi
}
