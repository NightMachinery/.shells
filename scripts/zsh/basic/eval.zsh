###
alias seval='ge_ecdbg=y geval' #silent debuggable eval
alias evaldbg='seval'
alias eval-good='geval'
alias eval-quoted='reval'
alias greval=rgeval
eval-ec() { ge_no_hist=y geval "$@" }
reval-ec() { ge_no_hist=y rgeval "$@" }
reval-ecdate() {
    test -z "$*" && return 0
    local cmd="$(gq "$@")"
    ecdate "$cmd"
    eval "$cmd"
}
###
function geval() {
    test -z "$*" && return 0
    local cmd="$@"
    test -z "$ge_ecdbg" && {
        test -z "$ge_no_ec"  && ec "$cmd" >&2
        test -z "$ge_no_hist" && print -r -S -- "$cmd" #Add to history
    } || ecdbg "$cmd"
    eval -- "$cmd"
}
function aget() {
    ##
    # "aget does not wait for all forked processed. Probably unsolvable unless we invoke zsh -c"
    # `zsh -f -c 'sleep 3 &'` doesn't wait for them either. We had this problem in borg's old forking system as well.
    ##
    local cmd=("$@")

    local u="./$(uuidgen)" erri jufile j jd
    mkdir -p "$u"
    u="$(realpath "$u")"
    jd="$u"
    ec $u >> "$deleteus"
    test -e "$ag_f" && {
        cp "$ag_f" "$u"/
        ecdbg ag_f: "$ag_f"
    }
    cd "$u"
    test -e "$ag_f" && {
        jufile=(./*(D))
        j="$jufile"
    }
    ecdbg jufile: "$jufile"
    if eval "$cmd[@]" ; then
        wait
        test -n "$ag_no_rm" || {
            cd ..
            command rm -r "$u"
        }
    else
        err="$?" && ecerr "aget $cmd[@] exited '$err'. Listing files (can be empty):"
        exa -a -l
        cd ..
        return "$err"
    fi
}
function reval() {
    # ecdbg revaling "$(gquote "$@")"
    # Don't put stuff here, reval is used in ecdbg itself!
    local cmd="$(gquote "$@")"
    test -z "$*" && return 0 # Don't throw an error, it throws some other stuff up :|
    eval "$cmd"
}
function rgeval() {
    geval "$(gquote "$@")"
}
function psource()
{
    if [[ -r "$1" ]]; then
        source "$1"
    fi
}
