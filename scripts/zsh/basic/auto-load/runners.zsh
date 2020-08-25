function rexx(){
    xargs -d " " -n 1 -I _ "$=1" <<< "${@:2}"
}
function rex(){
    zargs -i _ -- "${@:2}" -- "$=1"
    #Using -n 1 fails somehow. Probably a zargs bug.
}
function rexa(){
    local i
    for i in "${@:2}"
    do
        eval "$(sed -e "s/_/${i:q:q}/g" <<< "$1")" #sed itself needs escaping, hence the double :q; I don't know if this works well.
    done
}
redo-eval() {
    local i
    for i in {1.."${@: -1}"}
    do
        eval "${@: 1:-1}"
    done
}
redo() redo-eval "$(gquote "${@: 1:-1}")" "${@: -1}"
redo2() { redo "$@[2,-1]" "$1" }
function redo-async() {
    local cmd=( "$@[2,-1]" ) n="$1" i
    for i in {1..$n}; do
        reval "$cmd[@]" &
    done
}
##
function skipglob() {
    if test -n "${*[2,-1]}" ; then
        # ecdbg "$0: ${*[2,-1]}"
        eval "$1 $(gq "${@:2}")"
    fi
}
aliasfn skipemptyargs skipglob
aliasfn skipargs skipglob
function skipemptyin() {
    # whitespace doesn't matter in this application, and in fact needs to be ignored
    # local in="${$(</dev/stdin ; print -n .)[1,-2]}"
    local in="$(</dev/stdin)"
    test -z "$in" || { print -nr -- "$in" | reval "$@" }
}
aliasfn skipin skipemptyin
##
function indir() {
    # we are not handling the autocompletion system at all
    local origfile="$1" dir="$(bottomdir "$1")" cmd=("${@:2}")
    { test -e "$origfile" && test -d "$dir" } || {
        ecerr "$0: '$origfile' is invalid (probably doesn't exist)"
        return 1
    }

    if test -z "$cmd[*]" ; then
        cd "$dir"
        return 0
    elif [[ "$cmd[1]" == 'cd' ]] ; then
        cd "$dir"
        reval "${cmd[@]}"
        return $?
    fi

    pushf "$dir"
    {
        reval "${cmd[@]}"
    } always { popf }
}
aliasfn in indir # best reserved for interactive use
##
function reval-notifexit() {
    # always alone is not sufficient. Test with `zsh -c 'reval-notifexit iterm_focus.py'`.
    # But now always is most probably redundant.
    setopt localtraps
    trap "" INT
    { ( reval "$@" ) } always {
        # bello
        notif "$0: $@"
    }
}
##
