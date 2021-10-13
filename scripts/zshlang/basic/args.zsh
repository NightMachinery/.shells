###
function in-or-args2() {
    if (( $# )) ; then
        inargs=( "$@" )
    else
        if ! isInTty ; then
            inargs="${$(</dev/stdin ; print -n .)[1,-2]}"
        else
            return 1
        fi
    fi
}

function in-or-args() {
    if (( $# )) ; then
        arrN "$@"
    else
        if isInTty ; then
            pbpaste
        else
            print -nr -- "${$(</dev/stdin ; print -n .)[1,-2]}"
        fi
    fi
}

function pcat() {
    possiblycat "${@:-50}"
}
##
function arr0() { print -nr -- "${(pj.\0.)@}" }
function arrN() { print -nr -- "${(pj.\n.)@}" }
alias arrn='arrN'
function arrNN() { print -r -- "${(pj.\n.)@}" }
alias arrnn='arrNN'

# function in-or-args-arr0() {
#     (( $# )) && arr0 "$@" || ec "$(</dev/stdin)"
# }
# function in-or-args-arrN() {
#     (( $# )) && arrN "$@" || ec "$(</dev/stdin)"
# }
###
function args-nochromefile() {
    doc 'Converts file:/// to /. Outputs in $out'
    local arg
    out=()
    for arg in "$@"
    do
        [[ "$arg" =~ '^file://(/.*)$' ]] && out+="$match[1]" || out+="$arg"
    done
}
function rpargs() {
    doc 'Converts all existent paths in args to their abs path. Outputs both in NUL-delimited stdout and $out.'

    unset out

    local i args
    args=()
    for i in "$@"
    do
        test -e "$i" && args+="$(realpath --canonicalize-existing -- "$i")" || args+="$i"
    done
    out=( "${args[@]}" )
    re "printf '%s\0'" "$args[@]"
}
function opts-urls() {
    doc "Partitions the arguments into (global variables) |urls| and |opts| (everything else)."
    
    opts=()
    urls=()
    local i
    for i in "$@" ; do
        if match-url2 "$i" ; then
            urls+="$i"
        else
            opts+="$i"
        fi
    done
}
##
function bool() {
    local i="${1:l}"

    if [[ "${i}" == (n|no|0) ]] ; then
        return 1
    else
        test -n "${i}"
        return $?
    fi
}

function bool-ask {
    local i="${1:l}" ask_args=("${@[2,-1]}")

    if [[ "${i}" == (a|ask) ]] ; then
        ask "${ask_args[@]}"
        return $?
    else
        bool "${i}"
        return $?
    fi
}
##
