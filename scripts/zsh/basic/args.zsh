###
function in-or-args2() {
    (( $# )) && inargs=( "$@" ) || inargs="${$(</dev/stdin ; ec .)%.}"
    # https://unix.stackexchange.com/questions/206449/elegant-way-to-prevent-command-substitution-from-removing-trailing-newline
}
function in-or-args() {
    (( $# )) && ec "$@" || ec "${$(</dev/stdin ; ec .)%.}"
}
function arr0() { ec "${(pj.\0.)@}" }
function arrN() { ec "${(pj.\n.)@}" }
function in-or-args-arr0() {
    (( $# )) && arr0 "$@" || ec "$(</dev/stdin)"
}
function in-or-args-arrN() {
    (( $# )) && arrN "$@" || ec "$(</dev/stdin)"
}
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

    local i args
    args=()
    for i in "$@"
    do
        test -e "$i" && args+="$(realpath --canonicalize-existing -- "$i")" || args+="$i"
    done
    out=( "${args[@]}" )
    re "printf '%s\0'" "$args[@]"
}
