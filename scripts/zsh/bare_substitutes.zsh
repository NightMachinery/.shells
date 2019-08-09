(( $+functions[ec] )) || {
    ec() print -r -- "$@"
    ecerr() { ec "$@" 1>&2 }
    ecdbg() ecerr "$@"
    gq() { ec "${(q+@)@}" }
}
(( $+functions[dvar] )) || function dvar() {
        local pre=''
        test -z "$2" || pre="CODE $2 | "
        echo "$pre$(typeset -p "$1" 2>&1)"
        echo "$pre$1 in env: $(printenv "$1")"
    }
