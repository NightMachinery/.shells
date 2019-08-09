
typeset -A memoi_stdout
typeset -A memoi_stderr
typeset -A memoi_time

(( $+functions[ec] )) || {
    ec() print -r -- "$@"
    ecerr() { ec "$@" 1>&2 }
    ecdbg() ecerr "$@"
}
(( $+functions[dvar] )) || function dvar() {
        local pre=''
        test -z "$2" || pre="CODE $2 | "
        echo "$pre$(typeset -p "$1" 2>&1)"
        echo "$pre$1 in env: $(printenv "$1")"
    }

memoi_expire=3600
function memoi-eval() {
    dvar memoi_stdout
    dvar memoi_stdout
    local now="$(date +%s)"
    local cmd="${(q)@}"
    dvar cmd
    { (( $+memoi_time[$cmd] )) && { (( memoi_expire == 0 )) || { ((memoi_expire >= 0 )) && (( (now - memoi_time[cmd]) <= memoi_expire )) } } } && {
        ecdbg Using memoi: "$cmd"
        test -n "$memoi_stdout[$cmd]" && ec "$memoi_stdout[$cmd]"
        test -n "$memoi_stderr[$cmd]" && ecerr "$memoi_stderr[$cmd]"
   } || {
        ecdbg 'Evaling (no memoi): ' "$cmd"
        local errfile="$(mktemp)"
        memoi_stdout[$cmd]="$(eval "$cmd" 2>"$errfile")"
        memoi_stderr[$cmd]=<$errfile
        \rm "$errfile"
        memoi_time[$cmd]="$(date +%s)"
        test -n "$memoi_stdout[$cmd]" && ec "$memoi_stdout[$cmd]"
        test -n "$memoi_stderr[$cmd]" && ecerr "$memoi_stderr[$cmd]"
    }
    dvar memoi_stdout
}
