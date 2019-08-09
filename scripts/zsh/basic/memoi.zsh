## Aliases
## Vars
memoi_expire=$(( 3600*24*7 ))
## Functions
function mem() { memoi_expire=$(( $1 * 60 )) reval "$@" }
function memoi-eval() {
    typeset -Ag memoi_stdout
    typeset -Ag memoi_stderr
    typeset -Ag memoi_timestamp
    typeset -Ag memoi_exit
    typeset -Ag memoi_debug

    dvar memoi_exit
    local now="$(date +%s)"
    local cmd="$(gq "$@")"
    re dvar cmd memoi_expire memoi_timestamp now
    { (( $+memoi_timestamp[$cmd] )) && ecdbg ice0 && { (( memoi_expire == 0 )) || { ((memoi_expire >= 0 )) && ecdbg fire0 && (( (now - memoi_timestamp[$cmd]) <= memoi_expire )) && ecdbg fire1 } } && ecdbg monkey0 } && {
        dact fsay using memoi
        ecdbg Using memoi: "$cmd"
        test -n "$memoi_stdout[$cmd]" && ec "$memoi_stdout[$cmd]"
        test -n "$memoi_stderr[$cmd]" && ecerr "$memoi_stderr[$cmd]"
        (exit 0)
   } || {
        dact fsay memoi not fresh enough
        ecdbg 'Evaling (no memoi): ' "$cmd"
        local errfile="$(mktemp)"
        memoi_stdout[$cmd]="$(eval "$cmd" 2>"$errfile")"
        memoi_exit[$cmd]=$?
        memoi_stderr[$cmd]="$(<$errfile)"
        \rm "$errfile"
        memoi_timestamp[$cmd]="$(date +%s)"
        test -n "$memoi_stdout[$cmd]" && ec "$memoi_stdout[$cmd]"
        test -n "$memoi_stderr[$cmd]" && ecerr "$memoi_stderr[$cmd]"
    }
    dvar memoi_exit
    # dvar memoi_stdout
    memoi_debug[$cmd]=yes
    memoi_debug[apple]=yes
    return $memoi_exit[$cmd]
}
