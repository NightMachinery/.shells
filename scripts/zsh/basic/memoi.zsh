## Aliases
## Vars
memoi_expire=$(( 3600*24*7 ))
## Functions
function mem() { memoi_expire=$(( $1 * 60 )) reval "$@" }
function memoi-eval() {
    # typeset -Ag memoi_stdout
    # typeset -Ag memoi_stderr
    # typeset -Ag memoi_timestamp
    # typeset -Ag memoi_exit
    # typeset -Ag memoi_debug
    local now="$(date +%s)"
    local cmd="$(gq "$@")"

    silent redis-cli --raw ping || { test -n "$memoi_strict" && { ecerr '`redis-cli ping` failed. Please make sure redis is up.' ; return 33 } || eval "$cmd" }
    { (( $(redis-cli --raw exists $cmd) )) && ecdbg ice0 && { (( memoi_expire == 0 )) || { ((memoi_expire >= 0 )) && ecdbg fire0 && (( (now - $(redis-cli --raw hget $cmd timestamp)) <= memoi_expire )) && ecdbg fire1 } } && ecdbg monkey0 } && {
        dact fsay using memoi
        ecdbg Using memoi: "$cmd"
        silent redis-cli --raw hexists $cmd stdout && ec "$(redis-cli --raw hget $cmd stdout)"
        silent redis-cli --raw hexists $cmd stderr && ec "$(redis-cli --raw hget $cmd stderr)"
        (exit 0)
   } || {
        dact fsay memoi not fresh enough
        ecdbg 'Evaling (no memoi): ' "$cmd"
        local errfile="$(mktemp)"
        local out
        out="$(eval "$cmd" 2>"$errfile")"
        silent redis-cli hset $cmd exit $?
        <<<"$out" silent redis-cli -x hset $cmd stdout
        silent redis-cli -x hset $cmd stderr <$errfile
        \rm "$errfile"
        silent redis-cli hset $cmd timestamp "$(date +%s)"
        silent redis-cli --raw hexists $cmd stdout && ec "$(redis-cli --raw hget $cmd stdout)"
        silent redis-cli --raw hexists $cmd stderr && ec "$(redis-cli --raw hget $cmd stderr)"
    }
    return $(redis-cli --raw hget $cmd exit)
}
