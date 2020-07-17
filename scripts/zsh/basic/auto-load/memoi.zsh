## Aliases
## Vars
memoi_expire=$(( 3600*24*7 ))
## Functions
function meme() { memoi_expire=$(( $1 * 60 )) reval "$@" }
function memoi-eval() {
    doc "[memoi_expire=<seconds> memoi_strict= memoi_key= deusvult=] $0 cmd...
memoi_key is used to differentiate commands that, e.g., depend on an env var that may change.
deusvult forces expiration and a reevaluation. It is a global override.
"

    # zmodload zsh/zprof
    local now="$(date +%s)"
    local cmd="$(gq "$@")"
    local custom_key="$memoi_key"
    local rediskey="$custom_key|> $cmd"
    local deusvult="$deusvult"

    silent redis-cli --raw ping || { test -n "$memoi_strict" && { ecerr '`redis-cli ping` failed. Please make sure redis is up.' ; return 33 } || eval "$cmd" }
    if test -z "$deusvult" && { (( $(redis-cli --raw exists $rediskey) )) && { (( memoi_expire == 0 )) || { ((memoi_expire >= 0 )) && (( (now - $(redis-cli --raw hget $rediskey timestamp)) <= memoi_expire )) } } }
    then
    {
        # dact fsay using memoi
        # ec Using memoi: "$cmd"
    }
    else
    {
        # dact fsay memoi not fresh enough
        # ec 'Evaling (no memoi): ' "$cmd"
        local errfile="$(mktemp)"
        local out
        out="${$(eval "$cmd" 2>"$errfile" ; print -n .)[1,-2]}"
        silent redis-cli hset $rediskey exit $?
        print -nr -- "$out" | silent redis-cli -x hset $rediskey stdout
        silent redis-cli -x hset $rediskey stderr <$errfile
        \rm "$errfile"
        silent redis-cli hset $rediskey timestamp "$(date +%s)"
    }
    fi
    #dbgcmd="$cmd"
    silent redis-cli --raw hexists $rediskey stdout && print -nr -- "${$(redis-cli --raw hget $rediskey stdout ; print -n .)[1,-3]}"
    silent redis-cli --raw hexists $rediskey stderr && print -nr -- "${$(redis-cli --raw hget $rediskey stderr ; print -n .)[1,-3]}" >&2
    # zprof
    local retcode="$(redis-cli --raw hget $rediskey exit)"
    return $retcode
}
