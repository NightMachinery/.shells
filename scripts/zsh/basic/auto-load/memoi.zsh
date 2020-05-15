## Aliases
## Vars
memoi_expire=$(( 3600*24*7 ))
## Functions
function meme() { memoi_expire=$(( $1 * 60 )) reval "$@" }
function memoi-eval() {
    # zmodload zsh/zprof
    local now="$(date +%s)"
    local cmd="$(gq "$@")"

    silent redis-cli --raw ping || { test -n "$memoi_strict" && { ecerr '`redis-cli ping` failed. Please make sure redis is up.' ; return 33 } || eval "$cmd" }
    if { (( $(redis-cli --raw exists $cmd) )) && { (( memoi_expire == 0 )) || { ((memoi_expire >= 0 )) && (( (now - $(redis-cli --raw hget $cmd timestamp)) <= memoi_expire )) } } } 
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
        silent redis-cli hset $cmd exit $?
        print -nr -- "$out" | silent redis-cli -x hset $cmd stdout
        silent redis-cli -x hset $cmd stderr <$errfile
        \rm "$errfile"
        silent redis-cli hset $cmd timestamp "$(date +%s)"
    	}
	fi
	#dbgcmd="$cmd"
    silent redis-cli --raw hexists $cmd stdout && print -nr -- "${$(redis-cli --raw hget $cmd stdout ; print -n .)[1,-3]}"
    silent redis-cli --raw hexists $cmd stderr && print -nr -- "${$(redis-cli --raw hget $cmd stderr ; print -n .)[1,-3]}" >&2
    # zprof
    local retcode="$(redis-cli --raw hget $cmd exit)"
    return $retcode
}
