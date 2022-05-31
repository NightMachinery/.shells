## Aliases
alias deus='deusvult=y '
## Vars
memoi_expire=$(( 3600*24*7 ))
## Functions
function isDeus() {
  test -n "$deusvult"
}
aliasfn isdeus isDeus

function expirein() {
  memoi_expire=$(( $1 * 60 )) reval "$@"
}

function memoi-eval() {
###
# GLOBAL OUT (as always these do not survive  a fork): memoi_cache_used
#    doc "[memoi_expire=<seconds> memoi_strict= memoi_key= deusvult= memoi_skiperr= memoi_od=] $0 cmd...
# memoi_key is used to differentiate commands that, e.g., depend on an env var that may change.
# deusvult forces expiration and a reevaluation. It is a global override. It also voids skiperr.
# skiperr skips storing stderr and the return code, and also doesn't ping redis to see if it's alive.
# "
## perf
# Test using:
# `time (memoi_skiperr=y memoi-eval fd "${fd_default[@]}" "${args[@]:-.}" "$(realpath ".")" > /dev/null )`
###
    # zmodload zsh/zprof

    unset memoi_cache_used
    #: needs zmodload zsh/datetime loaded
    local now=$EPOCHREALTIME #"$(date +%s)"
    local cmd="$(gquote "$@")"
    local custom_key="$memoi_key"
    local rediskey="$custom_key|> $cmd"
    local deusvult="$deusvult"
    local skiperr="$memoi_skiperr"
    local aborterr="$memoi_aborterr"
    # test -n "$deusvult" && skiperr='' # uncommenting this can help make `deus` transparently undo the memoi effect. But I am not sure we want that.

    local inheriterr="${memoi_inheriterr:-$skiperr}"
    #: 'inheriterr' skips storing the stderr.
    #: skipping forces inheritance, because why not?

    local override_duration="${memoi_override_duration:-${memoi_od:-0.12}}" # 0.12

    local retcode=0 # If skiperr, we will return the correct exit code if the cache is not used. (We rely on this behavior in, e.g., `ffz`.
    local delme=''

    test -n "$skiperr" || ensure-redis || {
        test -n "$memoi_strict" && { ectrace "$0: redis is down" ; return 33 } || {
        eval "$cmd"
        return $?
        }
        }
    if test -z "$deusvult" && {
        integer ttl="$(redism ttl $rediskey)"
        (( ttl > 10 || ttl == -1 )) # && {
        # (( memoi_expire == 0 )) || { ((memoi_expire >= 0 )) && (( (now - $(redism hget $rediskey timestamp)) <= memoi_expire )) }
        # }
        }
    then
        # dact fsay using memoi
        # ec Using memoi: "$cmd"
        memoi_cache_used=y
    else
        # dact fsay memoi not fresh enough
        # ec 'Evaling (no memoi): ' "$cmd"

        # Redirecting to the real stderr lets us use, e.g., fzf
        local errfile=/dev/stderr #/dev/null
        { test -z "$inheriterr" } && errfile="$(mktemp)"
        local out
        out="${$(eval "$cmd" 2>"$errfile" ; ret_code=$? ; print -n . ; return $ret_code)[1,-2]}" ; retcode=$?
        local duration=$(( EPOCHREALTIME - now ))
        if { test -z "$aborterr" || (( retcode == 0 )) } && (( duration >= override_duration )) ; then
            ##
            # by not storing the time if the command executed quickly, we'll ensure a re-eval the next time.
            # silent redis-cli hset $rediskey timestamp "$now"
            ##
            (( memoi_expire > 0 )) && silent redis-cli expire $rediskey "$memoi_expire"
        else
            ##
            # ecdbg "Command took $duration. Setting its timestamp to 0 so that the next evaluation will not use the cache."
            # silent redis-cli hset $rediskey timestamp "0"
            ##
            delme=y
        fi
        print -nr -- "$out" | silent redis-cli -x hset $rediskey stdout
        if test -z "$skiperr" ; then
            silent redis-cli hset $rediskey exit $retcode
            { test -z "$inheriterr" } && {
            silent redis-cli -x hset $rediskey stderr <$errfile
            command rm "$errfile"
            }
        fi
    fi

    # silent redism hexists $rediskey stdout && print -nr -- "${$(redism hget $rediskey stdout ; print -n .)[1,-3]}"
    redism hexists $rediskey stdout &> /dev/null && redism hget $rediskey stdout # outputting directly to stdout is much faster (300ms vs 70ms). Note that redis-cli outputs an extra newline, which can be corrected for using ` | ghead -n -1`, but this costs an additional 30ms.
    if test -z "$skiperr" ; then
        { test -z "$inheriterr" } && {
        silent redism hexists $rediskey stderr && print -nr -- "${$(redism hget $rediskey stderr ; print -n .)[1,-3]}" >&2
        }
        # zprof
        retcode="$(redism hget $rediskey exit)"
    fi
    ##
    # There are concurrency issues with just deleting the key (someone might be in the middle of using it.)
    test -n "$delme" && silent redis-cli expire "$rediskey" 1 # silent redis-cli del $rediskey
    ##
    return $retcode
}
@opts-setprefix memoi-eval memoi

function eval-memoi() {
  memoi-eval "$@"
}
enh-savename eval-memoi memoi-eval
@opts-setprefix eval-memoi memoi

function reval-memoi() {
  memoi-eval "$@"
}
enh-savename reval-memoi memoi-eval
@opts-setprefix reval-memoi memoi

function me() {
  memoi-eval "$@"
}
enh-savename me memoi-eval
@opts-setprefix me memoi
##
