###
function h-redis-auth-ensure {
    #: On multi-user hosts redis must require a password: binding to 127.0.0.1
    #: excludes other *hosts* but not the other *users* of the same machine,
    #: who could otherwise read our history and brishgarden state.
    #:
    #: `redis-cli` picks the password up from $REDISCLI_AUTH, which the shell
    #: normally exports at startup. Re-read it here so that shells started
    #: *before* the password existed - and any process that inherited a stale
    #: environment - heal themselves instead of failing every redis call with
    #: NOAUTH.
    ##
    if test -z "${REDISCLI_AUTH}" && test -r ~/.redis-auth ; then
        export REDISCLI_AUTH="$(<~/.redis-auth)"
    fi
}

function ensure-redis {
    (( ${+commands[redis-cli]} )) || {
        ecerr "redis-cli not found. Have you installed redis?"
        return 2
    }

    h-redis-auth-ensure

    [[ "${$(redism ping):l}" == pong ]] || {
        ecerr '`redis-cli ping` failed. Please make sure redis is up.'
        return 1
    }
}
function redis-assert {
    ensure-redis "$@"
}

function redism {
    h-redis-auth-ensure
    revaldbg redis-cli --raw "$@"
    local r=$?
    if (( r == 141 )) ; then
        local cmd="$(gq "$0" "$@")"
        local msg="$0: redis returned $r (is stdout a bad pipe?). Cmd: $cmd"
        ##
        # ecerr $msg
        # ectty $msg
        ##
        ectrace_ret=$r ectrace "$msg"
        ##
        # eval "$cmd"
        return $r
    fi
    return $r
}

function redism-bool {
    local o
    o="$(redism "$@")" @TRET

    if (( o == 1 )) ; then #: success
        return 0
    else
        return 13
    fi
}
##
function redis-defvar {
    local name="${1}"

    if test -z "${name}" ; then
        ecerr "$0: name not supplied"
        return 1
    fi

    fndef "${name}_get" redism get "$name"
    aliasfnq "${name}_set" silent redism set "$name"
    aliasfnq "${name}_setnx" silent redism setnx "$name"
    fndef "${name}_del" silent redism del "$name"
}
##
