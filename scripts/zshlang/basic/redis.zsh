###
function ensure-redis {
    (( ${+commands[redis-cli]} )) || {
        ecerr "redis-cli not found. Have you installed redis?"
        return 2
    }

    [[ "${$(redism ping):l}" == pong ]] || {
        ecerr '`redis-cli ping` failed. Please make sure redis is up.'
        return 1
    }
}
function redis-assert {
    ensure-redis "$@"
}

function redism {
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
