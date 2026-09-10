##
function brishgarden-boot {
    #: @duplicateCode/9d5c6cada217b9c834ffb54708013acb
    ##
    tmuxnewsh2 BrishGarden BRISHGARDEN_DEBUGME="$BRISHGARDEN_DEBUGME" BRISHGARDEN_N="${1:-256}" brishgarden "${@[2,-1]}" # using the shell to increase max open files
    ## tests:
    # `time (parallel_jobs=0 para 'sleep 1 ; ec {}' ::: {1..100} >/dev/null)`
    # `time (parallel_jobs=0 para -k 'sleep 1 ; ec {}' ::: {1..100} >/dev/null)`
    # `parallel_jobs=0 time2 para 'sleep 5 ; ec ${brish_server_index}: {}' ::: {1..100}`
    # `parallel_jobs=0 time2 para 'ec ${brish_server_index}: {}' ::: {1..200}`
    ##
}

function brishgarden-count() {
    fnswap isI false ffps BRIIIII | wc -l
}

function brishz-alive-p {
    #: Whether the garden is answering, without running a command through it.
    #: For callers that want to delegate *if they can* and do the work
    #: themselves otherwise, since a failed [agfi:brishzq.zsh] cannot be told
    #: apart from a command that legitimately exited nonzero.
    #:
    #: A TCP probe rather than an HTTP request: the endpoint is POST-only and
    #: authenticated, so a liveness request would have to carry the API key and
    #: could still be refused for reasons unrelated to the garden being up.
    ##
    #: A remote garden is not probed -- the endpoint may be behind Caddy, a
    #: tunnel or a name that does not resolve here -- so assume the caller who
    #: set it knows. See =docs/api-keys.md= on the tunnelling gotcha.
    test -z "${bshEndpoint}" || return 0

    #: The same default as the clients use; see [agfi:brishz]. Kept in step by
    #: hand because =brishzq.zsh= is a standalone wrapper, not sourced from
    #: here.
    local host="${brishz_alive_host:-127.0.0.1}" port="${GARDEN_PORT:-7230}"

    zmodload zsh/net/tcp 2>/dev/null || return 1

    ztcp "${host}" "${port}" 2>/dev/null || return 1
    ztcp -c "${REPLY}" 2>/dev/null

    return 0
}
##
function brishz {
    ## PERF:
    # `hyperfine --warmup 5 'brishzq.zsh ec hi' "brishz_quote=y brishz.dash 'ec hi'" "brishz_quote='' brishz.dash 'ec hi'"` 81ms, 34ms, 24ms
    ##
    local stdin="$brishz_in"
    local -x brishz_copy="${brishz_copy:-$brishz_c}"
    local -x brishz_session="${brishz_session:-$brishz_s}"
    local -x brishz_nolog="${brishz_nolog}"
    # isI && brishz_copy=y
    
    if test -z "$stdin" ; then
        brishzq.zsh "$@"
    else
        print -nr -- "$stdin" | brishz_in='MAGIC_READ_STDIN' brishzq.zsh "$@"
    fi
}

function brishz-in {
    brishz_in="$(cat)" brishz "$@"
}

aliasfn bsh-er bshEndpoint=https://garden.lilf.ir/api/v1 # bsh eval remote
function brishzr {
    ensure-net "$0" || return $?
    if isLilf ; then
        reval "$@"
    else
        $proxyenv bsh-er brishz "$@"
    fi
}
## @security @tests
function garden-req() {
    # We spoof our IP here, to see if the server is fooled.
    local opts=()
    isDbg && opts+='-v'
    curl $opts[@] --fail --silent --location --user "Alice:$GARDEN_PASS0" 'https://garden.lilf.ir/api/v1/request/'"$1" --header "X-Forwarded-For: 1.2.3.4"
}
aliasfn garden-ip garden-req 'ip/'
function brishz-tests-nonlocal-access() {
    #: Expected: `hi`, then `401`, then a connection failure.
    ec "--- local, with the API key (expected: hi)"
    reval-ec curl --fail --silent --header "@$HOME/.keys/brishgarden" --header 'Content-Type: application/json' --request POST --data '{"cmd":"ec hi","verbose":"0"}' http://127.0.0.1:7230/zsh/

    ec "--- local, keyless (expected: 401)"
    reval-ec curl --silent --output /dev/null --write-out '%{http_code}\n' --header 'Content-Type: application/json' --request POST --data '{"cmd":"ec hi","verbose":"0"}' http://127.0.0.1:7230/zsh/

    #: [agfi:ip-internal-get1] lists loopback first, so filter it out; otherwise
    #: this "nonlocal" check silently tests 127.0.0.1.
    local nonlocal_ip
    nonlocal_ip="$(ip-internal-get1 | rg -v '^127\.' | ghead -1)"
    if test -z "$nonlocal_ip" ; then
        ecerr "$0: no non-loopback address found; skipping the nonlocal check"
        return 0
    fi

    ec "--- non-loopback address ${nonlocal_ip} (expected: connection refused)"
    #: A firewall usually drops these silently, so cap the wait.
    reval-ec curl --fail --silent --max-time 5 --header 'Content-Type: application/json' --request POST --data '{"cmd":"ec hi","verbose":"0"}' "http://${nonlocal_ip}:7230/zsh/"
}
##
function caddypass() {
    caddy hash-password -algorithm scrypt -salt "$GARDEN_SALT0" -plaintext "$GARDEN_PASS0"
    # remember to base64:
    # export GARDEN_SALT0_B64="$(print -nr -- "$GARDEN_SALT0" | base64)"
}
##
aliasfn bsh brishz
aliasfn bshr brishzr # You can also use .a
function brishz-all() {
    brishz_noquote=y reval-bell-sc2-nav_online brishz "%GARDEN_ALL $(gquote-simple "$@")"
}
function brishzr-all() {
    brishz_noquote=y @opts bell bell-sc2-activating_bots @ reval-bell brishzr "%GARDEN_ALL $(gquote-simple "$@")"
}
aliasfn brishz-restart brishz-all %BRISH_RESTART
aliasfn brishzr-restart brishzr-all %BRISH_RESTART
aliasfn xl brishz-restart
aliasfn xr brishzr-restart
## tests:
# dbg bsh-er brishz2.dash '%GARDEN_ALL pwd ; var-show a ; a=12'
# @todesign Solve this deadlock problem, so that we can use $GARDEN_ALL from within a brish command.
# brishzq.zsh brishz-all ec This will deadlock
# brishzq.zsh awaysh brishz-all ec This will not deadlock
##
