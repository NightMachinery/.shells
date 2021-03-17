function brishz() {
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
aliasfn bsh-er bshEndpoint=https://garden.lilf.ir/api/v1 # bsh eval remote
function brishzr() {
    ensure-net "$0" || return $?
    if isLilf ; then
        reval "$@"
    else
        bsh-er brishz "$@"
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
    reval-ec curl --fail --silent --header 'Content-Type: application/json' --request POST --data '{"cmd":"ec hi","verbose":"0"}' http://127.0.0.1:7230/zsh/
    reval-ec curl --fail --silent --header 'Content-Type: application/json' --request POST --data '{"cmd":"ec hi","verbose":"0"}' "http://$(ip-internal-get1 | ghead -1):7230/zsh/"
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
aliasfn brishz-all reval-bell-lm-mhm brishz %GARDEN_ALL
aliasfn brishzr-all reval-bell-lm-mhm brishzr %GARDEN_ALL
aliasfn brishz-restart brishz-all %BRISH_RESTART
aliasfn brishzr-restart brishzr-all %BRISH_RESTART
aliasfn xl brishz-restart
aliasfn xr brishzr-restart
## tests:
# dbg bsh-er brishz2.dash '%GARDEN_ALL pwd ; typ a ; a=12'
# @todesign Solve this deadlock problem, so that we can use $GARDEN_ALL from within a brish command.
# brishzq.zsh brishz-all ec This will deadlock
# brishzq.zsh awaysh brishz-all ec This will not deadlock
##
