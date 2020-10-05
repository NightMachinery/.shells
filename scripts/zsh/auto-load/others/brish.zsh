function brishz() {
    local stdin="$brishz_in"
    local brishz_copy="${brishz_copy:-$brishz_c}"
    # isI && brishz_copy=y
    if test -z "$stdin" ; then
        brishz_copy="$brishz_copy" brishzq.zsh "$@"
    else
        print -nr -- "$stdin" | brishz_in='MAGIC_READ_STDIN' brishz_copy="$brishz_copy" brishzq.zsh "$@"
    fi
}
aliasfn bsh-er bshEndpoint=https://garden.lilf.ir/api/v1
aliasfn brishzr bsh-er brishz
function garden-req() {
    # We spoof our IP here, to see if the server is fooled.
    local opts=()
    isDbg && opts+='-v'
    curl $opts[@] --fail --silent --location --user "Alice:$GARDEN_PASS0" 'https://garden.lilf.ir/api/v1/request/'"$1" --header "X-Forwarded-For: 1.2.3.4"
}
aliasfn garden-ip garden-req 'ip/'
##
function caddypass() {
    caddy hash-password -algorithm scrypt -salt "$GARDEN_SALT0" -plaintext "$GARDEN_PASS0"
    # remember to base64:
    # export GARDEN_SALT0_B64="$(print -nr -- "$GARDEN_SALT0" | base64)"
}
##
aliasfn bsh brishz
aliasfn bshr brishzr # You can also use .a
aliasfn brishz-all brishz %GARDEN_ALL
aliasfn brishzr-all brishzr %GARDEN_ALL
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
