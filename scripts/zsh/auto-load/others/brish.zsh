function brishz() {
    local stdin="$brishz_in"
    local brishz_copy=""
    isI && brishz_copy=y
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
function h2e-stdin() {
    h2e "$@" =(</dev/stdin)
}
function w2e-rpaste() {
    local html="$(pbpaste)"
    brishz_in="$html" brishzr h2e-stdin "$@"
}
aliasfn weep w2e-rpaste
##
aliasfn bsh brishz
aliasfn bshr brishzr # You can also use .a
aliasfn brishz-all brishz %GARDEN_ALL
aliasfn brishzr-all brishzr %GARDEN_ALL
aliasfn brishz-restart brishz-all %BRISH_RESTART
aliasfn brishzr-restart brishzr-all %BRISH_RESTART
aliasfn xl brishz-restart
aliasfn xr brishzr-restart
##
