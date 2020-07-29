typeset -g GARDEN_PORT=7230
function brishz() {
    local opts=()
    ## old GET
    # isDbg && opts+=(--data 'verbose=1')
    # rgeval curl --silent -G $opts[@] --data-urlencode "cmd=$(gq "$@")" http://127.0.0.1:8000/zsh/
    ##
    # httpie is slow
    # isDbg && opts+=(verbose=1)
    # http --body POST http://127.0.0.1:8000/zsh/ cmd="$(gq "$@")" $opts[@]
    ##
    local endpoint="${bzEndpoint:-http://127.0.0.1:$GARDEN_PORT}"
    if [[ "$endpoint" =~ 'garden' ]] ; then
        opts+=(--user "Alice:$GARDEN_PASS0")
    fi
    local v=0
    isDbg && v=1
    local req="$(jq --null-input --compact-output --arg c "$(gq "$@")" --arg v "$v" '{"cmd": $c, "verbose": $v}')"
    local cmd=( curl $opts[@] --fail --silent --location --header "Content-Type: application/json" --request POST --data "$req" $endpoint/zsh/ )
    cmd="$(gq "$cmd[@]")"
    <<<"$cmd" pbcopy
    eval "$cmd"
}
aliasfn brishzr bzEndpoint=https://garden.lilf.ir/api/v1 brishz
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
