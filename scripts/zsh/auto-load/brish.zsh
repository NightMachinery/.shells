function brishz() {
    # local opts=()
    ## old GET
    # isDbg && opts+=(--data 'verbose=1')
    # rgeval curl --silent -G $opts[@] --data-urlencode "cmd=$(gq "$@")" http://127.0.0.1:8000/zsh/
    ##
    # httpie is slow
    # isDbg && opts+=(verbose=1)
    # http --body POST http://127.0.0.1:8000/zsh/ cmd="$(gq "$@")" $opts[@]
    ##
    local v=0
    isDbg && v=1
    local req="$(jq --null-input --compact-output --arg c "$(gq "$@")" --arg v "$v" '{"cmd": $c, "verbose": $v}')"
    local cmd=( curl --header "Content-Type: application/json" --request POST --data "$req" http://127.0.0.1:8000/zsh/ )
    cmd="$(gq "$cmd[@]")"
    <<<"$cmd" pbcopy
    eval "$cmd"
}
