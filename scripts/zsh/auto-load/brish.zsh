function brishZ() {
    local opts=()
    isDbg && opts+=(--data 'verbose=1')
    rgeval curl -G $opts[@] --data-urlencode "cmd=$(gq "$@")" http://127.0.0.1:8000/zsh/
}
