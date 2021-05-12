##
function wread-bat() {
    unbuffer bat --theme OneHalfLight --pager=never --style=plain "$1" | aha --title "$(basename "$1")"
}
function tlbat() {
    uf_idem=y we_dler="wread-bat" w2e "$(basename "$1")" "$@"
}
##
