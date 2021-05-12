function wread-man() {
    local m=""
    m="$(MAN_KEEP_FORMATTING=1 COLUMNS=70 serr man "$1")" && m="$(<<<"$m" command ul)" || m="$(2>&1 "$1" --help)" || { ecerr "$0 failed for $1" ; return 1 }
    <<<"$m" aha --title "$1"
}

function tlman() {
    uf_idem=y we_dler="wread-man" w2e "$1" "$@"
}
##
