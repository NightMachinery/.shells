function p() {
    local paste="$(pbpaste)"
    test -n "$paste" && { geval "$(gq "${@}")" "$(gq "$paste")" ; return $? } || >&2 color red Clipboard returned empty.
}
