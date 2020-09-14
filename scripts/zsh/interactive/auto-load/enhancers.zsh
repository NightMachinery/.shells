function p() {
    pbpaste-plus # outputs in `paste`
    test -n "$paste[*]" && { geval "$(gq "${@}")" "$(gq "$paste[@]")" ; return $? } || >&2 color red Clipboard returned empty.
}
