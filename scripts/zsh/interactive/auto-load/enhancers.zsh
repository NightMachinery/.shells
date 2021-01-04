function p() { # paste
    pbpaste-plus # outputs in `paste`
    test -n "$paste[*]" && { geval "$(gq "${@}")" "$(gq "$paste[@]")" ; return $? } || >&2 color red Clipboard returned empty.
}
##
function enh-addfinder() {
    local sel
    sel=("${(@f)$(finder-selection-get.as)}") || return $?
    test -z "$sel[*]" && return 1
    rgeval "$@" $sel[@]
}
aliasfn pf enh-addfinder
##
