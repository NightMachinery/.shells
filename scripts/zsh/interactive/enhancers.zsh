function p() {
    local paste="$(pbpaste)"
    local ppaths=( "${(@f)$(clipboard-to-path.nu)}" )
    test -n "$ppaths[*]" && paste=( $ppaths[@] )
    test -n "$paste[*]" && { geval "$(gq "${@}")" "$(gq "$paste[@]")" ; return $? } || >&2 color red Clipboard returned empty.
}
