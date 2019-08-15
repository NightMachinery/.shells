function p() {
    local paste="$(pbpaste)"
    test -n $paste && geval "$(gq "${@}")" "$(gq $paste)" || ecerr Clipboard returned empty.
}
