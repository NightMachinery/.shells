erase-ansi-old() {
    gsed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[mGK]//g"
}
function erase-ansi() {
    strip-ansi
}
eea() {
    reval "$@" | erase-ansi
}
