erase-ansi() {
    gsed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[mGK]//g"
}
ea() {
    eval "$(gquote "$@")" | erase-ansi
}
