@s() {
    googler -j -w 'spotify.com' --url-handler echo "${(@f)$(google-quote "$@")}"
}
google-quote() {
    mapln '"$1"' "$@"
}
