function hi10-new-check() {
    local query="${1}"
    assert-args query @RET

    local new="$(full-html2 https://hi10anime.com/ | pup '.entry-title a text{}')"
    <<<$new rg --ignore-case "$query" # --quiet
}
function h_hi10-new-notify() {
    local query="${1:?}" # @regex

    local found
    if found="$(hi10-new-check "$query")" ;  then
        tnotifc "hi10: $found"
        # sleep $((3*86400))
        sleep 86400 # = 3600*24
    else
        sleep 86400 # = 3600*24
    fi
}
function hi10-new-notify() {
    loop h_hi10-new-notify "$@"
}
