function hi10-new-check() {
    local query="${1:?}"

    local new="$(full-html2 https://hi10anime.com/ | pup '.entry-title a text{}')"
    <<<$new rg --quiet --ignore-case -F "$query"
}
function h_hi10-new-notify() {
    local query="${1:?}"
    if hi10-new-check "$query" ;  then
        tnotifc "hi10: $query"
        sleep $((3*86400))
    else
        sleep 86400
    fi
}
function hi10-new-notify() {
    loop h_hi10-new-notify "$@"
}
