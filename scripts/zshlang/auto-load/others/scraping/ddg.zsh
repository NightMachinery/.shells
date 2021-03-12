function ddg-html() {
    local query="${1}"
    test -n "$query" || return 1
    
    eval-memoi withchrome full-html2 'https://duckduckgo.com/?q='"$(ec "$query" | gtr $'\n' ' ' | url-encode.py )"'kp=-2&kl=us-en'
}
function ddg-json() {
    local url="${1}"
    test -n "$url" || return 1

    ddg-html "$url" | sponge | ddg2json | {
        if isI && isOutTty ; then
            jq .
        else
            cat
        fi
    }
}
