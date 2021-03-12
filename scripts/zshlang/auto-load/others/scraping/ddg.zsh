function ddg-html() {
    local query="${1}"
    test -n "$query" || return 1
    
    revaldbg eval-memoi withchrome full-html2 'https://duckduckgo.com/?q='"$(ec "$query" | gtr $'\n' ' ' | url-encode.py )"'&kp=-2&kl=us-en'
}
function ddg-json() {
    local q="${1}"
    test -n "$q" || return 1

    ddg-html "$q" | sponge | ddg2json | {
        if isI && isOutTty ; then
            jq .
        else
            cat
        fi
    }
}
