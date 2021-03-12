function ddg-html() {
    local query="${1}"
    test -n "$query" || return 1
    
    # @docs https://duckduckgo.com/params
    fhMode=curlfullshort revaldbg eval-memoi full-html2 'https://duckduckgo.com/?q='"$(ec "$query" | gtr $'\n' ' ' | url-encode.py )"'&kp=-2&kl=us-en'
    # majority of time of curlfull is spent on loading puppeteer, I guess
}
function ddg-json() {
    local q="${1}" count="${2:-10}" js_mode="${ddg_json_js}"
    test -n "$q" || return 1

    if test -n "$js_mode" ; then
        ddg-html "$q" | sponge | ddg2json | {
            if isI && isOutTty ; then
                jq .
            else
                cat
            fi
        }
    else
        ddgr-en --json --num "$count" "$q" # much faster
    fi
}
