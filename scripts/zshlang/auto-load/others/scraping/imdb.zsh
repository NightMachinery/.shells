##
function imdb-ids-get {
    local urls
    urls=("${(@f)$(cat-paste-if-tty)}") @RET

    local ids=()
    local url r=0
    for url in $urls[@] ; do
        if [[ "$url" =~ 'https://www.imdb.com/title/(tt\d+)/*' ]] ; then
            ids+="${match[1]}"
        else
            ecerr "$0: could not parse URL $(gq "$url")"
            r=1
        fi
    done

    arrnn ${ids[@]}

    return $r
}
##
function imdb1() {
    : "DEPRECATED: Use the imdb function"

    imdbpy search movie --first "$*"
}

function imdb() {
    # @publishme : needs jq, dash, awk, googler, imdbpy, pcre-enabled zsh
    local query="$* site:imdb.com"

    setopt local_options
    setopt re_match_pcre
    setopt pipefail

    local isI="$(cmd-sub isI true)"
    local count=4
    $isI && count=20

    local urls url
    urls=("${(@f)$(ffgoo_count=$count ffgoo "$query")}") || return 1
    for url in $urls[@] ; do
        [[ "$url" =~ 'https://www.imdb.com/title/tt(\d+)/?' ]] && {
            imdbpy get movie "$match[1]"
        }
    done
}
##
