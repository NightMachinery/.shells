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
function isdefined() {
    local sym="$1"

    test -n "$sym" && (( $+commands[$sym] || $+functions[$sym] || $+aliases[$sym] ))
}
function cmd-sub() {
    local cmd="$1" sub="$2"

    if isdefined "$cmd" ; then
        print -nr -- "$cmd"
    else
        print -nr -- "$sub"
    fi
}
##
function ffgoo() {
    local query="$*"
    local count="${ffgoo_count:-${ffgoo_c:-30}}"

    setopt local_options
    setopt pipefail

    local fzf_cmd="$(cmd-sub fz fzf)"
    local memoi_cmd="$(cmd-sub memoi-eval '')"

    local search="$($memoi_cmd googler --json --count "$count" "$query")"
    local is i
    is=("${(@f)$(<<<$search jq -re '.[] | .title + " | " + .abstract + " | " + .metadata' |cat -n | SHELL=dash $fzf_cmd --multi --preview 'printf -- "%s " {}' --preview-window up:7:wrap --with-nth 2.. | awk '{print $1}')}") || return 1
    for i in $is[@] ; do
        i=$((i-1)) # jq is zero-indexed
        <<<$search jq -re ".[$i] | .url"
    done
}
