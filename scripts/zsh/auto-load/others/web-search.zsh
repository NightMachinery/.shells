function googler-en() {
    googler --tld com --lang en "$@"
}
function ddgr-en() {
    ddgr --reg 'us-en' --unsafe "$@"
}
function search-json() {
    local count="${search_json_count:-10}" query="$1"
    
    googler-en --json --count "$count" "$query" || ddgr-en --json --num "$count" "$query"
}
function goo-g() {
    # use -x, --exact for an exact search.
    unset goo_urls goo_titles goo_asbtracts goo_metadata

    local query="$*"
    local count="${goo_g_count:-${goo_g_c:-10}}"

    local memoi_cmd="$(cmd-sub memoi-eval '')"

    local res search
    search="$(search_json_count="$count" $memoi_cmd search-json "$query")"

    goo_urls=("${(@0)$(<<<$search jq -re --nul-output '.[] | .url')}") || return 1
    goo_titles=("${(@0)$(<<<$search jq -re --nul-output '.[] | .title')}")
    goo_abstracts=("${(@0)$(<<<$search jq -re --nul-output '.[] | .abstract')}")
    goo_metadata=("${(@0)$(<<<$search jq -re --nul-output '.[] | .metadata // empty')}") || true
}
function goo() {
    local goo_urls goo_titles goo_asbtracts goo_metadata
    goo-g "$*" || return 1
    ec ${(F)goo_urls}
}
function google-quote() {
    mapln '"$1"' "$@"
}
##
@s() {
    # @todo Rename me
    googler -j -w 'spotify.com' --url-handler echo "${(@f)$(google-quote "$@")}"
}
##
