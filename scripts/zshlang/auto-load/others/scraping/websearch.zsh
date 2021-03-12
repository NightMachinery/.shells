function googler-en() {
    local ans
    ans="$(googler --tld com --lang en "$@")" || return $?
    # we need to fail proactively to activate the failsafe, ddgr-en:
    if [[ "$ans" =~ '^\s*\[\s*\]\s*$' ]] ; then # empty answer in json
        return 1
    fi
    ec $ans
}
function ddgr-en() {
    ddgr --reg 'us-en' --unsafe "$@"
}
function search-json() {
    local count="${search_json_count:-10}" ddgMode="${search_json_ddg}" query="$1"

    local bad_google="search_json_bad_google" bad_google_val
    bad_google_val="{$(redism get "$bad_google"):-0}" || bad_google_val=0

    if test -z "$ddgMode" && { isDeus || (( ${bad_google_val} <= 3 )) } ; then
       if googler-en --json --count "$count" "$query" ; then
           redism del $bad_google
           return 0
       else
           redism incr $bad_google
           redism expire $bad_google $((3600*24*7))
       fi
    fi
    ddgr-en --json --num "$count" "$query"
}
function goo-g() {
    # use -x, --exact for an exact search.
    unset goo_urls goo_titles goo_asbtracts goo_metadata

    local query="$*"
    local count="${goo_g_count:-${goo_g_c:-10}}"

    local memoi_cmd="$(cmd-sub memoi-eval '')"

    local res search
    search="$(search_json_count="$count" memoi_key="$search_json_ddg|$count" $memoi_cmd search-json "$query")"

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
