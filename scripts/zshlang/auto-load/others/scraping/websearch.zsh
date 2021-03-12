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
    bad_google_val="${$(redism get "$bad_google"):-0}" || bad_google_val=0
    dvar bad_google_val

    if test -z "$ddgMode" && { isDeus || (( ${bad_google_val} <= 3 )) } ; then
       if googler-en --json --count "$count" "$query" ; then
           silent redism del $bad_google
           return 0
       else
           silent redism incr $bad_google
           silent redism expire $bad_google $((3600*24*7))
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
function ffgoo() {
    local query="$*"
    local count="${ffgoo_count:-${ffgoo_c:-25}}"

    setopt local_options
    setopt pipefail

    local fzf_cmd="$(cmd-sub fz fzf)"
    local memoi_cmd="$(cmd-sub memoi-eval '')"
    # local isI="$(cmd-sub isI true)"
    # local fz_opts=( "$fz_opts[@]" )

    local search="$(search_json_count="$count" $memoi_cmd search-json "$query")"
    local is i
    is=("${(@f)$(<<<$search jq -re '.[] | .title + ": " + (.abstract |= gsub("\\n";" ") + " " + .url).abstract + (if .metadata then " (" + (.metadata) + ")" else "" end)' |cat -n | SHELL=dash $fzf_cmd --multi --preview 'printf -- "%s " {}' --preview-window up:7:wrap --with-nth 2.. | awk '{print $1}')}") || return 1
    for i in $is[@] ; do
        i=$((i-1)) # jq is zero-indexed
        <<<$search jq -re ".[$i] | .url"
    done
}
##
@s() {
    # @todo Rename me
    googler -j -w 'spotify.com' --url-handler echo "${(@f)$(google-quote "$@")}"
}
##
