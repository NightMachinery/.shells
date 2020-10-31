function autosuggestions-goo() {
    curl-useragent "http://suggestqueries.google.com/complete/search?client=firefox&q=$(ecn "$*"|url-encode.py)" | jqm '.[1] | .[]' || autosuggestions-ddg "$@"
}
function autosuggestions-ddg() {
    curl-useragent "https://duckduckgo.com/ac/?q=$(ecn "$*"|url-encode.py)" | jqm '.[]|.phrase'
}
##
function ffgoo() {
    local query="$*"
    local count="${ffgoo_count:-${ffgoo_c:-30}}"

    setopt local_options
    setopt pipefail

    local fzf_cmd="$(cmd-sub fz fzf)"
    local memoi_cmd="$(cmd-sub memoi-eval '')"
    # local isI="$(cmd-sub isI true)"
    # local fz_opts=( "$fz_opts[@]" )

    local search="$($memoi_cmd googler-en --json --count "$count" "$query")"
    local is i
    is=("${(@f)$(<<<$search jq -re '.[] | .title + " | " + .abstract + " | " + .metadata' |cat -n | SHELL=dash $fzf_cmd --multi --preview 'printf -- "%s " {}' --preview-window up:7:wrap --with-nth 2.. | awk '{print $1}')}") || return 1
    for i in $is[@] ; do
        i=$((i-1)) # jq is zero-indexed
        <<<$search jq -re ".[$i] | .url"
    done
}
##
function chrome-history-get() {
    local query="${*}"

    local cols sep google_history s2='%s'
    cols=$(( COLUMNS / 3 ))
    sep='{::}'
    if isI ; then
        s2='\x1b[36m%s\x1b[m'
    fi

    if [ "$(uname)" = "Darwin" ]; then
        google_history="$HOME/Library/Application Support/Google/Chrome/Default/History"
    else
        google_history="$HOME/.config/google-chrome/Default/History"
    fi
    cp -f "$google_history" /tmp/h
    local links="$(sqlite3 -separator $sep /tmp/h \
        "select substr(title, 1, $cols), url
     from urls order by last_visit_time desc" |
        awk -F $sep '{printf "%-'$cols's  '$s2'\n", $1, $2}')"
    if test -n "$query" ; then
        links="$(ec "$links" | rgbase "$query")" # beware: rgbase always colors
    fi
    ec $links
}
function chis_find() {
    # Forked from fzf's wiki
    # browse Chrome's history
    local query='' fz_query='' memoi_cmd=''
    typeset -A fz_opts
    if isI ; then
        query="${*}"
        fz_opts+='--ansi'
    else
        fz_query="$*"
        memoi_cmd='memoi-eval'
    fi

    # ` sponge | ghead -n 100 |`
    local links="$(revaldbg $memoi_cmd chrome-history-get "$query"| fzp --exact --no-sort "$fz_query")"
    ec "$links"
}
@opts-setprefix chis_find chis
function chis_clean() {
    {
        if (( $#@ == 0 )) ; then
            cat
        else
            ec "${(F)@}"
        fi
    } | gsed 's#.*\(https*://\)#\1#'
}
function chis() {
    local doOpen="${chis_open:-y}"
    local links="$(chis_find "$@" | chis_clean)"

    ec "$links"|pbcopy
    [[ "${doOpen}" == y ]] && {
        local i
        for i in ${(@f)links}
        do
            open "$i"
        done
    }
}
alias ffchrome=chis
##
