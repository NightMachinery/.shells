function autosuggestions-goo() {
    curl-useragent "http://suggestqueries.google.com/complete/search?client=firefox&q=$(ecn "$*"|url-encode.py)" | jqm '.[1] | .[]' || autosuggestions-ddg "$@"
}
function autosuggestions-ddg() {
    curl-useragent "https://duckduckgo.com/ac/?q=$(ecn "$*"|url-encode.py)" | jqm '.[]|.phrase'
}
function autosuggestions-gateway() {
    set -- "$(trim "$*")"
    test -z "$*" && {
        if test -z "$autosuggestions_gateway_flag" ; then
            autosuggestions_gateway_flag=y $0 "$(pbpaste)"
            return $?
        else
            return 0
        fi
    }
    autosuggestions-goo "$@"
}
aliasfn asg autosuggestions-gateway
aliasfn as autosuggestions-gateway # @nameconfilct (macOS)
function jias() {
    # autosuggestions-gateway "$@" | jiarr
    local i results=() url
    for i in ${(@f)"$(autosuggestions-gateway "$@")"}
    do
        url="$(<<<"$i" url-encode.py)"
        url="[$i](https://www.google.com/search?q=$url)" # @todo md-quote i (don't use url-encode as it is very ugly)
        results+="$(jq --null-input --compact-output --arg i "${i}" --arg url "$url" '{ tlg_parsemode: "md", tlg_preview: "", tlg_title: $i, tlg_content: $url }')"
    done
    arrJ "$results[@]"
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
    assert cp -f "$google_history" /tmp/h
    local links
    links="$(sqlite3 -separator $sep /tmp/h \
        "select substr(title, 1, $cols), url
     from urls order by last_visit_time desc" |
        awk -F $sep '{printf "%-'$cols's  '$s2'\n", $1, $2}')" || {
        ectrace @RET
    }
    if test -n "$query" ; then
        links="$(ec "$links" | rgbase "$query")" @RET # beware: rgbase always colors
    fi
    ec $links
}
function chis_find() {
    # Forked from fzf's wiki
    # browse Chrome's history
    local rg_enabled="$chis_rg" exact="${chis_exact:-y}" # @warn fzf is very bad at searching URLs fuzzily, so you probably never want to disable 'exact'
    local query='' fz_query='' memoi_cmd=''
    typeset -a opts
    if isI ; then
        opts+='--ansi'
    fi
    if bool $exact ; then
        opts+='--exact'
    fi
    if bool $rg_enabled ; then # @warn hammerspoon needs this to be false
        query="${*}"
    else
        if bool $exact ; then
            fz_query="$*"
        else
            fz_query="$*"
        fi
        memoi_cmd='memoi-eval'
    fi

    # ` sponge | ghead -n 100 |`
    local links
    links="$(revaldbg $memoi_cmd chrome-history-get "$query"| fzp --no-sort "$opts[@]" "$fz_query")" @RET
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
    local links
    links="$(chis_find "$@" | chis_clean)" @RET

    ec "$links"|pbcopy
    [[ "${doOpen}" == y ]] && {
        local i
        for i in ${(@f)links}
        do
            chrome-open "$i" >&2 &|
        done

        # @todo2 give focus to Chrome
    }
}
alias ffchrome=chis
##
