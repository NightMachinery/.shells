function h_spotify-discography-get() {
    local url="${1:?}"


    # local title="$(serr url-title $url | sd '\s+' ' ')"
    # ec "$title"
    ec "$url"
}
function spotify-discography-get() {
    local artist_url="${1:?}"

    # `/album/` gets singles, too
    withchrome getlinks-c "${artist_url}/discography" | rg -F /album/ | inargsf re h_spotify-discography-get
}
renog spotify-discography-get
##
function rss-engine-spotify() {
    local url="${1}" title="${rssTitle}" receiver="${rss_engine_spotify_r:--1001203291196}"
    test -n "$url" || return 1
    test -n "$title" || title="$(serr url-title $url | sd '\s+' ' ')"


    local log_spotdl="$HOME/logs/$0_spotdl"
    local log="$HOME/logs/$0"
    ensure-dir "$log" || return $?

    ecdate "${title}: $url" | tee -a $log
    ##
    local date
    # date="$(serr url-date "$url")" && date="$(datenat_unix=y serr datenat "$date")" && test -n "$date" && {
    date="$(spotify-url-get-unix "$url")" && test -n "$date" && {
            if (( EPOCHREALTIME - date > (3600*24*365) )) ; then
                ecerr "$0: Skipping '$title' because of age '$(spotify-url-get-date "$url")'" 2>&2 2>>$log
                return 0
            fi
    }
    ##
    # local d
    # d="$(spotify-url-get-year "$url")" && {
    #     local y="$(gdate '+%Y')"
    #     if (( y - d >= 2 )) ; then
    #         ecerr "$0: Skipping '$title' because of age '$d'"
    #         return 0
    #     fi
    # }
    ##

    local dir="${deleteusdir:?}/music/$title $(md5m "$url")/" files
    pushf "$dir"
    {
        gtimeout --kill-after=15m --verbose 24h spotdl "$url" &>> "$log_spotdl" || {
            local ret=$?
            local msg="$0: spotdl failed with '$ret' for '$title' '$url'"
            ecerr $msg
            tsend "$receiver" "$msg"
            return $ret
        }
        jup
        ltl
        files=(*.mp3(DN))
        if (( ${#files} == 0 )) ; then
            tsend "$receiver" "$0: No files were found for '$title' '$url'"
            return 1
        fi
        reval-rainbow tsendf "$receiver" $files[@] |& tee -a $log || {
            local ret=$?
            local msg="$0: tsendf failed with '$ret' for '$title' '$url'"
            ecerr $msg
            tsend "$receiver" "$msg"
            return $ret
        }
    } always { popf ; trs-rm "$dir" }
}
##
function spotify-url-get-artist() {
    local url="${1:?}"

    full-html2 "$url" | pup 'meta[property="og:title"] attr{content}'
    ##
    # urlmeta2 "$url" description | rget '^\s*([^·]+)'
}
##
function spotify-url-get-date() {
    local url="${1:?}" out_fmt="${spotify_url_get_date_fmt:-%Y-%B}"

    date="$(eval-memoi full-html2 "$url" | pup 'meta[property="music:release_date"] attr{content}')" || return $?
    gdate --date="$date" "+$out_fmt"
}
function spotify-url-get-year() {
    @opts fmt '%Y' @ spotify-url-get-date "$@" || return $?
    ##
    # urlmeta2 "$url" description | rget '\s(\d\d\d\d)\s'
}
function spotify-url-get-unix() {
    @opts fmt '%s' @ spotify-url-get-date "$@" || return $?
}
##
spotify-artist-fz() {
    local query="${*:?}"
    local count="${spotify_artist_fz_c:-6}"

    local urls url name
    urls=("${(@f)$(ddg_json_js=y ffgoo_count=$count ffgoo "$query site:https://open.spotify.com/artist/")}")  || {
        local ret=$?
        ecerr "$0: ffgoo failed with '$ret'"
        return $ret
    }
    for url in $urls[@]
    do
        name="$(spotify-url-get-artist "$url")" || name="$query"
        ec "artists['$name']='$url'"
    done
}
function spotify-artist-fz-re() {
    local o="$(run-on-each spotify-artist-fz "$@")"
    ec 
    ec-copy "$o"
}
##
