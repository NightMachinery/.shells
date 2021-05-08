function h_spotify-discography-get() {
    local url="${1:?}"


    # local title="$(serr url-title $url | sd '\s+' ' ')"
    # ec "$title"
    ec "$url"
}
function spotify-discography-get1() {
    local artist_url="${1}"
    assert-args artist_url @RET

    # `/album/` gets singles, too
    withchrome getlinks-c "${artist_url}/discography" | rg -F /album/ | inargsf re h_spotify-discography-get
}
renog spotify-discography-get1
function spotify-discography-get() {
    spotify-discography-get2 "$@"
}
noglobfn spotify-discography-get
function spotify-discography-get2() {
    local artist_url="${1}"
    assert-args artist_url @RET

    local artist_id
    artist_id="$(ec "$artist_url" | rget '^https://open.spotify.com/artist/([^/]+)/?')" @TRET

    local limit=99999
    local urls=()
    urls+="https://api-partner.spotify.com/pathfinder/v1/query?operationName=queryArtistDiscographyAlbums&variables={\"uri\":\"spotify:artist:${artist_id}\",\"offset\":0,\"limit\":${limit}}&extensions={\"persistedQuery\":{\"version\":1,\"sha256Hash\":\"d1779ae56c892f6d8c8e6abe46d208fec828753bde79974d8dbf59b7ccaee1b4\"}}"
    urls+="https://api-partner.spotify.com/pathfinder/v1/query?operationName=queryArtistDiscographySingles&variables={\"uri\":\"spotify:artist:${artist_id}\",\"offset\":0,\"limit\":${limit}}&extensions={\"persistedQuery\":{\"version\":1,\"sha256Hash\":\"d1779ae56c892f6d8c8e6abe46d208fec828753bde79974d8dbf59b7ccaee1b4\"}}"

    # `operationName=queryArtistDiscographyCompilations` for compilations

    local auth
    auth="$(spotify-auth "$artist_url")" @RET
    dvar auth

    local url
    for url in $urls[@] ; do
        url="$(ecn "$url" | url-encode.py)"

        dact ecgray "$url"$'\n'

        local data
        data="$(curlm "$url" -H "$auth")" || { data="$(revaldbg curl --silent "$url" -H "$auth")" ; ectrace "$0: failed to fetch the data from Spotify" "data: $(gq "$data")" ; return 1 }
        # if we omit --fail from curl, it would return a proper error message in the payload

        ec "$data" | jq -re '.. | .shareUrl? // empty' | command sd '\?[^/]*/?$' ''
    done
}
renog spotify-discography-get2

function spotify-auth() {
    local url="$1"
    assert-args url @RET

    local auth
    auth="$(withchrome full-html2 "$url" | rget '"accessToken"\s*:\s*"([^"]+)"')" || { ectrace "$0: failed to fetch the access token from Spotify" "Note that some server subnets are blocked by Spotify." ; return $? }
    auth="authorization: Bearer ${auth}"

    ec "$auth"
}
##
function rss-engine-spotify() {
    local url="${1}" title="${rssTitle}" receiver="${rss_engine_spotify_r:--1001203291196}"
    test -n "$url" || return 1
    test -n "$title" || title="$(serr url-title $url | sd '\s+' ' ')"


    local log_spotdl="$HOME/logs/$0_spotdl" # use `tail -f` with this
    local log="$HOME/logs/$0"
    ensure-dir "$log" || return $?

    ecdate "${title}: $url" | tee -a $log
    ##
    local date
    # date="$(serr url-date "$url")" && date="$(datenat_unix=y serr datenat "$date")" && test -n "$date" && {
    date="$(spotify-url-get-unix "$url")" && test -n "$date" && {
            if (( EPOCHREALTIME - date > (3600*24*60) )) ; then
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

    fhMode=curl full-html2 "$url" | pup 'meta[property="og:title"] attr{content}'
    ##
    # urlmeta2 "$url" description | rget '^\s*([^·]+)'
}
##
function spotify-url-get-date() {
    local url="${1:?}" out_fmt="${spotify_url_get_date_fmt:-%Y-%B}"

    local date tmp

    date="$(fhMode=curl full-html2 "$url" | pup 'meta[property="music:release_date"] attr{content}')" || return $?
    #  `rget '<meta\s*property="music:release_date"\s*content="([^"]+)"'` also works
    #  withchrome is not strictly necessary, but without it sometimes the tag is not present ...

    dvar date
    if [[ "$date" =~ '^\s*(\d\d\d\d)\s*$' ]] ; then
        date="${match[1]}-1-1" # `gdate --date="2009"` parses wrongly
    else
        # try parsing it ourselves first, as gdate mistakes the position of months and days
        tmp="@$(jalalicli tojalali -j unix -g '2006-01-02' "$date")" && date="$tmp"
    fi
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
