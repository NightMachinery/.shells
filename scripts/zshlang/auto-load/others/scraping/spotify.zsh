function h_spotify-discography-get() {
    local url="${1:?}"


    local title="$(serr url-title $url)"
    ##
    # local date
    # date="$(serr url-date "$url")" && date="$(datenat_unix=y serr datenat "$date")" && test -n "$date" && {
    #         if (( EPOCHREALTIME - date > (3600*24*365) )) ; then
    #             ecerr "$0: Skipping '$title' because of age"
    #             return 0
    #         fi
    # }
    ##
    local d
    d="$(urlmeta2 "$url" description | rget '\s(\d\d\d\d)\s')" && {
        local y="$(gdate '+%Y')"
        if (( y - d >= 2 )) ; then
            ecerr "$0: Skipping '$title' because of age '$d'"
            return 0
        fi
    }
    ##

    ec "$title"
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
    local url="${1:?}" title="${rssTitle}" receiver="${rss_engine_spotify_r:--1001203291196}"

    local log="$HOME/logs/$0"
    ensure-dir "$log" || return $?

    local dir="${deleteusdir:?}/music/$title $(md5m "$url")/"
    pushf "$dir"
    {
        spotdl "$url" &>> "$log" || {
            local ret=$?
            local msg="$0: spotdl failed with '$ret' for '$title' '$url'"
            ecerr $msg
            tsend "$receiver" "$msg"
            return $ret
        }
        jup
        ltl
        revaldbg tsendf "$receiver" *.mp3(DN) || {
            local ret=$?
            local msg="$0: tsendf failed with '$ret' for '$title' '$url'"
            ecerr $msg
            tsend "$receiver" "$msg"
            return $ret
        }
    } always { popf ; trs-rm "$dir" }
}
