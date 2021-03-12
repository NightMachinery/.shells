function h_spotify-discography-get() {
    local url="${1:?}"


    local title="$(url-title $url)"
    local date
    date="$(url-date "$url")" && date="$(datenat_unix=y datenat "$date")" && {
            if (( EPOCHREALTIME - date > (3600*24*365) )) ; then
                ecerr "$0: Skipping '$title' because of age"
                return 0
            fi
    }
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

    local dir="$HOME/tmp/music/$title $(md5m "$url")/"
    pushf "$dir"
    {
        spotdl "$url" || {
            local ret=$?
            local msg="$0: spotdl failed with '$ret' for '$title' '$url'"
            ecerr $msg
            tsend "$receiver" "$msg"
            return $ret
        }
        jup
        tsendf "$receiver" *(DN) || return $?
    } always { popf ; trs-rm "$dir" }
}
