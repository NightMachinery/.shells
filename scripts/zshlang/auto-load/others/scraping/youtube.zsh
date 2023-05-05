##
# https://github.com/ytdl-org/youtube-dl#format-selection-examples
aliassafe ybase="noglob youtube-dl --no-playlist --write-sub --sub-lang en --prefer-ffmpeg"

aliassafe y="ybase --embed-subs --add-metadata --external-downloader aria2c --external-downloader-args '-c -j 3 -x 3 -s 3 -k 1M'" #  --embed-thumbnail errs: Only mp3 and m4a/mp4 are supported for thumbnail embedding for now. Causes only the first URL to be downloaded (possibly because of the error.)

aliassafe 'y@pl'="ytdl_opts=(--yes-playlist  -o '%(playlist_index)03d. %(title)s.%(ext)s') "

aliassafe ysmall="y -f '(bestvideo[height<=800]+bestaudio/best[height<=800]/best)[protocol^=http]'"
aliassafe ys="ysmall"
# youtube-dl sometimes exits on error instead of retrying (possibly always) # aria2 will not get used for DASH

aliassafe y1080="y -f '(bestvideo[height<=1200]+bestaudio/best[height<=1200]/best)'"
aliassafe yy='y1080'

# ysmp4 still can output an mkv. Probably because of merging?
aliassafe ysmp4="y -f '(bestvideo[ext=mp4][height<=800]+bestaudio/best[ext=mp4][height<=800]/best[ext=mp4]/best)'"

aliassafe ymp4="y -f 'bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best'"

aliassafe yarc="noglob retry ysmall --download-archive ~/.yarchive"

aliassafe yic='y --ignore-config' #--external-downloader-args "-s 4"'

aliassafe yaudio="noglob youtube-dl --no-playlist -f 'bestaudio[ext=m4a]/bestaudio'"

aliassafe yaudio-playlist='yaudio --yes-playlist'

aliassafe ymp3='noglob youtube-dl --no-playlist --prefer-ffmpeg --extract-audio --audio-format mp3'

aliassafe ymp3-playlist='ymp3 --yes-playlist'
# `-f best` to download single file
##
function youtube-dl {
    local cookie_mode="${youtube_dl_c}"
    typeset -ga ytdl_opts # has priority over args

    local head
    # head='youtube-dl'
    head='yt-dlp'

    local opts=(--no-abort-on-error)
    if [[ "$head" == 'yt-dlp' ]] ; then
        opts+=(--compat-options no-live-chat,no-keep-subs)
    fi

    isI || opts+=( --quiet --no-progress )

    if bool $cookie_mode ; then
        opts+=(--add-header "$(cookies-auto "$@")")
        # opts+=(--add-header "$(referer-auto "$@")")
        opts+=(--add-header "Referer: $(browser-current-url)")
    fi

    ##
    # if should-proxy-p ; then
    #     opts+=(--proxy 'socks5://127.0.0.1:1081')
    # fi
    ##

    if isSSH ; then # urlfinalg takes too much time on Iran's net.
        transformer urlfinalg "revaldbg $proxycmd $head $opts[@]" "$@" "$ytdl_opts[@]"
    else
        ##
        # revaldbg $proxycmd youtube-dl "$opts[@]" "$@" "$ytdl_opts[@]"
        ##
        revaldbg $proxyenv command "$head" "$opts[@]" "$@" "$ytdl_opts[@]"
    fi
}
##
function youtube-dl-format-fz {
    local ffull
    ffull="$(youtube-dl -F --quiet "$@")" @TRET
    ffull="$(ec "$ffull" | fz --header-lines=2)" @RET

    local f
    f="$(<<<$ffull gawk '{print $1}')" @TRET
    assert test -n "$f" @RET

    [[ "$ffull" =~ 'video only' ]] && f+="+bestaudio"

    rgeval retry youtube-dl -f "$f" "$@"
}
alias yf='youtube-dl-format-fz'

function ylist() {
    youtube-dl -j --flat-playlist "$@" | jq -r '"https://youtu.be/\(.id)"'
}
noglobfn ylist
function ytitle() {
    youtube-dl --get-filename -o "%(title)s" "$@"
}
##
function ytrans() {
    doc "youtube transcript"
    doc "@alt night/org-insert-youtube-video-with-transcript"
    ##
    local url="$1"
    local title=("${(@f)$(youtube-dl --get-filename -o "%(title)s" "$url")}")
    # local u="$(md5m "$title")"
    pushf "$title"
    {
        youtube-dl --convert-subs vtt --write-auto-sub --skip-download --sub-lang en "$1"
        vtt2txt2.py *.vtt | gtr $'\n' ' ' > ../"$title.txt"
    } always {
        popf
    }
    command rm -r "$title"

    t2e "$title" "$title.txt" # txt2epub
}
renog ytrans
##
function streamlink-m() {
    streamlink --player='mpv' "$@" 720p,best
}
##
function ygen() {
    y --force-generic-extractor "$@"
    rename .apt .mp4 *.apt
}
noglobfn ygen
##
function yt-from-id {
    #: @idempotent
    ##
    in-or-args "$@" |
        perl -pe 's|^(?!https://)|https://www.youtube.com/watch?v=|g' |
        cat-copy-if-tty
}

function yt-extract-id {
    in-or-args "$@" |
        rget '(?:\[)(.{11})(?:\])' |
        cat-copy-if-tty
}

function yt-from-name {
    cat-fdz-if-tty "$@" |
        yt-extract-id |
        yt-from-id |
        cat-copy-if-tty
}
##
