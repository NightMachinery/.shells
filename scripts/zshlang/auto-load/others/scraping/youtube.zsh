##
function youtube-dl() {
    local cookie_mode="${youtube_dl_c}"
    typeset -ga ytdl_opts # has priority over args

    local head
    # head='youtube-dl'
    head='yt-dlp'

    local opts=()
    if [[ "$head" == 'yt-dlp' ]] ; then
        opts+=(--compat-options no-live-chat,no-keep-subs --abort-on-error)
    fi

    isI || opts+=( --quiet --no-progress )

    if bool $cookie_mode ; then
        opts+=(--add-header "$(cookies-auto "$@")")
        # opts+=(--add-header "$(referer-auto "$@")")
        opts+=(--add-header "Referer: $(browser-current-url)")
    fi

    ##
    # if proxy-p ; then
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
