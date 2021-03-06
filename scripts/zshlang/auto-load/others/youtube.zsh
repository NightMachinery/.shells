function youtube-dl() {
    typeset -ga ytdl_opts # has priority over args
    local opts=()
    isI || opts+=( --quiet --no-progress )
    if isSSH ; then
        transformer urlfinalg "revaldbg $proxycmd youtube-dl $opts[@]" "$@" "$ytdl_opts[@]"
    else
        # urlfinalg takes too much time on Iran's net.
        revaldbg $proxycmd youtube-dl "$opts[@]" "$@" "$ytdl_opts[@]"
    fi
}
##
function yf() {
    # GISTME
    local ffull="$(youtube-dl -F "$@" | fz)"
    local f="$(<<<$ffull gawk '{print $1}')"
    test -z "$f" && return 1
    [[ "$ffull" =~ 'video only' ]] && f+="+bestaudio"
    rgeval y -f "$f" "$@"
}
function ylist() {
    youtube-dl -j --flat-playlist "$@" | jq -r '"https://youtu.be/\(.id)"'
}
noglobfn ylist
function ytitle() {
    youtube-dl --get-filename -o "%(title)s" "$@"
}
function ytrans() {
    doc "youtube transcribe"
    local url="$1"
    local title=("${(@f)$(youtube-dl --get-filename -o "%(title)s" "$url")}")
    # local u="$(md5m "$title")"
    pushf "$title"
    youtube-dl --convert-subs vtt --write-auto-sub --skip-download --sub-lang en "$1"
    vtt2txt2.py *.vtt | gtr $'\n' ' ' > ../"$title.txt"
    popf
    command rm -r "$title"
    t2e "$title" "$title.txt"
}
renog ytrans
##
function streamlink-m() {
    streamlink --player='mpv' "$@" 720p,best
}
##