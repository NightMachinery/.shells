function yf() {
    # GISTME
    local ffull="$(youtube-dl -F "$@" | fz)"
    local f="$(<<<$ffull gawk '{print $1}')"
    test -z "$f" && return 1
    [[ "$ffull" =~ 'video only' ]] && f+="+bestaudio"
    rgeval y -f "$f" "$@"
}
function youtube-dl() {
    local opts=()
    isI || opts+=( --quiet --no-progress )
    transformer urlfinalg "command youtube-dl $opts[@]" "$@"
}
function ylist() {
    youtube-dl -j --flat-playlist "$@" | jq -r '"https://youtu.be/\(.id)"'
}
noglobfn ylist
function ytitle() {
    youtube-dl --get-filename -o "%(title)s" "$@"
}
