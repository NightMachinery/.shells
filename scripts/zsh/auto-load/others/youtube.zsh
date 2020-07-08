function ylist() {
    youtube-dl -j --flat-playlist "$@" | jq -r '"https://youtu.be/\(.id)"'
}
noglobfn ylist
function ytitle() {
    youtube-dl --get-filename -o "%(title)s" "$@"
}
