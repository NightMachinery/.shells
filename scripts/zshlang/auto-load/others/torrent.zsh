function trr() (
    mkdir -p "${PEERFLIX_DIR:=$HOME/Downloads/video}"
    local opts=()
    isSSH || opts=( --mpv -- --fullscreen )
    peerflix "$@" --path "${PEERFLIX_DIR}" --no-quit "$opts[@]"
)
function trrz() {
    # TODO config stored at ~/Library/Preferences/teevee-nodejs/
    trr "$(teevee fetch)"
}
function torrent2magnet() {
    jglob
    re 'transmission-show -m' "$@"
}
alifn t2m=torrent2magnet
function magnet2torrent() {
    reval-ec aria2c -d "${2:-.}" --bt-metadata-only=true --bt-save-metadata=true "$(hash2magnet "$1")"
}
alifn m2t=magnet2torrent
hash2magnet() {
    [[ "$1" =~ '^magnet:' ]] && ec "$1" || ec "magnet:?xt=urn:btih:$1"
}
reify hash2magnet
alifn h2m=hash2magnet
