function magnet2torrent() {
    aria2c -d "${2:-.}" --bt-metadata-only=true --bt-save-metadata=true "$(hash2magnet "$1")"
}
hash2magnet() {
    [[ "$1" =~ '^magnet:' ]] && ec "$1" || ec "magnet:?xt=urn:btih:$1"
}
reify hash2magnet 
