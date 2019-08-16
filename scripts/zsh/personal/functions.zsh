cellar() {
    silent pushd "$cellar"
    reval "$@"
    silent popd
}
cellarq() {
    silent pushd "$cellar"
    eval "$@"
    silent popd
}
