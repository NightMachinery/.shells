cellar() {
    silent pushd "$cellar"
    reval "$@"
    silent popd
}
