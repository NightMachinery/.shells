function printz() {
    test -n "$*" && print -rz -- "$@"
}
