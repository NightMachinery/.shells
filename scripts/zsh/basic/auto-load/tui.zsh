function printz() {
    test -n "$*" && {
     isI && print -rz -- "$@" || ec "$@"
    }
}
