function in-or-args() {
    test -t 0 && ec "$@" || ec "$(</dev/stdin)"
}
