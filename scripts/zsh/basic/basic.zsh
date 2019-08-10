function in-or-args() {
    (( $# )) && ec "$@" || ec "$(</dev/stdin)"
}
