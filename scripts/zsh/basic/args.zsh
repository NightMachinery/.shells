function in-or-args() {
    (( $# )) && ec "$@" || ec "$(</dev/stdin)"
}
function arr0() { ec "${(pj.\0.)@}" }
function arrN() { ec "${(pj.\n.)@}" }
function in-or-args-arr0() {
    (( $# )) && arr0 "$@" || ec "$(</dev/stdin)"
}
function in-or-args-arrN() {
    (( $# )) && arrN "$@" || ec "$(</dev/stdin)"
}
