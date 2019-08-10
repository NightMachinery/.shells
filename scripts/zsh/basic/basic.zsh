function in-or-args() {
    (( $# )) && ec "$@" || ec "$(</dev/stdin)"
}
function pbcopy() {
    (( $+commands[pbcopy] )) && command pbcopy "$@"
}
