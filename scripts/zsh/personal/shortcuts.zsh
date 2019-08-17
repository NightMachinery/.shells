cellar() cellarq "$(gquote "$@")"
cellarq() {
    pushf "$cellar"
    eval "$@"
    popf
}
alias cellp="cellarq 'gcam . ; gl --no-edit ; gp'"
