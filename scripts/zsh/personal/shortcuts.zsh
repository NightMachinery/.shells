inqcell() incell "$(gquote "$@")"
incell() {
    pushf "$cellar"
    eval "$@"
    popf
}
alias cellp="incell 'gcam . ; gl --no-edit ; gp'"
indl() {
    pushf ~/Downloads/
    eval "$@"
    popf
}
