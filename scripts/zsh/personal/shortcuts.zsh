vcnpp() {
    vcsh night.sh commit -uno -am "." ; vcsh night.sh pull --no-edit ; vcsh night.sh push
}
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
