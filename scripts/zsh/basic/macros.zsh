mdoc_col=(0 35 255)
function m_doc() {
    ec "! { [[ ${(q+)1} == '-h' ]] || [[ ${(q+)1} == '--help' ]] } || {
        ec \"Help for $2:
$@[3,-1]\"|color ${mdoc_col[@]:-blue} ; return 0 }"
}
alias mdoc='eval "$(m_doc "$*" "$0" '

mdoc_test() {
    mdoc hi no MAGIC
    ec "$*"
}
mdoc_test2() {
    eval "$(m_doc "$*" "$0" hi no)"
    ec "$*"
}
mdoc_test3() {
    eval "$(m_doc "$*" "$0" hi no MAGIC
    ec "$*"
}
mdoc_test4() {
    mdoc hi no)"
    ec "$*"
}
