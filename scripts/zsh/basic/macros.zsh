# alias -g MAGIC='| { eval "$(read -d "" -r -E)" }'
alias -g MAGIC='| { eval "$(< /dev/stdin)" }'
mdoc_col=(0 35 255)
function m_doc() {
    ec "! { [[ ${(q+)1} == '-h' ]] || [[ ${(q+)1} == '--help' ]] } || {
        ec \"$2:
$@[3,-1]\"|color ${mdoc_col[@]:-blue} ; return 0 }"
}
alias mdoc='m_doc "$*" "$0" '

mdoc-test() {
    mdoc Usage: Just do not \;\) MAGIC
    ec no
}
