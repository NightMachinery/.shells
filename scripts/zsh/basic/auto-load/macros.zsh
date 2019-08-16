# alias -g MAGIC='| { eval "$(read -d "" -r -E)" }'
alias -g MAGIC='| { eval "$(< /dev/stdin)" }'
mdoc_col=(0 35 255)
function m_doc() {
    doc 'Usage: m_doc <original-args> <name-of-script> <text-to-prepend-to-help> <help> ...
Just use the alias `mdoc`.'
    ec "! { [[ ${(q+)1} == '-h' ]] || [[ ${(q+)1} == '--help' ]] } || {
        ec ${(q+)3}${(q+@)@[4,-1]}|color ${mdoc_col[@]:-blue} ; return 0 }"
}
alias mdoc='m_doc "$*" "$0" ""'
alias mdocu='m_doc "$*" "$0" "Usage: $0 "'

mdoc-test() {
    mdoc Usage: Just do not \;\) MAGIC
    ec no
}
