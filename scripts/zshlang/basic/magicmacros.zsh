## Global Aliases
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'

alias -g @f=' | inargsf '
alias -g @ff=' | fz | inargsf '
alias -g @w=' | fzinw '
##
# alias -g MAGIC='| { eval "$(read -d "" -r -E)" }'
alias -g MAGIC='| { eval "$(< /dev/stdin)" }'
alias -g '@RET'=' || return $?'
alias -g '@MRET'='"$0" || return $?'
##
function magic_h() {
    : 'Usage: magic ... ; mret'
    : 'Does not access stdin, which makes it less buggy. E.g., "arger 1 2 3|fz |stdinmagic" hangs. https://unix.stackexchange.com/questions/585941/zsh-weird-behavior-bug-in-reading-stdin '

    eval "$(reval "$@")"
}
alias magic='local magic_ret="" ; magic_h '
alias mret0='magic_ret=0 ; return 0'
alias mret='test -z "$magic_ret" || return "$magic_ret"'
###
mdoc_col=(0 35 255)
if ! (( ${+functions[color]} )) ; then
    function color() {
        : 'This is a placeholder for the real color function.'
        ec "COLOR_REP (YOU SHOULDN'T SEE THIS): $@"
    }
fi
function m_doc() {
    : 'Usage: m_doc <original-args> <name-of-script> <text-to-prepend-to-help> <help> ...
Just use the alias `mdoc`.'

    print -r -- "! { [[ ${(q+)1} == '-h' ]] || [[ ${(q+)1} == '--help' ]] } || {
        color ${mdoc_col[@]:-blue} ${(q+)3}${(q+@)@[4,-1]} ; mret0 }"
}
alias mdoc='m_doc "$*" "$0" ""'
alias mdocu='m_doc "$*" "$0" "Usage: $0 "'

##
mdoc-test() {
    # uses the global alias MAGIC
    mdoc Usage: sth MAGIC
    echo no
}
mdoc-test2() {
    # without global alias
    magic mdoc Usage: sth ; mret
    echo no
}
##
function fn-isTop() {
    local caller=( "${@:-${funcstack[2]}}" ) prefixes=( ${fn_isTop_p[@]} )
    test -z "$caller" && {
        ectrace "$0: Have you called me at the top level by myself? :D"
        return 1
    }
    local caller_glob="(${(@j.|.)caller})"
    if (( ${#prefixes} > 0 )) ; then
        caller_glob="(${caller_glob}|(${(@j.|.)prefixes}*))"
    fi
    # dvar caller_glob

    local i
    for i in {${#funcstack}..1} ; do
        # ecdbg "fn ${i}: ${funcstack[$i]}"

        [[ "${funcstack[$i]}" == ${~caller_glob} ]] && return 0
        if ! funcstack-isExcluded "${funcstack[$i]}" ; then
            [[ "${funcstack[$i]}" == ${~caller_glob} ]]
            return $?
        fi
    done
    ectrace "$0: @impossible funcstack exhausted"
    return 1
}
function r-e-val() {
    : "A function used to test fn-isTop"

    reval "$@"
}
##
