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
function fn-name() {
    local start="${1:-2}"

    local indices=( {${start}..${#funcstack}} )
    local i
    for i in $indices[@] ; do
        # ecdbg "fn ${i}: ${funcstack[$i]}"

        if ! funcstack-isExcluded "${funcstack[$i]}" ; then
            ec "${funcstack[$i]}"
            return 0
        fi
    done
    return 1
}
function fn-isTop() {
    local caller=( "${@}" ) prefixes=( ${fn_isTop_p[@]} ) reverse="${fn_isTop_r}"
    local caller_name="$(fn-name 3)" # funcstack_excluded_names is itself used in fn-name, so we have to get this before polluting it
    local funcstack_excluded_names=( ${funcstack_excluded_names[@]} ${fn_isTop_x[@]} )
    local funcstack_excluded_prefixes=( ${funcstack_excluded_prefixes[@]} ${fn_isTop_xp[@]} )

    local indices=( {${#funcstack}..1} )
    if bool "$reverse" ; then
        if (( ${#funcstack} < 3 )) ; then
            # The function has no parents
            return 1
        fi
        indices=( {3..${#funcstack}} )

        funcstack_excluded_names+="$caller_name"
    else
        if test -z "$caller" ; then
            caller+="$caller_name"
        fi
    fi
    assert-args caller @RET

    local caller_glob="(${(@j.|.)caller})"
    if (( ${#prefixes} > 0 )) ; then
        caller_glob="(${caller_glob}|(${(@j.|.)prefixes}*))"
    fi
    re dvar caller_name caller_glob

    local i
    for i in $indices[@] ; do
        ecdbg "fn ${i}: ${funcstack[$i]}"

        [[ "${funcstack[$i]}" == ${~caller_glob} ]] && return 0
        if ! funcstack-isExcluded "${funcstack[$i]}" ; then
            [[ "${funcstack[$i]}" == ${~caller_glob} ]]
            return $?
        fi
    done
    if bool "$reverse" ; then
        # requested parent not found
        return 1
    else
        ectrace "$0: @impossible funcstack exhausted"
        return 1
    fi
}
function re-val() {
    : "A function used to test fn-isTop"

    reval "$@"
}
##
