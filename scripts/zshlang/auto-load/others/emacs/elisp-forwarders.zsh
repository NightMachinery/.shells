##
function alias-elisp {
    local elisp_name
    local alias_name
    alias_name="${1}"
    elisp_name=("${@[2,-1]:-$alias_name}")

    # assert-args elisp_name @RET # disabled for perf

    ##
    #: We don't want to quote these, actually.
    # elisp_name="$(emc-quote-safe "${elisp_name[@]}")"
    # elisp_name="$(gquote-dq "${elisp_name[@]}")"
    ##
    elisp_name="${elisp_name[*]}"
    ##

    eval "function $alias_name { revaldbg emc-eval \"(${elisp_name} \$(emc-quote-safe \"\$@\"))\" }"
}

alias-elisp night/z z # good for testing
##
alias-elisp night/marker-audio-kill
##
