##
function alias-elisp {
    local elisp_name="$1"
    # assert-args elisp_name @RET # disabled for perf
    local alias_name="${2:-$elisp_name}"

    eval "function $alias_name { revaldbg emc-eval \"(${elisp_name} \$(emc-quote-safe \"\$@\"))\" }"
}

alias-elisp z night/z # good for testing
##
alias-elisp night/marker-audio-kill
##
