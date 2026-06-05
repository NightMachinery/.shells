## Return/error global aliases shared by public plugins and the full local stack.
alias -g '@RET'=' || return $?'

alias -g '@STRUE'=' || { if (( ${+functions[ectrace]} )); then ectrace_single_trace=y ectrace_ret=$? ectrace ; fi ; true }'

alias -g '@TRET'=' || { local retcode=$? ; if (( ${+functions[ectrace]} )); then ectrace_single_trace=y ectrace_ret=$retcode ectrace ; else ecerr "$0: exited ${retcode}" ; fi ; return $retcode }'

alias -g '@MRET'='"$0" || return $?'
