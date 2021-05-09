#!/usr/bin/env zshplain.dash

export FZF_DEFAULT_OPTS='--bind '\''shift-up:toggle+up,shift-down:toggle+down,alt-up:preview-up,alt-down:preview-down,tab:toggle,shift-tab:toggle+beginning-of-line+kill-line,alt-/:toggle-preview,ctrl-j:toggle+beginning-of-line+kill-line,ctrl-t:top,ctrl-s:select-all,alt-enter:print-query'\'' --color=light --multi --hscroll-off 99999'

alias ec='print -r --'
alias ecn='print -rn --'
function gquote() {
    ec "${(q+@)@}"
}
###

local lines query d

d=( "${(@f)$(fzf --print-query --no-sort -i --exact)}" ) || return $?
query="${d[1]}"
lines="${(@F)d[2,-1]}"

# pbcopy is @darwinOnly
if d=( "${(@f)$(ecn "$lines" | gtr -s '[:blank:]' '\n' | fzf -i --exact --query "$query")}" ) ; then
    ##
    if (( ${#d} >= 2 )) ; then
        gquote "${(@)d}" | pbcopy
    else
        ecn "${(@F)d}" | pbcopy
    fi
else
    ecn "$lines" | pbcopy
fi
