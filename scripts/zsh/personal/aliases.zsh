isNotExpensive || {
    export _ZL_ADD_ONCE=1
    export _ZL_MATCH_MODE=1
    antibody bundle skywind3000/z.lua
    ialiases[z]=y
    ialias zz='z -c'      # restrict matches to subdirs of $PWD
    # alias zi='z -i'      # cd with interactive selection
    ialias zf='z -I'      # use fzf to select in multiple matches
    ialias zb='z -b'      # quickly cd to the parent directory
}
ialias zshrc='$=EDITOR ~/.zshrc'
ialias zshenv='$=EDITOR ~/.zshenv'
alias hrep="fc -El 0 | grep"
alias grep='grep --color=auto'

ialias plc=playlistc
ialias emc="emacsclient -t"
ialias emcg="emacsclient -c"
alias b='builtin'
alias typ='typeset -p'
