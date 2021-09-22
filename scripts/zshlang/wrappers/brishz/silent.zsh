#!/usr/bin/env -S zsh -f

alias gq=gquote
gquote () {
    print -r -- "${(q+@)@[1]}" "${(qq@)@[2,-1]}"
}

silence () {
    {
        eval "$(gquote "$@")"
    } &> /dev/null
}
##
silence "$@"
