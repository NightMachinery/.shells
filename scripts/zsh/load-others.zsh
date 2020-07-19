typeset -g NIGHT_NO_EXPENSIVE
isNotExpensive || {
    re source "$NIGHTDIR"/zsh/expensive/**/*
}
# personal initializes a lot of vars, so it should be sourced first
test -z "$NIGHT_PERSONAL" || re source "$NIGHTDIR"/zsh/personal/**/*(.)
run-on-each source "$NIGHTDIR"/zsh/auto-load/**/*(.) #I have disabled dotfiles because vim and others create them for swap.
psource "$NIGHTDIR/zsh/last.zsh"
