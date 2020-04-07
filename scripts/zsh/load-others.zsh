typeset -g NIGHT_NO_EXPENSIVE
isNotExpensive || {
    re source "$NIGHTDIR"/zsh/expensive/**/*
}
run-on-each source "$NIGHTDIR"/zsh/auto-load/**/*(.) #I have disabled dotfiles because vim and others create them for swap.
test -z "$NIGHT_PERSONAL" || re source "$NIGHTDIR"/zsh/personal/**/*
psource "$NIGHTDIR/zsh/last.zsh"
