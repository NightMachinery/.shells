typeset -g NIGHT_NO_EXPENSIVE
isNotExpensive || {
    source <(antibody init)
    ANTIBODY_HOME="$(antibody home)"
    re source "$NIGHTDIR"/zsh/expensive/**/*
}
run-on-each source "$NIGHTDIR"/zsh/auto-load/**/*(.) #I have disabled dotfiles because vim and others create them for swap.
test -z "$NIGHT_PERSONAL" || re source "$NIGHTDIR"/zsh/personal/**/*
source-interactive-all() {
    re source "$NIGHTDIR"/zsh/interactive/**/*(.)
    NIGHT_NO_EXPENSIVE=y
}
