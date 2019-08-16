typeset -g NIGHT_NO_EXPENSIVE
test -n "$NIGHT_NO_EXPENSIVE" || { source <(antibody init)
                                   ANTIBODY_HOME="$(antibody home)"
                                   re source "$NIGHTDIR"/zsh/expensive/**/*
                                   NIGHT_NO_EXPENSIVE=y }
run-on-each source "$NIGHTDIR"/zsh/auto-load/**/*(.) #I have disabled dotfiles because vim and others create them for swap.
test -z "$NIGHT_PERSONAL" || re source "$NIGHTDIR"/zsh/personal/**/*
