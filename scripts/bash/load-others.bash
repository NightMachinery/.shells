source <(antibody init)
run-on-each source "$NIGHTDIR"/bash/auto-load/* #I have disabled dotfiles because vim and others create them for swap.
test -z "$NIGHT_PERSONAL" || re source "$NIGHTDIR"/bash/personal/**/*
test -n "$ZSH_VERSION" && source "$NIGHTDIR"/zsh/load-others.zsh
