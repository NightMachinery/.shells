typeset -g NIGHT_NO_EXPENSIVE
isNotExpensive || {
    re source "$NIGHTDIR"/zshlang/expensive/**/*
}
# personal initializes a lot of vars, so it should be sourced first
test -z "$NIGHT_PERSONAL" || re source "$NIGHTDIR"/zshlang/personal/**/*(.)
run-on-each source "$NIGHTDIR"/zshlang/auto-load/**/*(.) #I have disabled dotfiles because vim and others create them for swap.

psource "$NIGHTDIR/zshlang/last.zsh"
