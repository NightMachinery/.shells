sin() {
    export FORCE_INTERACTIVE=y
    NIGHT_NO_EXPENSIVE=''
    sb
    source ~/.zshrc
    sin-e
    eval "$(gquote "$@")"
}
sin-e() {
    re source "$NIGHTDIR/zsh/exorbitant/**/*(.)"
}
