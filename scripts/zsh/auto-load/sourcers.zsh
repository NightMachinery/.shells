sin() {
    FORCE_INTERACTIVE=y
    NIGHT_NO_EXPENSIVE=''
    sb
    source ~/.zshrc
    sin-e
    eval "$(gquote "$@")"
}
sin-e() {
    re source "$NIGHTDIR"/zsh/exorbitant/**/*(.)
}
cin() {
    test -n "$CONDA_IS_LOADED" && return 0
    local __conda_setup="$(conda 'shell.zsh' 'hook' 2> /dev/null)"
    if [ $? -eq 0 ]; then
        eval "$__conda_setup"
    else
        ecerr conda not loaded
    fi
}
cina() {
    cin
    conda activate "$@"
}
