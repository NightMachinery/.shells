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
##
conda-init() {
    test -n "$CONDA_IS_LOADED" && return 0
    local __conda_setup="$(conda 'shell.zsh' 'hook' 2> /dev/null)"
    if [ $? -eq 0 ]; then
        eval "$__conda_setup"
        CONDA_IS_LOADED=y
    else
        ecerr conda not loaded
    fi
}
aliasfn conda-init cin
conda-activate() {
    cin
    conda activate "$@"
}
aliasfn cina conda-activate
##
function rust-setup() {
    (( $+commands[rustc] )) && {
        # export RUST_SRC_PATH="$(memoi_skiperr=y memoi-eval rustc --print sysroot)/lib/rustlib/src/rust/src"
        export RUST_SRC_PATH="$(memoi_skiperr=y memoi-eval rustc --print sysroot)/lib/rustlib/src/rust/library"
    }
}
