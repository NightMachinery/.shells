##
function source-suitable-p {
    local f="${1:t}"

    [[ "$f" == *.(zsh|bash) \
        || "$f" == '.zshenv' \
        || "$f" == '.zshrc' \
        || "${f}" == '.shared.sh' \
        || "$f" == '.privateShell' ]]
}

function source-cmd {
    local cmd="${commands[$1]}" ; shift
    # assert isNotBinary "$cmd" @RET
    assert source-suitable-p "$cmd" @RET

    reval-ec psource "$cmd" "$@"
}
##
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
aliasfn cin conda-init

function conda-activate() {
    conda-init
    conda activate "$@"

    if proxy-auto-p ; then
        # conda will source its own useless 'conda' function wrapper, which we need to again replace by our own proxifier wrapper
        pxify-command conda
    fi
}
aliasfn cina conda-activate
##
function rust-setup() {
    (( $+commands[rustc] )) && {
        # export RUST_SRC_PATH="$(memoi_skiperr=y memoi-eval rustc --print sysroot)/lib/rustlib/src/rust/src"
        export RUST_SRC_PATH="$(memoi_skiperr=y memoi-eval rustc --print sysroot)/lib/rustlib/src/rust/library"
    }
}
##
function phpbrew-source {
    #: @broken @upstreamBugs
    ##
    source ~/.phpbrew/bashrc @TRET

    if isDarwin ; then
        phpbrew lookup-prefix homebrew
    elif isUbuntu ; then
        phpbrew lookup-prefix ubuntu
    fi
}
##
