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
function sin {
    FORCE_INTERACTIVE=y
    NIGHT_NO_EXPENSIVE=''
    sb
    source ~/.zshrc
    sin-e
    eval "$(gquote "$@")"
}

function sin-e {
    re source "$NIGHTDIR"/zsh/exorbitant/**/*(.)
}
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
