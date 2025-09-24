##
function codex-install {
    reval-ecgray npm-install '@openai/codex'
}
##
function codex-clean-text {
    in-or-args "$@" |
        perl -CSD -pe 's/\x{258C}//g' |
        cat-copy-if-tty
}
##
