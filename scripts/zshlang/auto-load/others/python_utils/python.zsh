##
function comment-rm-py {
    cat-paste-if-tty |
        perl -CS -lne 'm/^\s*#/ || print' |
        cat-copy-if-tty
}
##
