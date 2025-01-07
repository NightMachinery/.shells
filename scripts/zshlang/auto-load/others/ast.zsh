##
function comment-rm-json {
    #: * @warn
    #: ** This only removes lines starting with a comment.
    #: *** This will also delete such lines even if they are in a multi-line string. (Can JSON have multi-line strings though?)
    #: ** Basically, we are using a very simple regex and not actually parsing the AST.
    ##
    cat-paste-if-tty |
        perl -CS -lne 'm{^\s*//} || print' |
        cat-copy-if-tty
}
# aliasfn comment-rm-json json5-to-json
##
function comment-rm-py-v2 {
    whitespace-shared-rm |
        comment_rm_py.py |
        trailing-whitespace-rm |
        strip-duplicate-subsequent-blank-whitespace-lines |
        cat-copy-if-tty
}
aliasfn comment-rm-py comment-rm-py-v2
aliasfn comment-rm-py-inplace inplace_io_m='last_stdin_stdout' inplace-io comment-rm-py

function comment-rm-py-v1 {
    #: * @warn
    #: ** This only removes lines starting with a comment.
    #: *** This will also delete such lines even if they are in a multi-line string.
    #: ** Basically, we are using a very simple regex and not actually parsing the AST.
    ##
    cat-paste-if-tty |
        perl -CS -lne 'm/^\s*#/ || print' |
        cat-copy-if-tty
}
##
function comment-rm-latex {
    #: * @warn
    #: ** This only removes lines starting with a comment.
    #: ** Basically, we are using a very simple regex and not actually parsing the AST.
    ##
    cat-paste-if-tty |
        perl -CS -lne 'm/^\s*%/ || print' |
        cat-copy-if-tty
}
##
