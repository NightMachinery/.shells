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
function comment-rm-py {
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
