##
function py-tuple-to-dict {
    cat-paste-if-tty |
        py_tuple_to_dict.pl "$@" |
        cat-copy-if-tty
}
##
