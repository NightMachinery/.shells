##
function pyblack {
    command black "$@"
}
##
function pyblack-cat {
    local tmp
    tmp="$(gmktemp --suffix=.py)" @TRET

    cat-paste-if-tty |
        whitespace-shared-rm > "$tmp" @RET

    reval-ec pyblack "$@" "$tmp" @RET

    cat "$tmp" |
        cat-copy-if-tty
}
##
