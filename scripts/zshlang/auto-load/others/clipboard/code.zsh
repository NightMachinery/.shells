##
function pbcopy-code {
    #: useful for Microsoft Office
    ##
    local lang="${1}"

    local opts=()
    if test -n "$lang" ; then
        opts+=(-l "$lang")
    fi

    cat-paste-if-tty |
        pygmentize -O style=borland -f rtf "${opts[@]}" |
        pbcopy-rtf
}
##
