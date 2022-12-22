##
function ocr {
    local lang="$1"

    local opts=()
    if test -n "$lang" ; then
        opts+=(-l "$lang")
    fi

    pngpaste - |
        reval-ec tesseract "${opts[@]}" stdin stdout |
        cat-copy-if-tty
}

function ocr-fa {
    ocr "fas" |
        reval-rtl cat-copy
}
##
