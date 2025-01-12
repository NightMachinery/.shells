##
function dblp-get {
    local url="${1}"
    assert-args url @RET
    shift

    dblp_titles.py -u "${url}" "$@" |
        cat-copy-if-tty
}

function dblp-titles {
    dblp-get "$@" -o titles-only |
        cat-copy-if-tty
}

function dblp-relevant-2025 {
    dblp_relevance_2025.py "$@" |
        cat-copy-if-tty
}

function dblp-urls-extract {
    pbpaste-urls |
        rg 'https://dblp.org/pid/(\d+/\d+(?:-\d+)?)' |
        duplicates-clean |
        cat-copy-if-tty
}
##
