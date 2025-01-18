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
##
function h-dblp-urls-extract {
    in-or-args "$@" |
        rg 'https://dblp.org/pid/' |
        duplicates-clean |
        cat-copy-if-tty
}

function dblp-urls-extract-and-save {
    pbpaste-urls |
        rg 'https://dblp.org/' |
        parallel_jobs=128 parallelm --bar --keep-order url-final2 |
        h-dblp-urls-extract > dblp_all.txt @RET
}
##
