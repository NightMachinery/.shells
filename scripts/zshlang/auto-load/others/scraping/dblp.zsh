function dblp-get {
    local url="${1}"
    assert-args url @RET
    shift

    dblp_titles.py -u "${url}" "$@"
}

function dblp-titles {
    dblp-get "$@" -o titles-only
}

function dblp-relevant-2025 {
    dblp_relevance_2025.py "$@"
}

function dblp-urls-extract {
    pbpaste-urls |
        rg 'https://dblp.org/pid/(\d+/\d+)' |
        duplicates-clean
}
