function mal-rank-get() {
    local url="$1"
    assert-args url @RET

    full-html2 "$url" | serr hxnormalize -l 300 -x | rget 'Ranked:</span> #(\d+)<sup>'
}
