##
function xray-json-to-link {
    local f="${1}"
    local name="${2:-${f:t:r}}"
    assert-args f @RET

    local links
    links=(${(@f)"$(Xray-Link-Json "$f")"}) @RET

    ec-sep-h
    local link
    for link in ${links[@]} ; do
        ec "${link}#${name}" @RET
        ec
    done | cat-copy-if-tty
}
##
