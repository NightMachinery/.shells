##
function xray-json-to-link {
    local f="${1}"
    local name="${2:-${f:t:r}}"
    assert-args f @RET

    local link
    link="$(Xray-Link-Json "$f")" @RET

    ec-copy "${link}#${name}" @RET
}
##
