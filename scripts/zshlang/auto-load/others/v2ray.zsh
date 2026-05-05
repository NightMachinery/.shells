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
function xray-sub-get {
    local url="${1}"
    assert-args url @RET

    curl --fail --location  --suppress-connect-headers --styled-output --cookie-jar /dev/null "${url}" |
        gbase64 --decode |
        cat-copy-if-tty
}

function xray-sub2json {
    local links
    links="$(xray-sub-get "$@")" @RET

    ec "${links}" |
        Xray-Link-Json - |
        command jq . |
        cat-copy-if-tty

    # jqm '.outbounds' |
    #     perl -ne 'print $p if $. > 2; $p = $_' | head -n -1 |
}
##
