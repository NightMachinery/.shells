##
function url-sha256() {
    brishzr l_url-sha256 "$@"
}
noglobfn url-sha256

function l_url-sha256() {
    local url="$1"
    assert-args url @RET

    gurl "$url" | sha256sum | awkn 1
}
renog l_url-sha256
##
