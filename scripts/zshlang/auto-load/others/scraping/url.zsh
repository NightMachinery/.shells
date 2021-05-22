##
aliasfn url-match match-url
##
function url-sha256() {
    brishzr l_url-sha256 "$@"
}
noglobfn url-sha256

function l_url-sha256() {
    local url="$1"
    assert-args url @RET

    gurl "$url" | sha256sum | awkn 1 @TRET
}
renog l_url-sha256
##
function url-encode() {
    local inargs
    in-or-args2 "$@"

    ecn "$inargs" | url-encode.py
}
function url-decode() {
    local inargs
    in-or-args2 "$@"

    ecn "$inargs" | url-decode.py
}
##
