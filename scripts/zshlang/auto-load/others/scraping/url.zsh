##
function match-url-rg() {
    # FNSWAP: rg
    rg --engine pcre2 -e "$nightUrlRegex" "$@"
}
function match-url2() {
    ec "$*" | ghead -n 1 | silent match-url-rg
}
function match-url() {
    # sometimes errs, presumably because it runs out of memory
    [[ "$*" =~ "^$nightUrlRegex\$" ]]
}
function match-url-liberal() {
    # https://gist.github.com/gruber/249502
    doc "Doesn't require http"

    [[ "$*" =~ '(?i)\b((?:[a-z][\w-]+:(?:/{1,3}|[a-z0-9%])|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'\''".,<>?«»“”‘’]))' ]]
}

aliasfn url-match match-url2
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
