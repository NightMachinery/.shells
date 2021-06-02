##
function lnc-epub {
    local url="$1"
    assert-args url @RET

    lightnovel-crawler --all --single --format epub --suppress --ignore -o . --source "$url"
}
