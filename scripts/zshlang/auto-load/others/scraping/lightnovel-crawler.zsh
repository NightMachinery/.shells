##
function lnc-epub {
    local url="$1"
    assert-args url @RET

    lightnovel-crawler --all --single --format epub --suppress --source "$url"
    # --ignore -o .

    if isJulia ; then
        jup
        p2k *.epub
    fi
}
alias lnc='lnc-epub'
##
