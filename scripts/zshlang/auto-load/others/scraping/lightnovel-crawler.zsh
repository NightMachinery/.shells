##
function lnc-epub {
    local url="$1"
    assert-args url @RET

    jee

    lightnovel-crawler --all --single --format epub --suppress --source "$url"
    # --ignore -o .

    if isJulia ; then
        jup
        dir2k .
    fi
}
alias lnc='lnc-epub'
##
