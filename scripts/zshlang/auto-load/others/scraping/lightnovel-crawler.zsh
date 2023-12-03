##
function lnc-mobi {
    local url="$1"
    assert-args url @RET

    $proxyenv reval-ec lightnovel-crawler --all --single --format mobi --suppress --source "$url"
}

function lnc-epub {
    local url="$1"
    assert-args url @RET

    if isJulia ; then
        jee
    fi

    $proxyenv reval-ec lightnovel-crawler --all --single --format epub --suppress --source "$url"
    # --ignore -o .

    if isJulia ; then
        jup
        dir2k .
    fi
}
alias lnc='lnc-epub'
##
