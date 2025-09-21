##
function emacswiki-install {
    @deprecated
    # @alt `git submodule add git://github.com/emacsmirror/emacswiki.org.git emacswiki`
    ##
    local url="$1"
    local name="${url:t}"
    assert-args url name @RET

    local tmp
    tmp="$(gmktemp)" @TRET

    assert gurl "$url" > $tmp @RET
    assert reval-ec mv "$tmp" "$DOOMDIR/emacswiki/${name}" @RET
}
##
