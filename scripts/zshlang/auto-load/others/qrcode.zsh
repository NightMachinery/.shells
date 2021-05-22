function qrcode1() {
    # doesn't support newlines
    ##
    local i="$*"
    assert-args i @RET
    i="$(url-encode "$i")" @TRET

    gurl qrenco.de/"$i"
}
function qrcode() {
    # [[id:8ed49663-a35c-44c8-a398-39cc77a13064][CLI/QRCode.org:QRCode]]
    ##
    qrcode1 "$@"
}
##
function jqr-gif() {
    # doesn't support newlines https://github.com/x-hw/amazing-qr/issues/30
    : " -v 20 (1-40): increases the resolution"
    ##
    jej
    local i="$1" ; shift
    assert-args i @RET

    vid2gif "$jufile" a.gif
    amzqr "$i" -c -p a.gif "$@"
    trs a.gif
}
alias jqrg='jqr-gif'
##
