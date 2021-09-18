##
function qrcode-decode {
    qrcode-decode1 "$@"
}

function qrcode-decode0 {
    : "usage: <png file>"

    zbarimg --raw "$@" @RET
}

function qrcode-decode1 {
    local i="$1"
    assert test -e "$i" @RET

    qr_decode.js "$i" | jqm .data
}
##
function qrcode() {
    # [[id:8ed49663-a35c-44c8-a398-39cc77a13064][CLI/QRCode.org:QRCode]]
    ##
    qrcode0 "$@"
}

function qrcode-ansi {
    qrcode0 '-' "$@"
}
##
function qrcode1() {
    # doesn't support newlines
    ##
    local i="$*"
    assert-args i @RET
    i="$(url-encode "$i")" @TRET

    ecgray "$0: @warn uses a web API to create the QR code, so it's insecure!"

    gurl qrenco.de/"$i"
}
##
function qrcode0 {
    local out="$1" opts=("${@[2,-1]}") error_correction="${qrcode_ec:-H}"

    local tmp=''
    if test -z "$out" ; then
        tmp="$(gmktemp --suffix='.png')" @TRET
        out="$tmp"
    elif [[ "$out" == '-' ]] ; then
        out=''
        opts+=(--type ansi256utf8)
    fi
    if test -n "$out" ; then
        opts+=( --output "$out" )
    fi

    qrencode --level="$error_correction" "$opts[@]" @RET
    # --level={LMQH} specify  error  correction  level  from  L (lowest) to H (highest). (default=L)
    # H: This provides a lost data restoration rate of about 30%.
    #
    # -k, --kanji
    #        assume that the input text contains kanji (shift-jis).
    # -c, --casesensitive
    #        encode lower-case alphabet characters in 8-bit mode. (default)
    # -i, --ignorecase
    #        ignore case distinctions and use only upper-case characters.
    # -8, --8bit
    #        encode entire data in 8-bit mode. -k, -c and -i will be ignored.

    if test -n "$tmp" ; then
        ecgray "$0: output set to $(gquote-sq "$tmp")"

        pbadd "$out" || true
    fi
}
@opts-setprefix qrcode0 qrcode

function qrcode-error-correction-max {
    @opts ec H @ qrcode0 "$@"
}
@opts-setprefix qrcode-error-correction-max qrcode
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
