##
function h-pastebin-sprunge {
    curl --progress-bar --form 'sprunge=<-' http://sprunge.us
    # @alt 'f:1=<-' ix.io
}
aliasfn pastebin-sprunge pastebin_e='h-pastebin-sprunge' pastebin

function h-pastebin-gnupaste {
    local mime="${pastebin_mime:-text/plain}" expire="${pastebin_mime:-99m}"

    curl --progress-bar --form 'file=@-' --form "type=${mime}" --form "expire=${expire}" https://paste.gnugen.ch

    # expire: The desired expiration date. This field defaults to one year. Valid units are s(econds), h(ours), d(ays), w(eeks) and m(onths).
    # This service is provided with no guarantees of any kind. Please note that your IP address will be stored for security purposes.
}
aliasfn pastebin-gnupaste pastebin_e='h-pastebin-gnupaste' pastebin
@opts-setprefix pastebin-gnupaste pastebin

function pastebin {
    local engine=("${pastebin_e[@]:-h-pastebin-gnupaste}")

    cat-paste-if-tty | { tee /dev/tty ; ec-sep-h > /dev/tty } | "$engine[@]" "$@" | cat-copy-if-tty
}
alias pb='pastebin'
##
function pastebin-png-jdl {
    local ext=png

    local tmp
    tmp="$(gmktemp --suffix ".${ext}")" @TRET

    pngpaste "$tmp" @TRET
    icat "$tmp"
    reval-ec jdl "$tmp" @TRET

    bell-image-uploaded
}
alias pbi='pastebin-png-jdl'

function pastebin-png-gnupaste {
    : "usage: cat file.png | pastebin-png"

    @opts mime image/png @ pastebin "$@"
}

function pastebin-jpg-gnupaste {
    : "usage: cat file.jpg | pastebin-jpg"

    @opts mime image/jpeg @ pastebin "$@"
}
##
