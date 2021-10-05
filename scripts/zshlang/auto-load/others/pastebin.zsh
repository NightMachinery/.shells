##
function pastebin-sprunge {
    cat-paste-if-tty | curl --progress-bar --form 'sprunge=<-' http://sprunge.us | cat-copy-if-tty
    # @alt 'f:1=<-' ix.io
}

function pastebin-gnupaste {
    local mime="${pastebin_mime:-text/plain}" expire="${pastebin_mime:-99m}"

    cat-paste-if-tty | curl --progress-bar --form 'file=@-' --form "type=${mime}" --form "expire=${expire}" https://paste.gnugen.ch | cat-copy-if-tty

    # expire: The desired expiration date. This field defaults to one year. Valid units are s(econds), h(ours), d(ays), w(eeks) and m(onths).
    # This service is provided with no guarantees of any kind. Please note that your IP address will be stored for security purposes.
}
@opts-setprefix pastebin-gnupaste pastebin

aliasfn pastebin pastebin-gnupaste

function pastebin-png-jdl {
    local ext=png

    local tmp
    tmp="$(gmktemp --suffix ".${ext}")" @TRET

    pngpaste "$tmp" @TRET
    icat "$tmp"
    reval-ec jdl "$tmp" @TRET

    bell-image-uploaded
}
alias ppp='pastebin-png-jdl'

function pastebin-png-gnupaste {
    : "usage: cat file.png | pastebin-png"

    @opts mime image/png @ pastebin "$@"
}

function pastebin-jpg-gnupaste {
    : "usage: cat file.jpg | pastebin-jpg"

    @opts mime image/jpeg @ pastebin "$@"
}
##
