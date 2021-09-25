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

function pastebin-png {
    : "usage: cat file.png | pastebin-png"

    @opts mime image/png @ pastebin "$@"
}

function pastebin-jpg {
    : "usage: cat file.jpg | pastebin-jpg"

    @opts mime image/jpeg @ pastebin "$@"
}
##
