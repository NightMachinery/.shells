alias dlg="\noglob deluge-console"
alias dlgi="\noglob deluge-console info"
##
function dlga() {
    local torrent="${1:?}"
    local dest="${@[2,-1]}"
    if ! [[ "$dest" =~ '^/' ]] ; then
        dest="$HOME/Downloads/$dest"
    fi

    deluge-console add --move-path "$dest" "$torrent"
}
noglobfn dlga

function dlg-ab() {
    local trr="$1" ; shift
    local dest="$*"
    if ! [[ "$dest" =~ '^/' ]] ; then
        dest="audiobooks/$dest"
    fi

    dlga "$trr" "$dest"

    : "myan checks the IP for auth, so you need to use the downloading server's VPN"
}
##
