alias dlg="\noglob deluge-console"
alias deluge-info="\noglob deluge-console info"
alias dlgi="deluge-info"
##
function deluge-add {
    local torrent="${1:?}"
    local dest="${@[2,-1]}"
    if ! [[ "$dest" =~ '^/' ]] ; then
        dest="$HOME/Downloads/$dest"
    fi

    deluge-console add --move-path "$dest" "$torrent"
}
noglobfn deluge-add
alias dlga=deluge-add

function deluge-add-audiobook {
    local trr="$1" ; shift
    local dest="$*"
    if ! [[ "$dest" =~ '^/' ]] ; then
        dest="audiobooks/$dest"
    fi

    dlga "$trr" "$dest"

    : "myan checks the IP for auth, so you need to use the downloading server's VPN"
}
noglobfn deluge-add-audiobook
alias dlg-ab='deluge-add-audiobook'
alias dlgab='deluge-add-audiobook'
##
