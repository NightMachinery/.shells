alias dlg="\noglob deluge-console"
alias dlgi="\noglob deluge-console info"
function dlga() {
    local torrent="${1:?}"
    local dest="${2}"
    if ! [[ "$dest" =~ '^/' ]] ; then
        dest="$HOME/Downloads/$dest"
    fi

    deluge-console add --move-path "$dest" "$torrent"
}
noglobfn dlga
