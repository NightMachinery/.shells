function techmeme-extracturl() {
    local turl="${1:?}"

    if [[ "$turl" =~ '#a([^#]+)$' ]] ; then
        full-html2 "$turl" | pup "#${match[1]} .ourh attr{href}" || {
            ecerr "$0: Failed for $turl"
            return 1
        }
    else
        ecerr "$0: Bad URL: $turl"
        return 2
    fi
}
