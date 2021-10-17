function techmeme-extracturl() {
    # @alt: use https://www.techmeme.com/river
    ##
    local turl="${1:?}"

    if [[ "$turl" =~ '#a([^#]+)$' ]] ; then
        local html link crit
        html="$(full-html2 "$turl")" || {
            ecerr "$0: Failed to download $turl"
            return 1
        }
        crit="$(<<<"$html" pup "#${match[1]}")" || return 3
        link="$(<<<"$crit" pup ".ourh attr{href}")"
        if test -z "$link" ; then
            link="$(<<<"$crit" pup  'div.ii a attr{href}' | ghead -n 1)"
            if test -z "$link" ; then
                tnotif-casual "$0: Could not extract from: $turl"
                return 1
            fi
        fi
        ec $link
    else
        ecerr "$0: Bad URL: $turl"
        return 2
    fi
}
