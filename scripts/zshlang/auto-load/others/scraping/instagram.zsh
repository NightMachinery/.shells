##
function instaloader-post {
    pxa-maybe

    if (( $# == 0 )); then
        ecerr "Usage: $0 <post_url_or_shortcode>..."
        return 1
    fi

    local i shortcode
    for i in "$@"; do
        shortcode=''
        if [[ "$i" =~ '(?:/p/|/reel/|/tv/)([^/?#]+)' ]]; then
            shortcode="${match[1]}"
        elif [[ "$i" != */* ]]; then
            shortcode="$i"
        fi

        if [[ -z "$shortcode" ]]; then
            ecerr "Error: Could not determine shortcode from input: ${i}"
            continue
        fi

        revaldbg instaloader -- "-${shortcode}" || {
            ecerr "Failed to download post: ${shortcode}"
            continue
        }
    done
}

alias insta='\noglob instaloader-post'

function jinsta {
    jee
    instaloader-post "$@"
    jup
}
noglobfn jinsta
##
