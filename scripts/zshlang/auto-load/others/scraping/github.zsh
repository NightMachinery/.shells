##
function gh-to-readme() {
    local urls=() i i2 readme url
    local exts=(md rst org)
    local readmes=(readme README ReadMe readMe Readme)
    local branches=(master main develop)

    for i in "$@"
    do
        if [[ "$i" =~ 'github.com' ]] || ! [[ "$i" == *.(${(j.|.)~exts}) ]] ; then
            i2="${i}.md"
            comment we hope to handle wiki pages with this method, but beware that nonexistent wiki pages trigger create a new page, not the desired not existent response.

            local candidates=("$i2"
                              "${i}/raw/${^branches[@]}/${^readmes[@]}.${^exts[@]}")
            dvar candidates
            unset url_exists_out
            re-any url-exists $candidates[@]
            i="$url_exists_out"
        fi
        url-exists "$i" && urls+="$i" || ecerr "$0: URL $(gquote-dq "$i") does not seem to exist. (Empty URL means no URL found.)"
    done

    arrnn "$urls[@]"
}

function gh-to-raw() {
    in-or-args "$@" | sd '/blob/' '/raw/'
}

function w2e-gh() {
    h2ed=html2epub-pandoc-simple w2e-curl "$1" "${(@f)$(gh-to-readme "${@:2}")}"
}
noglobfn w2e-gh
##
