function opera-bookmarks-dir-json() {
    if isDarwin ; then
        typeset -g OPERA_BOOKMARKS="$HOME/Library/Application Support/com.operasoftware.Opera/Bookmarks"
    else
        ectrace "Not implemented yet"
        return $?
    fi

    assert test -e "$OPERA_BOOKMARKS" @RET

    cat $OPERA_BOOKMARKS | jq '[.roots.custom_root.speedDial.children[0].children[] | select(.type="folder") ] | sort_by(.date_modified) | reverse'
}
function opera-bookmarks-dir-fz-json() {
    local q="$(fz-createquery "$@")"

    local d="$(opera-bookmarks-dir-json)"
    local names
    names=( ${(@f)"$(ecn "$d" | jqm '.[] | .name' | fzp --no-sort "$q")"} ) @RET

    local name
    for name in $names[@] ; do
        ecn "$d" | jq --arg n "$name" '.[] | select(.name == $n)'
    done
}
function opera-bookmarks-dir-fz() {
    opera-bookmarks-dir-fz-json "$@" | jqm '.children[] | .url'
}
##
