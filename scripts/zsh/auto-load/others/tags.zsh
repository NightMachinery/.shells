function ntag-has() {
    local f="$1" tag="$2"
    
    [[ "$f" == *.${tag}.* ]]
}
function ntag-add() {
    local f="$1" tags=("${@:2}") tag toadd=()
    local ft="${f:t}"
    test -e "$f" || {
        ecerr "$0: Nonexistent file: $f"
        return 1
    }
    for tag in $tags[@] ; do
        if ! ntag-has "$ft" "$tag" ; then
            toadd+="$tag"
        fi
    done
    test -z "$toadd[*]" || reval-ec mv "$f" "${f:r}.${(j/./)toadd}.${f:e}"
}
alias tg=ntag-add
## fuzzy
function ntag-search() {
    local query="$(mg_sep=' ' mapg "\'.\$i." "$@")"

    ##
    # local nightNotes="${ntag_search_dir:-.}"
    # ntsearch_glob='' ntsearch_rg_opts=(-uuu) ntl-fzf "$query"
    ##
    fd | fz --query "$query"
}
aliasfn tgs ntag-search
##
