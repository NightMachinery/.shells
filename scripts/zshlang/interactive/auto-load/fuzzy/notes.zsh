function nt-link() {
    local q="$(fz-createquery "$@")"

    local i s
    s="$(fd --base-directory "$nightNotes" | fzp "$q")" @RET
    for i in "${(@f)s}" ; do
        # fd is already returning paths relative to its root dir
        # ec "[[nightNotes:$(realpath --relative-to "$nightNotes" "$i")]]"
        ec "[[nightNotes:$i]]"
    done > >(pbcopy) | cat
}
aliasfn ntg nt-link # nt get
