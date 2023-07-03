function nt-link() {
    if (( $#@ == 0 )) ; then
        set -- "$(pbpaste | prefixer -r "$nightNotes")"
    fi
    local q="$(fz-createquery "$@")"

    local i s
    s="$(fd --base-directory "$nightNotes" | fzp --select-1 "$q")" @RET
    for i in "${(@f)s}" ; do
        # fd is already returning paths relative to its root dir
        # ec "[[nightNotes:$(grealpath --relative-to "$nightNotes" "$i")]]"
        ecn "[[nightNotes:$i]]"
    done > >(pbcopy) | cat
}
aliasfn ntg nt-link # nt get
