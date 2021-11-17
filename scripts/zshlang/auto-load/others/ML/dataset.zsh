##
function dataset-categorize {
    local categories
    categories=( ${(@f)"$(cat)"} undo ) @TRET

    local imgs=( *(DN.) )
    local i=1 sel dest last_src
    while (( i <= ${#imgs} )) ; do
        src="${imgs[$i]}"
        sel="$(arrN $categories \
            | fzf_mru_context="${0}:${(j.:.)categories}" \
            fz --preview-window="top,50%,nowrap,nohidden" \
            --preview="kitty +kitten icat --transfer-mode file --align=left --place="10x10@1x1"\
            $(gq "$src")")" @TRET

        if [[ "$sel" == undo ]] ; then
            test -n "$last_src" @TRET

            ecbold "Undoing:"
            gmv -i -v "$dest" "$last_src" @TRET

            last_src=''
            i=$(( i - 1 ))

            # sleep 0.5
        else
            last_src="$src"
            dest="${sel}/$src"
            ensure-dir "$dest"
            gmv -i -v "$src" "$dest" @TRET

            i=$(( i + 1 ))
        fi
done
}
##
