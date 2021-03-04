trs() {
    local i
    for i in "$@"
    do
        [[ -e "$i" ]] && {
            ec Trying to remove "$i"
            if (( ${+commands[trash]} )) ; then
                trash -- "$i"
            else
                ecerr "WARNING: 'trash' command not found, switching to 'rm'."
                rm -r -- "$i"
            fi
        }
    done
}
aliasfn trs-empty empty-trash
