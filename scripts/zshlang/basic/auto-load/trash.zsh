typeset -g TRASH_DIR=~/.trash_rip
##
function trs() {
    local i
    for i in "$@"
    do
        [[ -e "$i" ]] && {
            ec Trying to remove "$i"
            if (( ${+commands[rip]} )) ; then
                assert rip -- "$i" @RET
            elif (( ${+commands[trash]} )) ; then
                assert trash -- "$i" @RET # bloody slow
            else
                ecerr "WARNING: 'trash' or 'rip' commands not found, switching to 'rm'."
                assert rm -r -- "$i" @RET
            fi
        }
    done
}
function trs-empty {
    trs-rm "${TRASH_DIR}" || true
    assert empty-trash @RET
}
##
function rip {
    command rip --graveyard "${TRASH_DIR}" "$@"
}

function trs-restore {
    rip -su
    # -s, --seance       Prints files that were sent under the current directory
    #
    # u, --unbury <target>       Undo the last removal by the current user, or specify some file(s) in the graveyard.  Combine with -s to restore everything printed by -s.
}
