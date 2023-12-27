typeset -g TRASH_DIR=~/.trash_rip
##
function trs {
    : "@warn using 'symlinked_dir/' will delete the underlying dir, not the symlink."

    local i
    for i in "$@"
    do
        [[ -e "$i" ]] && {
            # ec Trying to remove "$i"
            if (( ${+commands[rip]} )) ; then
                assert reval-ec rip -- "$i" @RET
            elif (( ${+commands[trash]} )) ; then
                assert reval-ec command trash -- "$i" @RET #: bloody slow
            else
                ecgray "WARNING: 'trash' or 'rip' commands not found, switching to 'rm'."

                assert reval-ec command rm -r -- "$i" @RET
            fi
        }
    done
}
function trs-empty {
    trs-rm "${TRASH_DIR}" /tmp/graveyard-*(DN) || true
    assert empty-trash @RET
}
##
function rip {
    command rip --graveyard "${TRASH_DIR}" "$@"
}

function trs-restore {
    rip -u "$@"
    # -s, --seance       Prints files that were sent under the current directory
    #
    # u, --unbury <target>       Undo the last removal by the current user, or specify some file(s) in the graveyard.  Combine with -s to restore everything printed by -s.
}

function trs-restore-current-dir {
    rip -su
    # -s, --seance       Prints files that were sent under the current directory
    #
    # u, --unbury <target>       Undo the last removal by the current user, or specify some file(s) in the graveyard.  Combine with -s to restore everything printed by -s.
}

function trs-log-current-dir {
    rip -s
    # -s, --seance       Prints files that were sent under the current directory
}
##
