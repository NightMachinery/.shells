##
#: Claude Code's Bash tool sources a session snapshot after .zshenv, and the
#: snapshot opens with `unalias -a', which would delete every alias .zshenv
#: just defined. ~/.zshrc keeps aliases out of the snapshot, so without this a
#: command there has none at all. See =docs/claude-code-shell.md=.
function h-claude-code-snapshot-lean-p {
    #: Whether snapshot file $1 carries no function bodies: the line after its
    #: `# Functions' header is already the next header. Only such a snapshot
    #: may keep our aliases, since the bodies in an older one are parsed as it
    #: is sourced, and an alias of a function's name (`ls', say) turns its
    #: definition into a parse error that stops the rest of the file. Reads a
    #: few lines, not the file; any other layout answers no.
    local file="${1}" line
    local -i n=0
    {
        while IFS= read -r line ; do
            (( ++n > 20 )) && return 1
            [[ "${line}" == '# Functions' ]] && break
        done
        IFS= read -r line || return 1
        [[ "${line}" == '# '* ]]
    } < "${file}"
}

if [[ -n "${CLAUDECODE:-}" ]] ; then
    function unalias {
        #: Skip only a lean snapshot's own `unalias -a'; everything else, its
        #: `unalias grep' before Claude Code's grep wrapper included, is the
        #: builtin.
        if [[ "$*" == '-a' ]] ; then
            local caller="${funcfiletrace[1]%:*}"
            if [[ "${caller}" == */shell-snapshots/snapshot-*.sh ]] &&
                h-claude-code-snapshot-lean-p "${caller}" 2>/dev/null ; then
                return 0
            fi
        fi
        builtin unalias "$@"
    }
fi
##
