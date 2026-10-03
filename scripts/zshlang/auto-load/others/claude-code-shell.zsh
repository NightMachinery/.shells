##
#: Claude Code's Bash tool sources a session snapshot after .zshenv, and the
#: snapshot opens with `unalias -a', which would delete every alias .zshenv
#: just defined. ~/.zshrc keeps aliases out of the snapshot, so without this a
#: command there has none at all. See =docs/claude-code-shell.md=.
function h-claude-code-snapshot-lean-p {
    #: Match the current generator's exact preamble and empty function section.
    #: An older snapshot must clear aliases before parsing function bodies,
    #: since an alias of a function name can stop the file with a parse error.
    #: The same bounded scan verifies the opening call's line when supplied;
    #: a later unalias call or any unknown layout must use the builtin.
    local file="${1}" caller_line="${2:-}" line expected
    local -i n=0 opening_line=0
    {
        for expected in \
            '# Snapshot file' \
            '# Unset all aliases to avoid conflicts with functions' \
            'unalias -a 2>/dev/null || true' \
            '# Functions' \
            '# Shell Options' ; do
            (( ++n ))
            IFS= read -r line || return 1
            [[ "${line}" == "${expected}" ]] || return 1
            if [[ "${line}" == 'unalias -a 2>/dev/null || true' ]] ; then
                opening_line=${n}
            fi
        done
        [[ -z "${caller_line}" || "${caller_line}" == "${opening_line}" ]]
    } < "${file}"
}

if [[ -n "${CLAUDECODE:-}" ]] ; then
    function unalias {
        #: Skip only a lean snapshot's own `unalias -a'; everything else, its
        #: `unalias grep' before Claude Code's grep wrapper included, is the
        #: builtin.
        if [[ "$*" == '-a' ]] ; then
            local caller="${funcfiletrace[1]%:*}"
            local caller_line="${funcfiletrace[1]##*:}"
            if [[ "${caller}" == */shell-snapshots/snapshot-*.sh ]] &&
                h-claude-code-snapshot-lean-p "${caller}" "${caller_line}" 2>/dev/null ; then
                local name body
                for name in "${(@k)galiases}" ; do
                    body="${galiases[$name]}"
                    builtin unalias -- "${name}"
                    builtin alias -- "${name}=${body}"
                done
                return 0
            fi
        fi
        builtin unalias "$@"
    }
fi
##
