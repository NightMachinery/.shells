##
#: Assembles each agent's global instruction file from shared, private,
#: per-agent, per-host and machine-local parts.
#:
#: Assembly rather than symlinks or `@` imports, because Codex supports
#: neither: it reads one =AGENTS.md= and does not inline =@path= references
#: (openai/codex#17401). Claude does support imports and Antigravity has its
#: own =GEMINI.md= tier, but using those would make the local and per-host
#: layers reach some agents and silently not others.
#:
#: See =PE/Agents/readme.org=.
##
function h-agents-md-agents {
    #: The agents to assemble for, as `name<TAB>output path`.
    ##
    ec "claude"$'\t'"${HOME}/.claude/CLAUDE.md"
    ec "codex"$'\t'"${HOME}/.codex/AGENTS.md"
    ec "gapcode"$'\t'"${HOME}/.gapcode/AGENTS.md"
    ec "gemini"$'\t'"${HOME}/.gemini/AGENTS.md"
}

function h-agents-md-parts {
    #: The source files for one agent, broad to narrow: whatever comes last
    #: wins where they disagree. Missing parts are normal and skipped.
    ##
    local agent="${1}"

    local dir="${agents_md_dir:-${NIGHTDIR}/PE/Agents}"
    local private_dir="${agents_md_private_dir:-${nightNotesPrivate}/configs/agents}"
    local host="${agents_md_host:-${HOST:-$(hostname)}}"

    #: every agent, every host
    ec "${dir}/AGENTS.md"
    ec "${private_dir}/AGENTS.md"
    #: this agent, every host.
    #: `agent-` prefixed because a bare `claude.md` is the same file as
    #: `CLAUDE.md` on a case-insensitive filesystem, and Claude Code would
    #: discover this source as a nested project memory and load the tier
    #: twice -- on macOS only, so the two would also disagree per host.
    ec "${dir}/agent-${agent}.md"
    ec "${private_dir}/agent-${agent}.md"
    #: every agent, this host
    ec "${dir}/hosts/${host}.md"
    ec "${private_dir}/hosts/${host}.md"
    #: this machine, untracked
    ec "${HOME}/.agents.local.md"
    ec "${HOME}/.${agent}.local.md"
}

function h-agents-md-assemble {
    #: Prints the assembled instruction file for one agent.
    ##
    local agent="${1}"

    local -a parts
    #: `:a` normalizes, so a NIGHTDIR carrying a trailing slash cannot change
    #: the assembled bytes and make an up-to-date file read as stale.
    parts=("${(@f)$(h-agents-md-parts "${agent}")}") @TRET
    parts=("${parts[@]:a}")

    #: Claude strips block-level HTML comments before loading, so the
    #: provenance costs nothing there and little anywhere else.
    ec "<!-- Assembled by agents-md-sync. Edit the sources, not this file. -->"

    local p first=y
    for p in "${parts[@]}" ; do
        test -s "${p}" || continue

        if test -z "${first}" ; then
            ec
        fi
        first=''

        ec "<!-- ${p/#${HOME}/~} -->"
        command cat -- "${p}" @RET
    done
}

function agents-md-sync {
    #: Rewrites every agent's instruction file from its sources. Idempotent,
    #: and only touches a file whose content actually changed, so mtimes stay
    #: meaningful. Called by the agent launchers, so it is on a hot path: keep
    #: it to reading a handful of small files.
    ##
    local verbose_p="${agents_md_sync_verbose_p:-n}"

    local line agent target assembled
    for line in "${(@f)$(h-agents-md-agents)}" ; do
        agent="${line%%$'\t'*}"
        target="${line#*$'\t'}"

        #: Only for agents that are actually installed here.
        test -d "${target:h}" || continue

        assembled="$(h-agents-md-assemble "${agent}")" @TRET

        #: Before the content check, not after: a symlink to another agent's
        #: file reads as up to date for exactly as long as the two assemble
        #: identically, and then silently stops doing so. Writing through it
        #: would also clobber whatever it points at.
        if test -L "${target}" ; then
            command rm -f -- "${target}" @RET
        elif test -e "${target}" && [[ "$(command cat -- "${target}")" == "${assembled}" ]] ; then
            continue
        fi

        ec "${assembled}" > "${target}" @RET
        if bool "${verbose_p}" ; then
            ecgray "$0: wrote ${target/#${HOME}/~}"
        fi
    done
}

function agents-md-doctor {
    #: Reports what each agent actually loads, which sources exist, and
    #: whether the written file still matches them.
    #:
    #: This exists because =~/.claude/VPS.md= and =~/.claude/instructions.md=
    #: sat symlinked into place for months while Claude Code, which reads only
    #: =CLAUDE.md=, never loaded either. Silent is the failure mode worth
    #: engineering against.
    ##
    local line agent target assembled p
    for line in "${(@f)$(h-agents-md-agents)}" ; do
        agent="${line%%$'\t'*}"
        target="${line#*$'\t'}"

        ecbold "${agent} -> ${target/#${HOME}/~}"

        if ! test -d "${target:h}" ; then
            ecgray "  not installed on this host"
            continue
        fi

        for p in "${${(@f)$(h-agents-md-parts "${agent}")}[@]:a}" ; do
            if test -s "${p}" ; then
                ec "  + $(wc -l < "${p}" | tr -d ' ')L  ${p/#${HOME}/~}"
            elif test -e "${p}" ; then
                ecgray "  · empty  ${p/#${HOME}/~}"
            else
                ecgray "  - absent ${p/#${HOME}/~}"
            fi
        done

        assembled="$(h-agents-md-assemble "${agent}")" @TRET

        if ! test -e "${target}" ; then
            ecerr "  MISSING: run agents-md-sync"
        elif [[ "$(command cat -- "${target}")" != "${assembled}" ]] ; then
            ecerr "  STALE: differs from its sources, run agents-md-sync"
        else
            local lines="${#${(@f)assembled}}"
            ec "  = ${lines}L, in sync"
            #: Claude Code's docs ask for under 200 lines; adherence drops on
            #: longer files, and every agent pays the context either way.
            if (( lines > 200 )) ; then
                ecerr "  OVERSIZED: ${lines} lines, trim the sources"
            fi
        fi
    done
}
##
