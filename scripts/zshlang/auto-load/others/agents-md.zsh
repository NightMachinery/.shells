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
    #: The agents to assemble for, as `name<TAB>output path<TAB>mode`.
    #:
    #: The mode is empty for an ordinary target and `overlay` for one that
    #: assembles only its own per-agent tiers; see [agfi:h-agents-md-parts].
    ##
    ec "claude"$'\t'"${HOME}/.claude/CLAUDE.md"
    ec "codex"$'\t'"${HOME}/.codex/AGENTS.md"
    #: gapcode is not in use. ~/.gapcode/AGENTS.md is left as it was last
    #: assembled; nothing rewrites it now.
    # ec "gapcode"$'\t'"${HOME}/.gapcode/AGENTS.md"
    #: Antigravity's binary is `agy`; its state lives under ~/.gemini/, and
    #: its global rules file is GEMINI.md there, per
    #: https://antigravity.google/docs/rules-workflows . A global AGENTS.md is
    #: read by nothing: that path is workspace-scoped only.
    ec "antigravity"$'\t'"${HOME}/.gemini/GEMINI.md"
    #: [agfi:claude-work] is Claude Code on a second config home, so a second
    #: account. Claude Code takes its user memory from
    #: `$CLAUDE_CONFIG_DIR/CLAUDE.md`, which is this path.
    #:
    #: An *overlay*, because the shared spine already reaches that profile
    #: without us. Claude Code also walks every ancestor of the session's cwd
    #: looking for `<dir>/CLAUDE.md` and `<dir>/.claude/CLAUDE.md`, and $HOME is
    #: an ancestor of every project here -- so ~/.claude/CLAUDE.md loads as
    #: *project* memory. Under ~/.claude itself that pickup is deduplicated
    #: against the user-tier file; here there is nothing to dedup against, so
    #: writing the spine again would carry all of it twice.
    #:
    #: The cost is that the spine reaches a work session only when its cwd is
    #: under $HOME. [agfi:agents-md-doctor] says so out loud rather than
    #: leaving the dependency implicit.
    ec "claude-work"$'\t'"${HOME}/.claude-work/CLAUDE.md"$'\t'"overlay"
}

function h-agents-md-parts {
    #: The source files for one agent, broad to narrow: whatever comes last
    #: wins where they disagree. Missing parts are normal and skipped.
    #:
    #: In `overlay` mode only the agent's own tiers are emitted; the shared
    #: spine, the host tier and ~/.agents.local.md are left out, because an
    #: overlay is a second profile of an agent that already receives those by
    #: another route. See [agfi:h-agents-md-agents].
    #: Usage: h-agents-md-parts <agent> [mode]
    ##
    local agent="${1}" mode="${2}"

    local dir="${agents_md_dir:-${NIGHTDIR}/PE/Agents}"
    local private_dir="${agents_md_private_dir:-${nightNotesPrivate}/configs/agents}"
    local host="${agents_md_host:-${HOST:-$(hostname)}}"

    if [[ "${mode}" != overlay ]] ; then
        #: every agent, every host
        ec "${dir}/AGENTS.md"
        ec "${private_dir}/AGENTS.md"
    fi
    #: this agent, every host.
    #: `agent-` prefixed because a bare `claude.md` is the same file as
    #: `CLAUDE.md` on a case-insensitive filesystem, and Claude Code would
    #: discover this source as a nested project memory and load the tier
    #: twice -- on macOS only, so the two would also disagree per host.
    ec "${dir}/agent-${agent}.md"
    ec "${private_dir}/agent-${agent}.md"
    if [[ "${mode}" != overlay ]] ; then
        #: every agent, this host
        ec "${dir}/hosts/${host}.md"
        ec "${private_dir}/hosts/${host}.md"
        #: this machine, untracked
        ec "${HOME}/.agents.local.md"
    fi
    ec "${HOME}/.${agent}.local.md"
}

function h-agents-md-assemble {
    #: Prints the assembled instruction file for one agent, or nothing at all
    #: when no source contributed anything. That is the normal state of an
    #: overlay whose tiers are still empty, and [agfi:agents-md-sync] reads it
    #: as "there should be no file here".
    #: Usage: h-agents-md-assemble <agent> [mode]
    ##
    local agent="${1}" mode="${2}"

    local -a parts
    #: `:a` normalizes, so a NIGHTDIR carrying a trailing slash cannot change
    #: the assembled bytes and make an up-to-date file read as stale.
    parts=("${(@f)$(h-agents-md-parts "${agent}" "${mode}")}") @TRET
    parts=("${parts[@]:a}")

    local p first=y
    for p in "${parts[@]}" ; do
        test -s "${p}" || continue

        if test -n "${first}" ; then
            #: Claude strips block-level HTML comments before loading, so the
            #: provenance costs nothing there and little anywhere else. Held
            #: back until the first real part so that an empty assembly comes
            #: out empty, rather than as a lone header.
            ec "<!-- Assembled by agents-md-sync. Edit the sources, not this file. -->"
        else
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

    local line agent target mode assembled
    local -a fields
    for line in "${(@f)$(h-agents-md-agents)}" ; do
        #: `p` is what makes `\t` a tab, and `@` inside the quotes is what
        #: keeps a missing third field empty instead of shifting the others.
        fields=("${(@ps:\t:)line}")
        agent="${fields[1]}"
        target="${fields[2]}"
        mode="${fields[3]}"

        #: Only for agents that are actually installed here.
        test -d "${target:h}" || continue

        assembled="$(h-agents-md-assemble "${agent}" "${mode}")" @TRET

        if test -z "${assembled}" ; then
            if [[ "${mode}" != overlay ]] ; then
                #: Every source for an ordinary target is missing or empty,
                #: which is a broken checkout rather than an intended state.
                #: Leave what is on disk alone instead of replacing a good
                #: file with nothing.
                ecerr "$0: no sources for ${agent}; leaving ${target/#${HOME}/~} as it is"
                continue
            fi

            #: An overlay with nothing in it yet. A file holding only the
            #: provenance comment is noise, and a leftover one is a lie, so
            #: the right state is no file at all.
            if test -e "${target}" || test -L "${target}" ; then
                command rm -f -- "${target}" @RET
                if bool "${verbose_p}" ; then
                    ecgray "$0: removed empty ${target/#${HOME}/~}"
                fi
            fi
            continue
        fi

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
    local line agent target mode assembled p
    local -a fields
    for line in "${(@f)$(h-agents-md-agents)}" ; do
        fields=("${(@ps:\t:)line}")
        agent="${fields[1]}"
        target="${fields[2]}"
        mode="${fields[3]}"

        if [[ "${mode}" == overlay ]] ; then
            ecbold "${agent} -> ${target/#${HOME}/~} (overlay)"
        else
            ecbold "${agent} -> ${target/#${HOME}/~}"
        fi

        if ! test -d "${target:h}" ; then
            ecgray "  not installed on this host"
            continue
        fi

        if [[ "${mode}" == overlay ]] ; then
            #: The whole point of the doctor is that a silent dependency is
            #: worse than a loud one, and this target has one.
            ecgray "  the shared spine is not written here. It arrives as project"
            ecgray "  memory from ~/.claude/CLAUDE.md, which Claude Code picks up"
            ecgray "  walking down from \$HOME -- so only while the session's cwd"
            ecgray "  is under \$HOME."
        fi

        for p in "${${(@f)$(h-agents-md-parts "${agent}" "${mode}")}[@]:a}" ; do
            if test -s "${p}" ; then
                ec "  + $(wc -l < "${p}" | tr -d ' ')L  ${p/#${HOME}/~}"
            elif test -e "${p}" ; then
                ecgray "  · empty  ${p/#${HOME}/~}"
            else
                ecgray "  - absent ${p/#${HOME}/~}"
            fi
        done

        assembled="$(h-agents-md-assemble "${agent}" "${mode}")" @TRET

        if test -z "${assembled}" ; then
            if [[ "${mode}" != overlay ]] ; then
                ecerr "  NO SOURCES: nothing to assemble from"
            elif test -e "${target}" ; then
                ecerr "  STALE: every source is empty, but the file is still there;"
                ecerr "  run agents-md-sync"
            else
                ec "  = empty overlay, nothing written"
            fi
        elif ! test -e "${target}" ; then
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

    h-agents-md-doctor-settings
}

typeset -ga agents_md_settings=(
    "${HOME}/.claude/settings.json"$'\t'"${NIGHTDIR}/configFiles/claude-code/settings.json"
    #: [agfi:claude-work] shares the tracked settings with the personal
    #: profile; only the account, history and projects differ.
    "${HOME}/.claude-work/settings.json"$'\t'"${NIGHTDIR}/configFiles/claude-code/settings.json"
)

function h-agents-md-doctor-settings {
    : "reports whether each agent's settings file is still the tracked one

Claude Code rewrites settings.json whenever a setting changes in the app. If it
ever does so by rename rather than in place, the symlink is replaced by a plain
file and the config silently stops being tracked -- the same class of quiet
failure this doctor exists for."
    ##
    local line target tracked resolved

    for line in "${agents_md_settings[@]}" ; do
        target="${line%%$'\t'*}"
        tracked="${${line#*$'\t'}:A}"

        ecbold "settings: ${target/#${HOME}/~}"

        if ! test -d "${target:h}" ; then
            ecgray "  not installed on this host"
            continue
        fi

        if ! test -e "${tracked}" ; then
            ecerr "  MISSING: ${tracked/#${HOME}/~} is not in the repo"
            continue
        fi

        if ! test -e "${target}" ; then
            ecerr "  MISSING: expected a symlink to ${tracked/#${HOME}/~}"
        elif ! test -L "${target}" ; then
            ecerr "  UNTRACKED: a plain file, not a symlink; the app replaced it."
            ecerr "  Diff it against ${tracked/#${HOME}/~}, keep what you want, and re-link."
        else
            resolved="${target:A}"
            if [[ "${resolved}" == "${tracked:A}" ]] ; then
                ec "  = symlinked to ${tracked/#${HOME}/~}"
            else
                ecerr "  WRONG TARGET: points at ${resolved/#${HOME}/~}"
            fi
        fi
    done
}
function h-agents-md-sync-ask {
    #: Syncs, and on failure hands the decision to the user instead of making
    #: it for them: aborting the launch over a stale instruction file is too
    #: harsh, and starting silently with one is how the old symlink
    #: arrangement went unnoticed for months.
    ##
    if agents-md-sync ; then
        return 0
    fi

    ecerr "$0: could not assemble the agent instruction files; see the error above."

    if ! isI ; then
        #: Nobody is there to answer, and a prompt no one sees is a hang.
        ecerr "$0: not interactive, launching with the files as they stand."
        return 0
    fi

    ask "Launch anyway, with possibly stale instructions?" N
}
##
