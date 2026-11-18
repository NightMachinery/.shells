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
typeset -g agents_md_header='<!-- Assembled by agents-md-sync. Edit the sources, not this file. -->'

function h-agents-md-agents {
    #: The agents to assemble for, as `name<TAB>output path<TAB>mode`.
    #:
    #: The mode is empty for an ordinary target, `overlay` for one that
    #: assembles only its own per-agent tiers (see [agfi:h-agents-md-parts]),
    #: and `rules` for a directory of size-capped rule files (see
    #: [agfi:h-agents-md-rule-files]).
    ##
    ec "claude"$'\t'"${HOME}/.claude/CLAUDE.md"
    ec "codex"$'\t'"${HOME}/.codex/AGENTS.md"
    #: gapcode is not in use. ~/.gapcode/AGENTS.md is left as it was last
    #: assembled; nothing rewrites it now.
    # ec "gapcode"$'\t'"${HOME}/.gapcode/AGENTS.md"
    #: Antigravity's binary is `agy`. Its global customization root is
    #: ~/.gemini/config, and it caps every rule file at 24,000 bytes, cutting
    #: the rest off silently; a single assembled GEMINI.md lost its last third
    #: that way (seen 2026-11, agy 1.5.8). Always-on rules in config/rules/
    #: have no such limit as a set, only agy's 20,000-token rules budget, so
    #: the assembly is split across several files there. Each needs
    #: `trigger: always_on` frontmatter: agy ignores one without it.
    ec "antigravity"$'\t'"${HOME}/.gemini/config/rules"$'\t'"rules"
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

function h-agents-md-retired {
    #: Files an earlier version of this sync wrote and nothing rewrites now.
    #: [agfi:agents-md-sync] removes each one that still starts with
    #: $agents_md_header, so a file someone wrote by hand is left alone.
    #:
    #: agy loads both of these as global rules, alongside config/rules/: the
    #: first was the antigravity target until 2026-11, the second an earlier
    #: target still, so leaving them would load stale rules next to the
    #: current ones. Google's separate gemini-cli reads them too.
    ##
    ec "${HOME}/.gemini/GEMINI.md"
    ec "${HOME}/.gemini/AGENTS.md"
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
            ec "${agents_md_header}"
        else
            ec
        fi
        first=''

        ec "<!-- ${p/#${HOME}/~} -->"
        command cat -- "${p}" @RET
    done
}

function h-agents-md-bytes {
    : "usage: h-agents-md-bytes <text>; REPLY: its length in bytes"
    #: zsh counts characters, and agy's cap is in bytes.
    setopt localoptions nomultibyte
    REPLY="${#1}"
}

function h-agents-md-sections {
    : "usage: h-agents-md-sections <level> <text>; reply: the text cut before each heading of that level"
    #: Joining reply with newlines gives the text back. A `#` line inside a
    #: fenced code block is a comment in a sample, not a heading. The
    #: provenance comments directly above a heading move with it, so a part
    #: that starts a chunk still names its source.
    ##
    setopt localoptions extendedglob
    local level="${1}" text="${2}"

    local marker=''
    marker="${(l:${level}::#:)marker}"
    local line fence=''
    local -a current carry
    reply=()
    for line in "${(@f)text}" ; do
        if [[ "${line}" == [[:space:]]#('```'|'~~~')* ]] ; then
            if test -n "${fence}" ; then
                fence=''
            else
                fence=y
            fi
        elif [[ -z "${fence}" && "${line}" == "${marker} "* ]] ; then
            carry=()
            while (( ${#current} )) && [[ "${current[-1]}" == '<!-- '*' -->' ]] ; do
                carry=("${current[-1]}" "${carry[@]}")
                current[-1]=()
            done
            if (( ${#current} )) ; then
                reply+=("${(pj:\n:)current}")
            fi
            current=("${carry[@]}")
        fi
        current+=("${line}")
    done
    if (( ${#current} )) ; then
        reply+=("${(pj:\n:)current}")
    fi
}

function h-agents-md-pack {
    : "usage: h-agents-md-pack <max bytes> <text> [<level>]; reply: the text in chunks of at most max bytes"
    #: Cuts only at headings, at the shallowest level that gets every chunk
    #: under the limit, and fills each chunk greedily. Joining reply with
    #: newlines gives the text back.
    ##
    local max="${1}" text="${2}" level="${3:-1}"

    local -a sections chunks
    h-agents-md-sections "${level}" "${text}"
    sections=("${reply[@]}")

    local section size current='' current_p='' current_size=0
    for section in "${sections[@]}" ; do
        h-agents-md-bytes "${section}"
        size="${REPLY}"
        if (( size > max )) ; then
            if (( level >= 6 )) ; then
                ecerr "agents-md-sync: a section with no subheadings is over ${max} bytes; split it in its source:"
                ecerr "  ${section%%$'\n'*}"
                return 1
            fi
            if test -n "${current_p}" ; then
                chunks+=("${current}")
            fi
            current='' current_p='' current_size=0
            h-agents-md-pack "${max}" "${section}" "$(( level + 1 ))" @RET
            chunks+=("${reply[@]}")
        elif test -z "${current_p}" ; then
            current="${section}" current_p=y current_size="${size}"
        elif (( current_size + 1 + size > max )) ; then
            chunks+=("${current}")
            current="${section}" current_size="${size}"
        else
            current+=$'\n'"${section}"
            (( current_size += 1 + size ))
        fi
    done
    if test -n "${current_p}" ; then
        chunks+=("${current}")
    fi
    reply=("${chunks[@]}")
}

function h-agents-md-rule-files {
    : "usage: h-agents-md-rule-files <assembled>; reply: the content of each rule file, in order"
    #: Every file opens with the frontmatter agy needs to load it at all, then
    #: $agents_md_header, so each one says on its own that it is generated.
    #: The default chunk size leaves room for both, and for the sources to
    #: grow a little, under agy's 24,000-byte cap.
    ##
    local max="${agents_md_rule_max_bytes:-22000}"
    local assembled="${1}"

    local body="${assembled}"
    if [[ "${body}" == "${agents_md_header}"$'\n'* ]] ; then
        body="${body[$(( ${#agents_md_header} + 2 )),-1]}"
    fi
    h-agents-md-pack "${max}" "${body}" @RET

    local chunk
    local -a files
    for chunk in "${reply[@]}" ; do
        #: A chunk ends where the next heading began, usually after a blank
        #: line. Kept, it would never compare equal to the file: `$(...)`
        #: strips every trailing newline when the file is read back.
        while [[ "${chunk}" == *$'\n' ]] ; do
            chunk="${chunk%$'\n'}"
        done
        files+=($'---\ntrigger: always_on\n---\n\n'"${agents_md_header}"$'\n'"${chunk}")
    done
    reply=("${files[@]}")
}

function h-agents-md-rule-path {
    : "usage: h-agents-md-rule-path <directory> <n>; REPLY: the nth rule file"
    #: Zero-padded, since agy reads the directory in name order.
    REPLY="${1}/agents-md-${(l:2::0:)2}.md"
}

function h-agents-md-sync-rules {
    : "usage: h-agents-md-sync-rules <directory> <assembled>; rewrites the rule files that changed and removes the rest"
    setopt localoptions bareglobqual
    local verbose_p="${agents_md_sync_verbose_p:-y}"
    local dir="${1}" assembled="${2}"

    h-agents-md-rule-files "${assembled}" @RET
    local -a contents=("${reply[@]}") wanted
    command mkdir -p -- "${dir}" @RET

    local i f
    for i in {1..${#contents}} ; do
        h-agents-md-rule-path "${dir}" "${i}"
        f="${REPLY}"
        wanted+=("${f}")
        if test -L "${f}" ; then
            command rm -f -- "${f}" @RET
        elif test -e "${f}" && [[ "$(command cat -- "${f}")" == "${contents[i]}" ]] ; then
            continue
        fi
        ec "${contents[i]}" > "${f}" @RET
        if bool "${verbose_p}" ; then
            ecgray "agents-md-sync: wrote ${f/#${HOME}/~}"
        fi
    done

    #: A shorter assembly needs fewer files, and a leftover one would still
    #: load, out of date.
    for f in "${dir}"/agents-md-<->.md(N) ; do
        if (( ! ${wanted[(Ie)${f}]} )) ; then
            command rm -f -- "${f}" @RET
            if bool "${verbose_p}" ; then
                ecgray "agents-md-sync: removed extra ${f/#${HOME}/~}"
            fi
        fi
    done
}

function h-agents-md-retired-ours-p {
    : "usage: h-agents-md-retired-ours-p <file>; succeeds for a regular file this sync wrote"
    local f="${1}" first=''
    test -f "${f}" && ! test -L "${f}" || return 1
    { IFS= read -r first || true } < "${f}"
    [[ "${first}" == "${agents_md_header}" ]]
}

function h-agents-md-remove-retired {
    : "removes the retired outputs this sync wrote; see h-agents-md-retired"
    local verbose_p="${agents_md_sync_verbose_p:-y}"
    local f
    for f in "${(@f)$(h-agents-md-retired)}" ; do
        h-agents-md-retired-ours-p "${f}" || continue
        command rm -f -- "${f}" @RET
        if bool "${verbose_p}" ; then
            ecgray "agents-md-sync: removed retired ${f/#${HOME}/~}"
        fi
    done
}

function agents-md-sync {
    #: Rewrites every agent's instruction file from its sources. Idempotent,
    #: and only touches a file whose content actually changed, so mtimes stay
    #: meaningful. Called by the agent launchers, so it is on a hot path: keep
    #: it to reading a handful of small files. Report changed targets only.
    ##
    local verbose_p="${agents_md_sync_verbose_p:-y}"

    local line agent target mode assembled broken=''
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
                broken=y
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

        if [[ "${mode}" == rules ]] ; then
            h-agents-md-sync-rules "${target}" "${assembled}" @RET
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

    #: After the new files are written, so agy is never left with neither.
    #: A broken checkout leaves the old files alone too.
    if test -z "${broken}" ; then
        h-agents-md-remove-retired @RET
    fi
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
        elif [[ "${mode}" == rules ]] ; then
            h-agents-md-doctor-rules "${target}" "${assembled}"
            h-agents-md-doctor-lines "${assembled}"
        elif ! test -e "${target}" ; then
            ecerr "  MISSING: run agents-md-sync"
        elif [[ "$(command cat -- "${target}")" != "${assembled}" ]] ; then
            ecerr "  STALE: differs from its sources, run agents-md-sync"
        else
            h-agents-md-bytes "${assembled}"
            ec "  = ${#${(@f)assembled}}L, ${REPLY}B, in sync"
            h-agents-md-doctor-lines "${assembled}"
        fi
    done

    h-agents-md-doctor-retired
    h-agents-md-doctor-settings
    h-agent-skills-doctor
}

function h-agents-md-doctor-lines {
    : "usage: h-agents-md-doctor-lines <assembled>; warns when it has too many lines or bytes"
    local lines="${#${(@f)1}}"
    #: Claude Code's docs ask for under 200 lines; adherence drops on longer
    #: files, and every agent pays the context either way.
    if (( lines > 200 )) ; then
        ecerr "  OVERSIZED: ${lines} lines, trim the sources"
    fi

    #: Codex counts the global file against `project_doc_max_bytes` (32 KiB by
    #: default) and gives project AGENTS.md files only what is left; at 37 KB
    #: they were silently truncated to nothing. Warn well before the default,
    #: so growth shows up here rather than as lost project instructions.
    local max="${agents_md_max_bytes:-30000}"
    h-agents-md-bytes "${1}"
    if (( REPLY > max )) ; then
        ecerr "  OVERSIZED: ${REPLY} bytes (warning at ${max}), trim the sources"
    fi
}

function h-agents-md-doctor-rules {
    : "usage: h-agents-md-doctor-rules <directory> <assembled>; reports each rule file of a rules target"
    setopt localoptions bareglobqual
    #: agy's documented cap, per file. Its 20,000-token rules budget covers
    #: every always-on rule, the workspace's AGENTS.md included; rules past it
    #: are reduced to a path the agent may or may not read.
    local cap="${agents_md_rule_cap_bytes:-24000}"
    local dir="${1}" assembled="${2}"

    if ! h-agents-md-rule-files "${assembled}" ; then
        ecerr "  CANNOT SPLIT: see the error above"
        return 0
    fi
    local -a contents=("${reply[@]}") wanted

    local i f size total=0 bad=''
    for i in {1..${#contents}} ; do
        h-agents-md-rule-path "${dir}" "${i}"
        f="${REPLY}"
        wanted+=("${f}")
        h-agents-md-bytes "${contents[i]}"
        #: `ec` adds the final newline.
        size="$(( REPLY + 1 ))"
        (( total += size ))
        if ! test -e "${f}" ; then
            ecerr "  MISSING: ${f/#${HOME}/~}"
            bad=y
        elif [[ "$(command cat -- "${f}")" != "${contents[i]}" ]] ; then
            ecerr "  STALE: ${f/#${HOME}/~} differs from its sources"
            bad=y
        else
            ec "  = ${size}B  ${f/#${HOME}/~}"
        fi
        if (( size > cap )) ; then
            ecerr "  OVER CAP: ${f:t} is ${size} bytes, and agy cuts every rule file at ${cap}"
        fi
    done
    for f in "${dir}"/agents-md-<->.md(N) ; do
        if (( ! ${wanted[(Ie)${f}]} )) ; then
            ecerr "  EXTRA: ${f/#${HOME}/~} still loads, out of date"
            bad=y
        fi
    done
    if test -n "${bad}" ; then
        ecerr "  run agents-md-sync"
    fi
    ec "  = ${#contents} files, ${total}B, about $(( total / 4 )) tokens of agy's 20,000-token rules budget"
}

function h-agents-md-doctor-retired {
    : "reports retired outputs that are still on disk; see h-agents-md-retired"
    local f
    for f in "${(@f)$(h-agents-md-retired)}" ; do
        test -e "${f}" || test -L "${f}" || continue
        ecbold "retired: ${f/#${HOME}/~}"
        if h-agents-md-retired-ours-p "${f}" ; then
            ecerr "  STALE: an old output of this sync, still loaded; run agents-md-sync"
        else
            ecerr "  NOT OURS: left alone, but agy still loads it as a global rule"
        fi
    done
}

typeset -ga agents_md_settings=(
    "${HOME}/.claude/settings.json"$'\t'"${NIGHTDIR}/configFiles/claude-code/settings.json"
    #: [agfi:claude-work] shares the tracked settings with the personal
    #: profile; only the account, history and projects differ.
    "${HOME}/.claude-work/settings.json"$'\t'"${NIGHTDIR}/configFiles/claude-code/settings.json"
    #: Codex reads hooks from its own file rather than its settings; see
    #: [agfi:codex-session-tmux-autoname].
    "${HOME}/.codex/hooks.json"$'\t'"${NIGHTDIR}/configFiles/codex/hooks.json"
    #: Antigravity's shared hooks file (the TUI's /hooks writes here too); see
    #: [agfi:agy-session-tmux-autoname].
    "${HOME}/.gemini/config/hooks.json"$'\t'"${NIGHTDIR}/configFiles/antigravity/hooks.json"
    #: The per-seat theme. Same slug in both config dirs -- the shared
    #: settings.json asks for `custom:profile' -- pointing at a different
    #: tracked file, which is what makes a work session orange and a personal
    #: one blue. Written by [agfi:claude-themes-link]; listed here because
    #: Claude Code rewrites its own themes directory when you edit a theme in
    #: =/theme=, and a symlink replaced by a plain file would silently stop
    #: tracking.
    "${HOME}/.claude/themes/profile.json"$'\t'"${NIGHTDIR}/configFiles/claude-code/themes/personal.json"
    "${HOME}/.claude-work/themes/profile.json"$'\t'"${NIGHTDIR}/configFiles/claude-code/themes/work.json"
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
