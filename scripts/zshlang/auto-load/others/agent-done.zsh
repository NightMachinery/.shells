##
#: Ending a session on purpose: the `/done' skill every agent shares, and the
#: exit path it calls.
#:
#: The skill itself is one tracked directory, =configFiles/agent-skills/done/=,
#: symlinked into each agent's skills directory by [agfi:agent-skills-link].
#: Claude Code, Codex and Antigravity all read `<dir>/<name>/SKILL.md' with
#: `name'/`description' frontmatter (Codex uses `$done'), so one file
#: serves the three of them; three near-copies would drift, and the prose in it
#: is the part worth getting right once.
#:
#: [agfi:agent-done] is the other half: the agent decides *whether* the work is
#: finished -- only it knows -- and this decides *how* a finished session ends.
#: The summary has to outlive the agent, and that is the whole difficulty. A
#: TUI draws on the alternate screen, so everything it printed is gone the
#: moment it exits, and a summary written into the transcript is exactly what
#: nobody will go and read. So the text is written to a file, and then put back
#: on the screen the session was using, after the session is gone.
##
typeset -g agent_skills_src_dir="${agent_skills_src_dir:-${NIGHTDIR}/configFiles/agent-skills}"

#: Skill roots outside the two standard checkouts, in the same
#: `<root>/<name>/SKILL.md' shape as those. For skills that have to travel with
#: something else -- a private config repository, a standalone tool -- and so
#: cannot live in the public tree.
#:
#: A root that does not exist contributes nothing, which is what lets one
#: default serve every machine: the entry below is only real on a host that has
#: cloned `night-gcp', and everywhere else it is silently skipped.
#:
#: Without this, a standalone repository has to be wired in by hand, which is
#: what [agfi:agent-subagents-start] still does for `tmux-subagents'.
#:
#: `${+name}' rather than `${name:-default}': an empty array is
#: indistinguishable from an unset one under `:-', so the default would come
#: back and there would be no way to turn this off. Setting the array to `()'
#: before this file loads disables it, which is what the tests do.
if (( ! ${+agent_skills_extra_roots} )) ; then
    typeset -ga agent_skills_extra_roots
    agent_skills_extra_roots=( "${HOME}/.night-gcp/skills" )
fi

#: Directories holding standalone skill REPOSITORIES, as opposed to skills.
#: That one level of nesting is the whole reason `~/code/skills' was not simply
#: added to the list above: its members are repositories -- `tmux-subagents',
#: `research-agent-ops' -- each of which keeps its skills in its own `skills/'
#: subdirectory, so `<root>/<name>/SKILL.md' does not match them. Every such
#: repository here follows that layout, so one glob covers the convention
#: rather than each repository being wired in by hand.
#:
#: Expanded inside [agfi:h-agent-skills-sources] and not here: the glob needs
#: `bareglobqual', which that function sets and which an agent shell does not
#: necessarily have on.
if (( ! ${+agent_skills_repo_roots} )) ; then
    typeset -ga agent_skills_repo_roots
    agent_skills_repo_roots=( "${HOME}/code/skills" )
fi

function h-agent-skills-codex-dir {
    #: User skills are shared across Codex seats, independent of CODEX_HOME.
    print -r -- "${agent_skills_codex_dir:-${HOME}/.agents/skills}"
}

function h-agent-skills-dirs {
    #: Where each agent installed on this host looks for user skills, as
    #: `<agent>\t<dir>' lines. A host without an agent contributes nothing,
    #: which is how one table serves every machine.
    #:
    #: Claude Code contributes one line per seat: its skills live under the
    #: config home, so a skill installed in ~/.claude is invisible to a work
    #: session, whose home is elsewhere ([agfi:claude-code-profile-current]).
    ##
    local p dir

    for p in "${claude_code_profile_order[@]}" ; do
        dir="${claude_code_profiles[$p]:-${HOME}/.claude}"
        test -d "${dir}" || continue
        print -r -- "claude"$'\t'"${dir}/skills"
    done

    dir="$(h-codex-session-home)"
    if test -d "${dir}" ; then
        print -r -- "codex"$'\t'"$(h-agent-skills-codex-dir)"
    fi

    #: Deliberately not [agfi:h-agy-session-home]: that is where agy keeps its
    #: conversations (~/.gemini/antigravity-cli). Its *configuration* -- hooks,
    #: MCP servers, skills -- sits one level up in ~/.gemini/config, which is
    #: also where the tracked hooks.json is linked.
    dir="${agy_config_dir:-${HOME}/.gemini/config}"
    test -d "${dir}" && print -r -- "agy"$'\t'"${dir}/skills"

    return 0
}

function h-agent-skills-sources {
    #: Emit full SKILL.md paths from every configured root. Validate all names
    #: before emitting anything so callers cannot partially link an ambiguous
    #: set. See `$agent_skills_extra_roots' for roots beyond the two checkouts.
    setopt localoptions bareglobqual
    local notes_dir="${agent_skills_notes_dir-${HOME}/notes/skills}"
    local root skill name repo_root
    local -a roots=("${agent_skills_src_dir}" "${notes_dir}" "${agent_skills_extra_roots[@]}") sources=()
    for repo_root in "${agent_skills_repo_roots[@]}" ; do
        test -n "${repo_root}" || continue
        roots+=( "${repo_root}"/*/skills(N/) )
    done
    local -A seen=()
    for root in "${roots[@]}" ; do
        test -n "${root}" || continue
        for skill in "${root}"/*/SKILL.md(N) ; do
            name="${${skill:h}:t}"
            if (( ${+seen[$name]} )) ; then
                [[ "${seen[$name]}" == "${skill}" ]] && continue
                ecerr "$0: duplicate skill ${name}: ${seen[$name]} and ${skill}"
                return 1
            fi
            seen[$name]="${skill}"
            sources+=("${skill}")
        done
    done
    (( ${#sources} )) && print -rl -- "${sources[@]}"
    return 0
}

function h-agent-skills-names {
    local sources skill
    sources="$(h-agent-skills-sources)" || return $?
    for skill in "${(@f)sources}" ; do
        test -n "${skill}" && print -r -- "${${skill:h}:t}"
    done
    return 0
}

function h-agent-skills-dangling-p {
    : "usage: h-agent-skills-dangling-p <path>; whether <path> is a symlink to nothing"
    #: Always safe to replace: there is nothing behind it to lose. This is what
    #: a skill leaves behind when it moves out of one of our roots.
    test -L "${1}" && ! test -e "${1}"
}

function h-agent-skills-file-link-dir-p {
    : "usage: h-agent-skills-file-link-dir-p <target dir> <skill source dir>; whether <target dir> is a real directory holding nothing but a SKILL.md link to that source's SKILL.md, or to nothing"
    #: The file-link layout: a directory of our own making around one
    #: SKILL.md link. It carries none of the skill's sibling files, and
    #: nothing else lives in it, so swapping it for a directory link loses
    #: nothing. Hidden entries count, so a directory somebody has put anything
    #: into is never taken for one.
    setopt localoptions bareglobqual
    local target="${1}" src="${2}"
    local skill="${target}/SKILL.md"
    local -a entries
    test -d "${target}" && ! test -L "${target}" || return 1
    entries=( "${target}"/*(DN) )
    (( ${#entries} == 1 )) && [[ "${entries[1]}" == "${skill}" ]] && test -L "${skill}" || return 1
    [[ "${skill:A}" == "${src:A}/SKILL.md" ]] || h-agent-skills-dangling-p "${skill}"
}

function agent-skills-link {
    : "installs every tracked agent skill into each agent's skills directory"
    #: One directory symlink per skill, `<dir>/<name>' -> the source's skill
    #: directory, for every agent. All three follow such links and read the
    #: skill's sibling files (references/, scripts/, assets/) through them,
    #: and Claude Code reports the link path as the skill's base directory.
    #: Symlinks, not copies: the tracked tree stays the only copy, so a fix
    #: reaches every agent at once and [agfi:h-agent-skills-doctor] can tell a
    #: link from a directory somebody edited in place. Idempotent and quiet;
    #: `agent_skills_link_verbose_p=y' says what it did.
    #:
    #: Called from [agfi:h-agent-launch], so it is on the launch path of every
    #: agent: keep it to a stat per target.
    ##
    local verbose_p="${agent_skills_link_verbose_p:-n}"

    local line agent dir name src target source_list ret=0
    source_list="$(h-agent-skills-sources)" || return $?
    test -n "${source_list}" || return 0

    for line in ${(f)"$(h-agent-skills-dirs)"} ; do
        agent="${line%%$'\t'*}"
        dir="${line#*$'\t'}"

        for src in "${(@f)source_list}" ; do
            src="${src:h}"
            name="${src:t}"
            target="${dir}/${name}"

            #: Already pointing at the tracked directory: the common case, and
            #: the reason this costs one stat rather than a write.
            if test -L "${target}" && [[ "${target:A}" == "${src:A}" ]] ; then
                continue
            fi

            if h-agent-skills-dangling-p "${target}" ; then
                #: What a skill that moved between roots leaves behind.
                ecgray "$0: replacing the dangling link ${target/#${HOME}/~}"
                command rm -- "${target}" @RET
            elif h-agent-skills-file-link-dir-p "${target}" "${src}" ; then
                ecgray "$0: replacing ${target/#${HOME}/~}, a directory holding only a SKILL.md link, with a link to ${${src:A}/#${HOME}/~}"
                command rm -- "${target}/SKILL.md" @RET
                #: rmdir, never rm -r: it refuses if anything appeared in the
                #: meantime.
                command rmdir -- "${target}" @RET
            elif test -e "${target}" || test -L "${target}" ; then
                #: Somebody's own skill of the same name -- a directory an
                #: agent wrote itself, a link of their own -- or one of ours
                #: left behind by a skill that moved between roots. Replacing
                #: either silently would lose it, so name what we expected and
                #: leave it: only you can tell those two apart.
                ecerr "$0: ${target/#${HOME}/~} is not our directory link (expected ${${src:A}/#${HOME}/~}); leaving it alone. Remove it and re-run $0."
                ret=1
                continue
            fi

            command mkdir -p -- "${dir}" @RET
            #: `-s' rather than `-sf': the guards above leave nothing to
            #: clobber, so a collision here is a race, and belongs in an error.
            command ln -s -- "${src:A}" "${target}" @RET
            if bool "${verbose_p}" ; then
                ecgray "$0: linked ${target/#${HOME}/~} (${agent})"
            fi
        done
    done
    return ${ret}
}

function agent-skills-prune-legacy-codex {
    : "removes our obsolete CODEX_HOME/skills file links after verifying their replacements"
    #: Explicit migration only, never part of the agent launch path. No
    #: recursive deletion: .system, unknown skills and user files stay put.
    local legacy="$(h-codex-session-home)/skills"
    local current="$(h-agent-skills-codex-dir)"
    local name src old replacement skill_file source_list

    if [[ "${legacy:A}" == "${current:A}" ]] ; then
        ecerr "$0: legacy and current skill roots coincide; refusing cleanup"
        return 1
    fi

    source_list="$(h-agent-skills-sources)" || return $?
    test -n "${source_list}" || return 0
    for src in "${(@f)source_list}" ; do
        src="${src:h}"
        name="${src:t}"
        old="${legacy}/${name}"
        replacement="${current}/${name}"
        skill_file="${old}/SKILL.md"

        test -e "${old}" || test -L "${old}" || continue
        if ! test -L "${replacement}" || [[ "${replacement:A}" != "${src:A}" ]] ||
            ! test -r "${replacement}/SKILL.md" ; then
            ecerr "$0: no verified replacement for ${name}; keeping legacy entry"
            continue
        fi
        if test -L "${old}" || ! test -d "${old}" ||
            ! test -L "${skill_file}" || [[ "${skill_file:A}" != "${src:A}/SKILL.md" ]] ; then
            ecerr "$0: ${old/#${HOME}/~} is not our legacy file-link layout; leaving it alone"
            continue
        fi
        command unlink "${skill_file}" @RET
        #: rmdir cannot remove extra contents, including hidden files.
        command rmdir "${old}" 2>/dev/null || true
        ecgray "$0: removed obsolete link ${skill_file/#${HOME}/~}; tracked source preserved"
    done
    return 0
}

function h-agent-skills-doctor {
    : "reports whether each agent's copy of each tracked skill is still linked"
    #: The same failure the settings doctor exists for: an agent that rewrites
    #: its own skills directory turns a symlink into a file, and the skill
    #: quietly stops being the tracked one.
    ##
    local line agent dir name src target source_list
    source_list="$(h-agent-skills-sources)" || return $?

    if test -z "${source_list}" ; then
        ecgray "skills: no skills in configured source directories"
        return 0
    fi

    for line in ${(f)"$(h-agent-skills-dirs)"} ; do
        agent="${line%%$'\t'*}"
        dir="${line#*$'\t'}"

        for src in "${(@f)source_list}" ; do
            src="${src:h}"
            name="${src:t}"
            target="${dir}/${name}"

            ecbold "skill ${name}: ${target/#${HOME}/~} (${agent})"
            if h-agent-skills-dangling-p "${target}" ; then
                ecerr "  DANGLING: run agent-skills-link"
            elif ! test -e "${target}" && ! test -L "${target}" ; then
                ecerr "  MISSING: run agent-skills-link"
            elif h-agent-skills-file-link-dir-p "${target}" "${src}" ; then
                ecerr "  FILE LINK ONLY: sibling files are missing; run agent-skills-link"
            elif ! test -L "${target}" ; then
                ecerr "  UNTRACKED: not the expected symlink. Compare with ${src/#${NIGHTDIR}/.} before re-linking."
            elif [[ "${target:A}" == "${src:A}" ]] ; then
                ec "  = symlinked to ${src/#${NIGHTDIR}/.}"
            else
                ecerr "  WRONG TARGET: points at ${${target:A}/#${HOME}/~}. Remove it and re-run agent-skills-link."
            fi
        done
    done
}
##
typeset -g agent_done_wait_s="${agent_done_wait_s:-20}"
#: A second home for every report, beside the agents' shared state
#: (~/.agents/runs, ~/.agents/skills), where any agent or person looks first.
#: `-' rather than `:-', so that an empty value turns the copy off.
typeset -g agent_done_copy_dir="${agent_done_copy_dir-${HOME}/.agents/done}"

function h-agent-done-dir {
    #: Where the summaries live. Next to the session registry rather than
    #: under ~/tmp, for the reason [agfi:h-agent-session-registry-dir] gives:
    #: that directory gets swept, and a report of a finished session is worth
    #: more the day after than the minute after.
    ##
    ec "$(h-agent-session-registry-dir)/done"
}

function h-agent-done-copy-path {
    #: Where the copy of a report goes; fails when copying is turned off.
    #: Usage: h-agent-done-copy-path <report>
    ##
    local report="${1}"
    assert-args report @RET

    test -n "${agent_done_copy_dir-}" || return 1
    ec "${agent_done_copy_dir}/${report:t}"
}

function h-agent-done-shq {
    #: A path for a command line meant to be pasted: `~/rest' when the rest
    #: needs no quoting, which keeps the line short, and fully quoted
    #: otherwise.
    ##
    local p="${1}" rest="${1#${HOME}/}"
    if [[ "${p}" == "${HOME}/"* && "${(q-)rest}" == "${rest}" ]] ; then
        print -r -- "~/${rest}"
    else
        print -r -- "${(q-)p}"
    fi
}

function h-agent-done-resume-native {
    #: The agent's own resume command, to paste into any shell: the bare
    #: binary, plus the environment that selects the seat this session ran
    #: on. That environment is this shell's, inherited from the agent, so it
    #: is the session's own answer. Antigravity is the exception: its accounts
    #: are separate homes that only the per-account launcher sets up, so its
    #: line names that zsh launcher.
    #: Usage: h-agent-done-resume-native <agent> <id> <transcript>
    ##
    local agent="${1}" id="${2}" transcript="${3}"
    assert-args agent id @RET
    (( ${+functions[h-agent-session-resume-argv]} )) || return 1

    local launcher="${agent}" env=''
    case "${agent}" in
        claude)
            test -z "${CLAUDE_CONFIG_DIR:-}" ||
                env="CLAUDE_CONFIG_DIR=$(h-agent-done-shq "${CLAUDE_CONFIG_DIR}") "
            ;;
        codex)
            test -z "${CODEX_HOME:-}" ||
                env="CODEX_HOME=$(h-agent-done-shq "${CODEX_HOME}") "
            ;;
        agy)
            local account
            if test -n "${transcript}" && account="$(h-agy-account-of-transcript "${transcript}" 2>/dev/null)" ; then
                launcher="agy-${account}"
            fi
            ;;
    esac

    local -a reply
    h-agent-session-resume-argv "${agent}" "${id}" "${launcher}" 2>/dev/null @RET
    print -r -- "${env}${(j: :)${(q-)reply[@]}}"
}

function h-agent-done-resume-help {
    #: How to bring the session back, in every harness that can: Paseo when it
    #: ran there, the agent's own CLI, our zsh resume helper, and the dead
    #: pane. Whoever reads a report is usually deciding whether to carry on,
    #: often from another shell, so each line is a command to paste.
    #: Extra ids come from `agent_done_ids'; see [agfi:h-agent-done-report].
    #: Usage: h-agent-done-resume-help <agent> <id> <transcript> <cwd> [<paseo-uuid>]
    ##
    local agent="${1}" id="${2}" transcript="${3}" cwd="${4}" paseo="${5:-}"
    local -A ids=()
    (( ${+agent_done_ids} )) && ids=( "${(@kv)agent_done_ids}" )

    local -a lines
    if test -n "${paseo}" ; then
        #: `--home' only when it is not the default, to keep the lines short.
        local home='' home_arg=''
        (( ${+functions[h-paseo-home]} )) && home="$(h-paseo-home 2>/dev/null)"
        if test -n "${home}" && [[ "${home:A}" != "${HOME}/.paseo" ]] ; then
            home_arg="--home $(h-agent-done-shq "${home}") "
        fi
        #: Both unarchive it, keeping its uuid and history: every surface
        #: that acts on an archived agent unarchives it first
        #: (`unarchiveAgentState' in Paseo's server), and an import of a
        #: session whose agent is archived revives that agent rather than
        #: making a new one, provided the directory matches.
        lines+=( "paseo:  paseo send ${home_arg}${paseo} 'your next message'"
            "          unarchives it, then runs that message as a turn" )
        local provider="${ids[paseo_provider]-}" pcwd="${ids[paseo_cwd]:-${cwd}}"
        if test -n "${provider}" && test -n "${id}" && test -n "${pcwd}" ; then
            lines+=( "        paseo import ${home_arg}--provider ${(q-)provider} --cwd $(h-agent-done-shq "${pcwd}") ${id}"
                "          unarchives it without starting a turn" )
        fi
        if [[ "${ids[paseo_keep]-}" == true ]] ; then
            lines+=( "        paseo-keep ${paseo}"
                "          re-pins it; /done lifts the pin to archive it" )
        fi
    fi

    local native
    if test -n "${id}" && native="$(h-agent-done-resume-native "${agent}" "${id}" "${transcript}")" ; then
        lines+=( "$(command printf '%-8s' "${agent}:")${cwd:+cd $(h-agent-done-shq "${cwd}") && }${native}" )
    fi

    if test -n "${transcript}" ; then
        lines+=( "zsh:    agent-session-resume $(h-agent-done-shq "${transcript}")" )
    fi

    if test -n "${ids[pane]-}" && test -z "${paseo}" ; then
        lines+=( "tmux:   prefix-r in the dead pane ${ids[pane]}" )
    fi

    if (( ! ${#lines} )) ; then
        print -r -- "  resume: nothing was recorded to resume from"
        return 0
    fi
    print -r -- "  resume, with one of these (two at once would both write the same transcript):"
    print -rl -- "${(@)lines/#/    }"
}

function h-agent-done-id {
    #: This session's own id. Each adapter already knows how to read it out of
    #: the environment its agent exports; see [agfi:h-agent-session-call].
    local agent="${1}"
    assert-args agent @RET

    h-agent-session-call "${agent}" current-id
}

function h-agent-done-pid {
    #: The agent's own process, so it can be asked to exit rather than having
    #: the floor pulled out from under it. Claude Code exports its pid;
    #: otherwise walk up from this shell to the first ancestor whose name is
    #: one of the agent's binaries ([agfi:h-agent-field]) -- the agent is
    #: always an ancestor, because this runs in a shell it spawned.
    #:
    #: `agent_done_pid' overrides, which is how this is testable without an
    #: agent to kill.
    ##
    local agent="${1}"
    assert-args agent @RET

    if test -n "${agent_done_pid}" ; then
        ec "${agent_done_pid}"
        return 0
    fi

    if [[ "${agent}" == claude ]] && test -n "${CLAUDE_PID}" ; then
        ec "${CLAUDE_PID}"
        return 0
    fi

    local -a binaries
    binaries=( ${=$(h-agent-field "${agent}" binaries)} ) @TRET

    local pid="${PPID}" comm
    local -i guard=0
    while (( pid > 1 && guard < 32 )) ; do
        comm="$(command ps -o comm= -p "${pid}" 2>/dev/null)"
        comm="${comm:t}"
        if (( ${binaries[(Ie)${comm}]} )) ; then
            ec "${pid}"
            return 0
        fi
        pid="$(ps-parent-pid "${pid}" 2>/dev/null)" || return 1
        (( guard++ ))
    done

    return 1
}

function h-agent-done-report {
    #: The text the pane is left showing: a header this knows how to fill in,
    #: then the agent's own summary. Written as one file so the thing that
    #: displays it can be a plain `cat' -- the process doing the displaying
    #: has to be able to start in a pane whose shell is already dead.
    #:
    #: The header carries every id that finds the session again, and the
    #: commands that resume it ([agfi:h-agent-done-resume-help]). The ids only
    #: [agfi:agent-done] can see come in through the associative array
    #: `agent_done_ids', as the summary does through `agent_done_summary':
    #: `paseo_provider', `paseo_cwd', `paseo_keep' and `paseo_subagents' for a Paseo agent,
    #: `paseo_parent' for a session started under one, `background' and `pane'.
    #:
    #: Then copied whole to [agfi:h-agent-done-copy-path]. A failed copy is
    #: reported and nothing more: the report itself is already saved.
    #: Usage: h-agent-done-report <agent> <id> <name> <transcript> <cwd> <seat> <report-path> [<paseo-uuid>]
    ##
    local agent="${1}" id="${2}" name="${3}" transcript="${4}" cwd="${5}" seat="${6}" out="${7}" paseo="${8:-}"
    assert-args agent out @RET

    local -A ids=()
    (( ${+agent_done_ids} )) && ids=( "${(@kv)agent_done_ids}" )

    local glyph label
    glyph="$(h-agent-field "${agent}" glyph 2>/dev/null)" || glyph=''
    label="$(h-agent-field "${agent}" label 2>/dev/null)" || label="${agent}"

    local copy=''
    copy="$(h-agent-done-copy-path "${out}")" || copy=''

    {
        print -r -- "${glyph} ${label} session finished${name:+: ${name}}"
        print -r --
        #: The seat belongs here rather than on the pane border: the border
        #: says which session is *live*, and this one is not any more, but
        #: which account did the work is worth recording.
        test -n "${seat}" && print -r -- "  seat:       ${seat}"
        print -r -- "  agent:      ${agent}"
        test -n "${id}" && print -r -- "  id:         ${id}"
        test -n "${paseo}" && print -r -- "  paseo:      ${paseo}${ids[paseo_provider]:+ (provider ${ids[paseo_provider]})}"
        #: Inherited, not this session's own: it ran under that Paseo agent.
        test -n "${ids[paseo_parent]-}" && print -r -- "  paseo parent: ${ids[paseo_parent]}"
        if test -n "${ids[paseo_subagents]-}" ; then
            #: Archived along with this one, so worth finding again.
            print -r -- "  paseo subagents:"
            print -rl -- "${(@)${(@f)ids[paseo_subagents]}/#/    }"
        fi
        test -n "${ids[background]-}" && print -r -- "  background: ${ids[background]}"
        test -n "${ids[pane]-}" && print -r -- "  pane:       ${ids[pane]}"
        test -n "${transcript}" && print -r -- "  transcript: ${transcript/#${HOME}/~}"
        test -n "${cwd}" && print -r -- "  cwd:        ${cwd/#${HOME}/~}"
        print -r -- "  ended:      $(date '+%Y-%m-%d %H:%M:%S')"
        print -r -- "  report:     ${out/#${HOME}/~}"
        test -n "${copy}" && print -r -- "  copy:       ${copy/#${HOME}/~}"
        print -r --
        h-agent-done-resume-help "${agent}" "${id}" "${transcript}" "${cwd}" "${paseo}"
        print -r --
        if test -n "${agent_done_summary}" ; then
            print -r -- "${agent_done_summary}"
        else
            print -r -- "(no summary was given)"
        fi
    } > "${out}" @RET

    if test -n "${copy}" ; then
        { command mkdir -p -- "${copy:h}" && command cp -- "${out}" "${copy}" } ||
            ecerr "$0: could not copy the report to ${copy}"
    fi
    return 0
}

function h-agent-done-pane-script {
    #: The command the dead pane is left holding, which has to do two different
    #: things because tmux gives it only one slot: `respawn-pane -k' -- bound to
    #: prefix-r here -- re-runs whatever command the pane last ran. So the first
    #: run shows the report and exits, leaving the pane dead with the summary on
    #: it, and any later run resumes the session instead. A state file next to
    #: the report is what tells the two apart.
    #:
    #: The shape of that resume line is a consumed contract, not only
    #: something the pane runs: [agfi:h-agent-session-tmux-dead-rows] reads the
    #: transcript back out of it to list this pane in [agfi:fftmux-agent-all].
    #:
    #: Resumed through [agfi:agent-session-resume], which resolves the agent
    #: from the transcript path and calls that agent's launcher -- so a work
    #: session comes back on the work seat, with its cues repainted, and this
    #: needs to know nothing about which agent it is ending.
    #: Usage: h-agent-done-pane-script <report> <transcript> <cwd> <out>
    ##
    local report="${1}" transcript="${2}" cwd="${3}" out="${4}"
    assert-args report out @RET

    local zsh_path="${commands[zsh]:-zsh}"
    #: `agent_done_resume_cmd' replaces the resume outright, which is both the
    #: escape hatch for resuming with extra flags and how the respawn branch is
    #: testable without starting a real session.
    local resume="${agent_done_resume_cmd}" managed_state=''
    if test -z "${resume}" && test -n "${transcript}" ; then
        resume="agent-session-resume ${(q)transcript}"
        if test -n "${TMUX_PANE}" ; then
            managed_state="$(command tmux show-option -pqv -t "${TMUX_PANE}" @agent_session_state 2>/dev/null)"
        fi
    fi

    {
        print -r -- '#!/bin/sh'
        print -r -- '#: Generated by agent-done. Deleting it costs nothing: the pane simply'
        print -r -- '#: stops being able to bring its session back.'
        print -r -- "shown=${(qq):-${report}.shown}"
        print -r -- 'if [ -e "$shown" ] ; then'
        if test -n "${resume}" ; then
            test -n "${cwd}" && print -r -- "    cd ${(qq)cwd} 2>/dev/null || true"
            #: Keep the resume line's consumed shape for the dead-pane picker.
            #: The managed runtime restores its original flags and hooks.
            test -n "${managed_state}" && print -r -- "    export AGENT_SESSION_REUSE_PANE=${(qq)managed_state}"
            print -r -- "    exec ${(qq)zsh_path} -c ${(qq)resume}"
        else
            #: No transcript, so nothing to resume; a shell is still better
            #: than showing the same report a second time.
            print -r -- "    exec ${(qq)zsh_path} -i"
        fi
        print -r -- 'fi'
        print -r -- ': > "$shown"'
        #: Clear and home, then the newline tmux swallows; see
        #: [agfi:h-agent-done-watch] for why.
        print -r -- "printf '\\033[H\\033[2J\\n'"
        print -r -- "exec cat -- ${(qq)report}"
    } > "${out}" @RET

    command chmod +x -- "${out}" 2>/dev/null || true
}

function h-agent-done-watch {
    #: Waits for the agent to go, then puts the report on screen. Runs outside
    #: the agent's process tree -- under the tmux server, or disowned -- because
    #: an agent commonly takes its shell tool's children with it when it exits,
    #: and this must outlive exactly that.
    #: Usage: h-agent-done-watch <pid> <pane> <report> <tty> [pane-script] [socket]
    ##
    local pid="${1}" pane="${2}" report="${3}" tty="${4}" script="${5}" socket="${6:-}"
    local -a tmux_args
    test -z "${socket}" || tmux_args=(-S "${socket}")
    assert-args report @RET

    #: Politeness with a deadline: the agent gets `agent_done_wait_s' to write
    #: out whatever it keeps in memory, and is then not allowed to hold the
    #: pane hostage.
    local -i waited=0 limit=$(( ${agent_done_wait_s} * 10 ))
    if test -n "${pid}" ; then
        while (( waited < limit )) && command kill -0 "${pid}" 2>/dev/null ; do
            sleep 0.1
            (( waited++ ))
        done
        if command kill -0 "${pid}" 2>/dev/null ; then
            command kill -KILL "${pid}" 2>/dev/null || true
            sleep 0.3
        fi
    fi

    if test -n "${pane}" ; then
        #: `respawn-pane -k' is what kills the pane: it clears it, runs this
        #: `cat' in it, and the pane dies again the instant `cat' returns.
        #: With `remain-on-exit' set beforehand the dead pane stays on screen
        #: showing the report, which is the point -- `kill-pane' would take the
        #: report away with the pane.
        #: Cleared and homed first (ESC[H ESC[2J rather than `clear', which
        #: would need a TERM the tmux server may not have): respawn reuses the
        #: pane's screen, so otherwise the report starts wherever the old
        #: shell's prompt left the cursor.
        #:
        #: The newline is not cosmetic. tmux swallows the first line a
        #: respawned pane prints -- measured: `printf ALPHA\nBETA\n' in a
        #: respawned pane shows only BETA -- so something has to be given up,
        #: and a blank line is the cheapest thing to lose.
        #: The script, when there is one, so that a later prefix-r brings the
        #: session back instead of re-printing the report; plain `cat' is the
        #: fallback and behaves as it always did.
        local display
        if test -n "${script}" && test -e "${script}" ; then
            display="sh ${(q)script}"
        else
            display="printf '\033[H\033[2J\n' ; command cat -- ${(q)report}"
        fi
        command tmux "${tmux_args[@]}" respawn-pane -k -t "${pane}" "${display}" 2>/dev/null && return 0
    fi

    #: No pane, or tmux would not have it: write to the terminal the session
    #: was using. Now that the TUI is gone this lands on the normal screen,
    #: where it stays in the scrollback.
    if test -n "${tty}" && test -c "${tty}" ; then
        { command cat -- "${report}" > "${tty}" } 2>/dev/null && return 0
    fi

    return 0
}

function h-agent-done-announce {
    #: For a session with no screen of its own to leave the report on -- a
    #: background Claude Code session, a Paseo agent -- a notification names
    #: the report instead.
    #: Usage: h-agent-done-announce <agent> <what> <name> <report>
    ##
    local agent="${1}" what="${2}" name="${3}" report="${4}"

    local glyph label
    glyph="$(h-agent-field "${agent}" glyph 2>/dev/null)" || glyph=''
    label="$(h-agent-field "${agent}" label 2>/dev/null)" || label="${agent}"
    notif_group='agent-done' notif "${glyph} ${label} ${what} finished${name:+: ${name}} -- report: ${report/#${HOME}/~}" >/dev/null 2>&1 || true
}

function h-agent-done-paseo-id {
    #: The Paseo agent this session *is*, by its uuid; fails when it is none.
    #: `PASEO_AGENT_ID' alone does not say so: everything a Paseo agent starts
    #: inherits it, a `claude' it opens in a tmux pane included, and ending the
    #: agent on that evidence would archive the parent. So the agent's own
    #: record ([agfi:paseo-ls]) must name this very session as the native one
    #: it runs.
    #: Usage: h-agent-done-paseo-id <session-id> <paseo-ls --json rows>
    ##
    local id="${1}" rows="${2}" uuid="${PASEO_AGENT_ID:-}"
    test -n "${id}" && test -n "${uuid}" && test -n "${rows}" || return 1

    print -r -- "${rows}" | command jq --exit-status --arg uuid "${uuid}" --arg id "${id}" \
        'any(.[]; .id == $uuid and .nativeSessionId == $id)' >/dev/null || return 1
    ec "${uuid}"
}

function h-agent-done-paseo-ids {
    #: What the report needs from this agent's Paseo record besides its uuid,
    #: into `agent_done_ids' (see [agfi:h-agent-done-report]): the provider
    #: and directory `paseo import' wants to unarchive it, whether it is pinned,
    #: and its direct subagents, which the archive takes down with it.
    #: Usage: h-agent-done-paseo-ids <uuid> <paseo-ls --json rows>
    ##
    local uuid="${1}" rows="${2}"
    assert-args uuid rows @RET

    local -a f
    f=( "${(@ps:\t:)$(print -r -- "${rows}" | command jq --raw-output --arg uuid "${uuid}" '
        map(select(.id == $uuid)) | first // {}
        | [(.provider // ""), (.cwd // ""), ((.keep // false) | tostring)] | @tsv')}" )
    agent_done_ids[paseo_provider]="${f[1]-}"
    agent_done_ids[paseo_cwd]="${f[2]-}"
    agent_done_ids[paseo_keep]="${f[3]-}"
    agent_done_ids[paseo_subagents]="$(print -r -- "${rows}" | command jq --raw-output --arg uuid "${uuid}" '
        .[] | select(.parentAgentId == $uuid)
        | "\(.id)  \(.status // "?")  \((.title // "")[0:60])"')"
}

function h-agent-done-paseo-blocker {
    #: Why archiving this Paseo agent would do more than end it, or nothing.
    #: An archive cascades to the agent's subagents, so one still running would
    #: be interrupted along with it, and a pinned one ([agfi:paseo-keep]) would
    #: lose the protection its pin promises. The agent's *own* pin is not a
    #: reason: the pin guards against bulk and cascading archives, and /done is
    #: the explicit end of this one agent, so [agfi:agent-done] lifts it.
    #: Reads the records afresh, since subagents may have started since.
    #: Usage: h-agent-done-paseo-blocker <uuid>
    ##
    local uuid="${1}"
    assert-args uuid @RET

    local rows
    rows="$(paseo-ls --json)" @RET
    #: `seen' guards the walk down the parent links against a cycle.
    print -r -- "${rows}" | command jq --raw-output --arg uuid "${uuid}" '
        . as $all
        | def below($ids; $seen):
            [ $all[] | select((.parentAgentId // "") | IN($ids[]))
                     | select(.id | IN($seen[]) | not) ] as $kids
            | if $kids == [] then []
              else $kids + below($kids | map(.id); $seen + ($kids | map(.id))) end;
        below([$uuid]; [$uuid]) as $kids
        | ($kids | map(select(.status == "running")) | length) as $busy
        | ($kids | map(select(.keep // false)) | length) as $pinned
        | if $busy > 0 then
            "\($busy) of its subagents \(if $busy == 1 then "is" else "are" end) still running, and archiving it would interrupt them"
          elif $pinned > 0 then
            "\($pinned) of its subagents \(if $pinned == 1 then "is" else "are" end) pinned (paseo-keep), and archiving it would archive them too"
          else empty end'
}

function h-agent-done-paseo-archive {
    #: Archives a finished Paseo agent: Paseo's own way of ending one, which
    #: interrupts its turn and takes it off the active list, keeping its
    #: history. Run detached by [agfi:agent-done], because the turn it
    #: interrupts is the one running agent-done, and that takes the calling
    #: shell with it. Its output has nowhere to go but a log beside the report,
    #: and a failure is announced, since nothing else would say so.
    #: Usage: h-agent-done-paseo-archive <uuid> <report>
    ##
    local uuid="${1}" report="${2}"
    assert-args uuid report @RET

    local log="${report}.archive.log" home
    if home="$(h-paseo-home 2>>"${log}")" &&
        command paseo archive --force --home "${home}" "${uuid}" >>"${log}" 2>&1 ; then
        return 0
    fi
    notif_group='agent-done' notif "agent-done: could not archive Paseo agent ${uuid[1,8]}; see ${log/#${HOME}/~}" >/dev/null 2>&1 || true
    return 1
}

function agent-done {
    : "ends this agent session, leaving an executive summary on screen

Reads the summary from stdin, from --summary-file, or from its arguments, and
saves it under \$(h-agent-done-dir) with the session's name, id and transcript.
Then it asks the agent to exit and shows the report: inside tmux by replacing
this pane with it and letting the pane die with the text still on it, outside
tmux by printing it to the terminal once the agent is gone. A Paseo agent is
archived instead, and a notification names the report.

Meant to be called by the shared /done skill, which is what decides that the
work is actually finished. --dry-run writes the report and kills nothing."
    ##
    local dry_p="${agent_done_dry_p:-n}" summary_file=''

    while (( $# )) ; do
        case "${1}" in
            --dry-run|-n) dry_p=y ; shift ;;
            --summary-file) summary_file="${2}" ; shift 2 ;;
            --) shift ; break ;;
            -*)
                ecerr "$0: unknown option: ${1}"
                return 1
                ;;
            *) break ;;
        esac
    done

    local agent="${agent_done_agent}"
    if test -z "${agent}" ; then
        agent="$(ai-agent-name)" || {
            ecerr "$0: not inside an agent session, so there is nothing to end"
            return 1
        }
    fi

    #: The summary, in the order that makes the skill's invocation shortest:
    #: a heredoc on stdin. Arguments and a file are for callers that already
    #: have it somewhere.
    local -x agent_done_summary=''
    if test -n "${summary_file}" ; then
        agent_done_summary="$(command cat -- "${summary_file}")" @RET
    elif (( $# )) ; then
        agent_done_summary="$*"
    elif ! test -t 0 ; then
        agent_done_summary="$(command cat)"
    fi

    local id='' transcript='' name=''
    id="$(h-agent-done-id "${agent}" 2>/dev/null)" || id=''
    if test -n "${id}" ; then
        transcript="$(h-agent-session-call "${agent}" resolve "${id}" 2>/dev/null | command head -n1)" || transcript=''
    fi
    if test -n "${transcript}" ; then
        name="$(h-agent-session-name-of-transcript "${transcript}" 2>/dev/null)" || name=''
        [[ "${name}" == '-' ]] && name=''
    fi

    local pid=''
    pid="$(h-agent-done-pid "${agent}" 2>/dev/null)" || pid=''

    #: A Paseo agent runs under the Paseo daemon, which owns no pane: the
    #: `TMUX' this shell inherited is wherever the daemon was started, so the
    #: pane checks below would only, rightly, refuse. Paseo ends one by
    #: archiving it, and its app keeps the conversation readable, so there is
    #: no screen to put back either. See "Paseo agents" in =docs/agent-done.md=.
    #: One read of Paseo's records serves both the check and the report's ids.
    local paseo_uuid='' paseo_rows=''
    if test -n "${PASEO_AGENT_ID:-}" && (( ${+functions[paseo-ls]} )) ; then
        paseo_rows="$(paseo-ls --json 2>/dev/null)" || paseo_rows=''
    fi
    paseo_uuid="$(h-agent-done-paseo-id "${id}" "${paseo_rows}")" || paseo_uuid=''

    #: The ids the report header needs beyond the session's own; see
    #: [agfi:h-agent-done-report].
    local -A agent_done_ids=()
    if test -n "${paseo_uuid}" ; then
        h-agent-done-paseo-ids "${paseo_uuid}" "${paseo_rows}" || true
    elif test -n "${PASEO_AGENT_ID:-}" ; then
        agent_done_ids[paseo_parent]="${PASEO_AGENT_ID}"
    fi

    #: A background Claude Code session (`claude --bg', or one backgrounded
    #: from the agent view) runs under the daemon's pty host: no pane, no tty,
    #: and a pid that belongs to the daemon's bookkeeping. It is ended with
    #: `claude stop', which keeps the conversation resumable, rather than by
    #: signal; and since there is no screen to leave the report on, the report
    #: is announced with a notification instead. See =docs/agent-sessions.md=,
    #: "Background sessions".
    local bg_home='' bg_short=''
    if [[ "${agent}" == claude ]] && test -n "${id}" && (( ${+functions[h-claude-code-bg-find]} )) ; then
        local bg_row
        if bg_row="$(h-claude-code-bg-find "${id}" 2>/dev/null)" && test -n "${bg_row}" ; then
            bg_home="${bg_row%%$'\t'*}"
            bg_short="${${bg_row#*$'\t'}%%$'\t'*}"
        fi
    fi

    local pane='' socket='' tty="${agent_done_tty}" cwd="${agent_done_cwd}"
    local -i pane_status=0
    if test -z "${bg_short}" && test -z "${paseo_uuid}" ; then
        pane="$(h-tmux-pane-of-pid "${pid}")" || pane_status=$?
        (( pane_status <= 1 )) || pane_status=2
    fi
    #: Also give managed-resume and cue helpers the recovered pane. This is
    #: scoped to this invocation, not a repair of the caller's environment.
    local -x TMUX_PANE="${pane}"
    if test -n "${pane}" ; then
        #: The pane's own tty and directory, not this shell's. The shell an
        #: agent runs its tools in usually has no controlling terminal, and it
        #: is commonly not even in the project directory -- this function is
        #: reached through `zsh -c', which starts wherever the rc files leave
        #: it. The pane is where the session actually lives.
        test -n "${tty}" || tty="$(command tmux display-message -p -t "${pane}" '#{pane_tty}' 2>/dev/null)" || tty=''
        test -n "${cwd}" || cwd="$(command tmux display-message -p -t "${pane}" '#{pane_current_path}' 2>/dev/null)" || cwd=''
        socket="$(command tmux display-message -p -t "${pane}" '#{socket_path}' 2>/dev/null)" || socket=''
        if [[ -z "${tty}" || -z "${socket}" ]] ; then
            ecerr "$0: cannot read the owning pane's tty/socket"
            pane_status=2
        fi
    fi
    test -n "${cwd}" || cwd="${PWD}"
    #: The session's own directory in preference to the pane's: they are the
    #: same until somebody starts a session somewhere and works elsewhere, and
    #: this is the one a resume has to return to.
    cwd="$(h-agent-session-dir "${transcript}" "${agent}" "${cwd}")"

    #: Outside tmux there is no pane to ask, and `tty' is no help either: the
    #: shell an agent runs its tools in has no controlling terminal, so it
    #: cannot name the terminal the *session* is attached to. The agent process
    #: can -- it is the one holding it.
    if (( pane_status != 2 )) && test -z "${tty}" && test -n "${pid}" ; then
        local pts
        pts="$(command ps -o tty= -p "${pid}" 2>/dev/null)"
        pts="${pts//[[:space:]]/}"
        if test -n "${pts}" && [[ "${pts}" != '??' ]] ; then
            tty="/dev/${pts}"
        fi
    fi
    test -n "${tty}" || tty="$(command tty 2>/dev/null)" || tty=''

    #: Which seat did the work. Read from the effective config dir, which this
    #: shell inherited from the agent, so it is the session's own answer and
    #: not the launcher's name ([agfi:claude-code-profile-current]).
    local seat=''
    if [[ "${agent}" == claude ]] && (( ${+functions[claude-code-profile-current]} )) ; then
        local profile
        profile="$(claude-code-profile-current 2>/dev/null)" || profile=''
        if test -n "${profile}" ; then
            seat="${claude_code_profile_markers[$profile]:+${claude_code_profile_markers[$profile]} }${claude_code_profile_labels[$profile]:-${(U)profile}}"
        fi
    fi

    test -z "${bg_short}" || agent_done_ids[background]="${bg_short}"
    test -z "${pane}" || agent_done_ids[pane]="${pane}"

    local dir report script
    dir="$(h-agent-done-dir)" @TRET
    mkdir -p -- "${dir}" @RET
    report="${dir}/$(date '+%Y-%m-%dT%H%M%S')-${agent}${id:+-${id}}.txt"
    script="${report}.pane.sh"

    h-agent-done-report "${agent}" "${id}" "${name}" "${transcript}" "${cwd}" "${seat}" "${report}" "${paseo_uuid}" @RET
    #: A Paseo agent leaves no pane behind to respawn; Paseo resumes it.
    if test -z "${paseo_uuid}" ; then
        h-agent-done-pane-script "${report}" "${transcript}" "${cwd}" "${script}" @RET
    fi

    if (( pane_status == 2 )) ; then
        ecerr "$0: cannot safely determine pane ownership; nothing ended; report: ${report}"
        return 2
    fi

    if test -n "${paseo_uuid}" ; then
        local paseo_block
        paseo_block="$(h-agent-done-paseo-blocker "${paseo_uuid}")" ||
            paseo_block="its Paseo records could not be read"
        if test -n "${paseo_block}" ; then
            ecerr "$0: not archiving Paseo agent ${paseo_uuid}: ${paseo_block}; nothing ended; report: ${report}"
            return 2
        fi
    fi

    if bool "${dry_p}" ; then
        ec "would end: ${agent}${name:+ (${name})}${id:+ ${id}}"
        if test -n "${bg_short}" ; then
            ec "would stop the background session ${bg_short} with \`claude stop' and notify; report: ${report}"
        elif test -n "${paseo_uuid}" ; then
            [[ "${agent_done_ids[paseo_keep]-}" == true ]] &&
                ec "would unpin the Paseo agent ${paseo_uuid} with paseo-unkeep"
            ec "would archive the Paseo agent ${paseo_uuid} with \`paseo archive --force' and notify; report: ${report}"
        else
            ec "would kill: pid=${pid:-<unknown>} pane=${pane:-<none>} tty=${tty:-<none>} socket=${socket:-<none>}"
        fi
        ec "would forget this session's /auto-continue registration, if it has one"
        ec "report: ${report}"
        local copy
        copy="$(h-agent-done-copy-path "${report}")" && ec "copy: ${copy}"
        test -n "${paseo_uuid}" || ec "pane script (prefix-r resumes): ${script}"
        return 0
    fi

    #: Resolving transcripts/writing a report takes time. Recheck ownership
    #: before unregistering or scheduling any destructive action.
    if test -z "${bg_short}" && test -z "${paseo_uuid}" ; then
        local current_pane='' current_status=0
        current_pane="$(h-tmux-pane-of-pid "${pid}")" || current_status=$?
        if (( current_status != pane_status )) || [[ "${current_pane}" != "${pane}" ]] ; then
            ecerr "$0: pane ownership changed; nothing ended; report: ${report}"
            return 2
        fi
    fi

    #: A pin ([agfi:paseo-keep]) is lifted here, before anything else changes,
    #: and not in the detached archive: a pin that will not come off is then a
    #: refusal like the blockers above, with nothing ended yet. The report
    #: says how to put it back.
    if test -n "${paseo_uuid}" && [[ "${agent_done_ids[paseo_keep]-}" == true ]] ; then
        if ! (( ${+functions[paseo-unkeep]} )) || ! paseo-unkeep "${paseo_uuid}" >/dev/null 2>&1 ; then
            ecerr "$0: Paseo agent ${paseo_uuid} is pinned and could not be unpinned; nothing ended; report: ${report}"
            return 2
        fi
        ecgray "$0: unpinned Paseo agent ${paseo_uuid}"
    fi

    #: A session ending on purpose is finished, and must not be typed into at
    #: the next reset because it once asked to be ([agfi:agent-auto-continue-off]).
    #: Guarded, since that file is independent of this one; and quiet when
    #: there was nothing to forget.
    #: It dispatches to Paseo's registry on `PASEO_AGENT_ID', so that is the
    #: verified agent or nothing: an inherited one is a parent's.
    if (( ${+functions[agent-auto-continue-off]} )) ; then
        local forgot
        forgot="$(PASEO_AGENT_ID="${paseo_uuid}" agent-auto-continue-off 2>/dev/null)" || forgot=''
        if [[ "${forgot}" == 'auto-continue off'* ]] ; then
            ecgray "$0: ${forgot}"
        fi
    fi

    if test -n "${bg_short}" ; then
        #: Announced first, then stopped: the stop takes this shell with it.
        h-agent-done-announce "${agent}" 'background session' "${name}" "${report}"
        ec "ending background ${agent}${name:+ (${name})} ${bg_short}; report: ${report}"
        h-claude-code-bg-home-run "${bg_home}" stop "${bg_short}" >/dev/null 2>&1 || true
        return 0
    fi

    if test -n "${paseo_uuid}" ; then
        h-agent-done-announce "${agent}" 'Paseo agent' "${name}" "${report}"
        ec "archiving Paseo agent ${paseo_uuid}${name:+ (${name})}; report: ${report}"
        #: Detached, like the watcher below: the archive interrupts the turn
        #: that is running this, and this shell goes with it.
        awaysh-sure h-agent-done-paseo-archive "${paseo_uuid}" "${report}"
        return 0
    fi

    if test -z "${pid}" && test -z "${pane}" ; then
        #: Nothing to kill and nowhere to show it: better to say so than to
        #: leave the caller thinking the session ended.
        ecerr "$0: cannot find the ${agent} process, and we are not in tmux; the report is at ${report}"
        return 1
    fi

    #: The cues the launcher painted on this pane die with it, except the ones
    #: it set on the *window*: a border row outlives the pane and would leave a
    #: dead pane still labelled WORK. Undone here rather than in the launcher's
    #: `always' block, which a killed pane never reaches.
    if [[ "${agent}" == claude ]] && (( ${+functions[h-claude-tmux-cues-teardown]} )) ; then
        h-claude-tmux-cues-teardown || true
    fi

    if test -n "${pane}" ; then
        #: Set before anything is killed: tmux only keeps a dead pane on
        #: screen if the option was already on when it died.
        command tmux -S "${socket}" set-option -p -t "${pane}" remain-on-exit on 2>/dev/null @RET
        #: Under the tmux server, so it survives this pane by construction.
        #: An absolute zsh, because `run-shell' inherits the server's
        #: environment, which is whatever the first client happened to have.
        #: `-c', not `-ic': ~/.zshenv loads zshlang for non-interactive shells
        #: too, so the function is there without paying for an interactive
        #: startup.
        local watch_cmd
        #: Carry the socket explicitly: the detached watcher's environment
        #: need not select this server, and pane IDs are only server-local.
        watch_cmd="h-agent-done-watch ${(q)pid} ${(q)pane} ${(q)report} ${(q)tty} ${(q)script} ${(q)socket}"
        command tmux -S "${socket}" run-shell -b \
            "${commands[zsh]:-zsh} -c ${(qq)watch_cmd}" 2>/dev/null @RET
    else
        #: No tmux server to hide behind, so the watcher has to become a
        #: session leader in its own right ([agfi:awaysh-sure], which is
        #: `setsid zsh -c'). It has two deaths to survive, and a plain
        #: background job survives neither reliably: this shell is the right
        #: half of the skill's pipeline, so it exits at once, and the agent
        #: commonly takes its whole tool-shell tree with it when it goes.
        awaysh-sure h-agent-done-watch "${pid}" '' "${report}" "${tty}"
    fi

    if test -n "${pid}" ; then
        #: SIGTERM, not SIGKILL: every one of the three writes state on the way
        #: out, and the watcher escalates if it does not go.
        command kill -TERM "${pid}" 2>/dev/null || true
    fi

    #: Only reached while the agent is still dying, and its answer goes
    #: nowhere anyway. Printed for a human running this by hand.
    ec "ending ${agent}${name:+ (${name})}; report: ${report}"
}
aliasfn agent-done-dry agent-done --dry-run

function agent-done-reports {
    : "the saved /done reports, newest first"
    setopt localoptions bareglobqual

    local dir
    dir="$(h-agent-done-dir)" @TRET

    local -a reports
    reports=( "${dir}"/*.txt(NOm) )
    if (( ! ${#reports} )) ; then
        ecgray "$0: none yet in ${dir/#${HOME}/~}"
        return 0
    fi

    print -rl -- "${reports[@]}"
}

function agent-done-report-last {
    : "prints the most recent /done report"
    local last
    last="$(agent-done-reports | command head -n1)" @TRET
    test -n "${last}" || return 1

    command cat -- "${last}"
}
