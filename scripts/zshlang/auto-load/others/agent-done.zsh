##
#: Ending a session on purpose: the `/done' skill every agent shares, and the
#: exit path it calls.
#:
#: The skill itself is one tracked file, =configFiles/agent-skills/done/=,
#: symlinked into each agent's skills directory by [agfi:agent-skills-link].
#: Claude Code, Codex and Antigravity all read `<dir>/<name>/SKILL.md' with
#: `name'/`description' frontmatter and all offer it as `/<name>', so one file
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
    test -d "${dir}" && print -r -- "codex"$'\t'"${dir}/skills"

    #: Deliberately not [agfi:h-agy-session-home]: that is where agy keeps its
    #: conversations (~/.gemini/antigravity-cli). Its *configuration* -- hooks,
    #: MCP servers, skills -- sits one level up in ~/.gemini/config, which is
    #: also where the tracked hooks.json is linked.
    dir="${agy_config_dir:-${HOME}/.gemini/config}"
    test -d "${dir}" && print -r -- "agy"$'\t'"${dir}/skills"

    return 0
}

function h-agent-skills-names {
    #: The tracked skills: one directory each, named after the skill.
    setopt localoptions bareglobqual

    local d
    for d in "${agent_skills_src_dir}"/*/SKILL.md(N) ; do
        print -r -- "${${d:h}:t}"
    done
}

function agent-skills-link {
    : "installs every tracked agent skill into each agent's skills directory"
    #: Symlinks, not copies: the tracked file stays the only copy, so a fix
    #: reaches all three agents at once and [agfi:agents-md-doctor] can tell a
    #: link from a file somebody edited in place. Idempotent and quiet;
    #: `agent_skills_link_verbose_p=y' says what it did.
    #:
    #: Called from [agfi:h-agent-launch], so it is on the launch path of every
    #: agent: keep it to a stat per target.
    ##
    local verbose_p="${agent_skills_link_verbose_p:-n}"

    local line agent dir name src target
    local -a names
    names=( ${(f)"$(h-agent-skills-names)"} )
    (( ${#names} )) || return 0

    for line in ${(f)"$(h-agent-skills-dirs)"} ; do
        agent="${line%%$'\t'*}"
        dir="${line#*$'\t'}"

        for name in "${names[@]}" ; do
            src="${agent_skills_src_dir}/${name}/SKILL.md"
            target="${dir}/${name}/SKILL.md"

            #: Already pointing at the tracked file: the common case, and the
            #: reason this costs one stat rather than a write.
            if test -L "${target}" && [[ "${target:A}" == "${src:A}" ]] ; then
                continue
            fi

            #: A plain file here is somebody's own skill of the same name, or
            #: one an agent wrote itself. Replacing it silently would lose it,
            #: so say so and leave it.
            if test -e "${target}" && ! test -L "${target}" ; then
                ecerr "$0: ${target/#${HOME}/~} is a plain file, not our link; leaving it alone"
                continue
            fi

            mkdir -p -- "${target:h}" @RET
            command ln -sf -- "${src}" "${target}" @RET
            if bool "${verbose_p}" ; then
                ecgray "$0: linked ${target/#${HOME}/~} (${agent})"
            fi
        done
    done
}

function h-agent-skills-doctor {
    : "reports whether each agent's copy of each tracked skill is still linked"
    #: The same failure the settings doctor exists for: an agent that rewrites
    #: its own skills directory turns a symlink into a file, and the skill
    #: quietly stops being the tracked one.
    ##
    local line agent dir name src target
    local -a names
    names=( ${(f)"$(h-agent-skills-names)"} )

    if (( ! ${#names} )) ; then
        ecgray "skills: nothing tracked in ${agent_skills_src_dir/#${NIGHTDIR}/.}"
        return 0
    fi

    for line in ${(f)"$(h-agent-skills-dirs)"} ; do
        agent="${line%%$'\t'*}"
        dir="${line#*$'\t'}"

        for name in "${names[@]}" ; do
            src="${agent_skills_src_dir}/${name}/SKILL.md"
            target="${dir}/${name}/SKILL.md"

            ecbold "skill ${name}: ${target/#${HOME}/~} (${agent})"
            if ! test -e "${target}" && ! test -L "${target}" ; then
                ecerr "  MISSING: run agent-skills-link"
            elif ! test -L "${target}" ; then
                ecerr "  UNTRACKED: a plain file. Diff it against ${src/#${NIGHTDIR}/.}, keep what you want, and re-link."
            elif [[ "${target:A}" == "${src:A}" ]] ; then
                ec "  = symlinked to ${src/#${NIGHTDIR}/.}"
            else
                ecerr "  WRONG TARGET: points at ${${target:A}/#${HOME}/~}"
            fi
        done
    done
}
##
typeset -g agent_done_wait_s="${agent_done_wait_s:-20}"

function h-agent-done-dir {
    #: Where the summaries live. Next to the session registry rather than
    #: under ~/tmp, for the reason [agfi:h-agent-session-registry-dir] gives:
    #: that directory gets swept, and a report of a finished session is worth
    #: more the day after than the minute after.
    ##
    ec "$(h-agent-session-registry-dir)/done"
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
    #: Usage: h-agent-done-report <agent> <id> <name> <transcript> <cwd> <report-path>
    ##
    local agent="${1}" id="${2}" name="${3}" transcript="${4}" cwd="${5}" out="${6}"
    assert-args agent out @RET

    local glyph label
    glyph="$(h-agent-field "${agent}" glyph 2>/dev/null)" || glyph=''
    label="$(h-agent-field "${agent}" label 2>/dev/null)" || label="${agent}"

    {
        print -r -- "${glyph} ${label} session finished${name:+: ${name}}"
        print -r --
        test -n "${id}" && print -r -- "  id:         ${id}"
        test -n "${transcript}" && print -r -- "  transcript: ${transcript/#${HOME}/~}"
        test -n "${cwd}" && print -r -- "  cwd:        ${cwd/#${HOME}/~}"
        print -r -- "  ended:      $(date '+%Y-%m-%d %H:%M:%S')"
        print -r -- "  report:     ${out/#${HOME}/~}"
        print -r --
        if test -n "${agent_done_summary}" ; then
            print -r -- "${agent_done_summary}"
        else
            print -r -- "(no summary was given)"
        fi
    } > "${out}" @RET
}

function h-agent-done-watch {
    #: Waits for the agent to go, then puts the report on screen. Runs outside
    #: the agent's process tree -- under the tmux server, or disowned -- because
    #: an agent commonly takes its shell tool's children with it when it exits,
    #: and this must outlive exactly that.
    #: Usage: h-agent-done-watch <pid> <pane> <report> <tty>
    ##
    local pid="${1}" pane="${2}" report="${3}" tty="${4}"
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
        command tmux respawn-pane -k -t "${pane}" \
            "printf '\033[H\033[2J\n' ; command cat -- ${(q)report}" 2>/dev/null && return 0
    fi

    #: No pane, or tmux would not have it: write to the terminal the session
    #: was using. Now that the TUI is gone this lands on the normal screen,
    #: where it stays in the scrollback.
    if test -n "${tty}" && test -c "${tty}" ; then
        { command cat -- "${report}" > "${tty}" } 2>/dev/null && return 0
    fi

    return 0
}

function agent-done {
    : "ends this agent session, leaving an executive summary on screen

Reads the summary from stdin, from --summary-file, or from its arguments, and
saves it under \$(h-agent-done-dir) with the session's name, id and transcript.
Then it asks the agent to exit and shows the report: inside tmux by replacing
this pane with it and letting the pane die with the text still on it, outside
tmux by printing it to the terminal once the agent is gone.

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

    local pane="${TMUX_PANE}" tty="${agent_done_tty}" cwd="${agent_done_cwd}"
    if test -n "${pane}" ; then
        #: The pane's own tty and directory, not this shell's. The shell an
        #: agent runs its tools in usually has no controlling terminal, and it
        #: is commonly not even in the project directory -- this function is
        #: reached through `zsh -ic', which starts wherever the rc files leave
        #: it. The pane is where the session actually lives.
        test -n "${tty}" || tty="$(command tmux display-message -p -t "${pane}" '#{pane_tty}' 2>/dev/null)" || tty=''
        test -n "${cwd}" || cwd="$(command tmux display-message -p -t "${pane}" '#{pane_current_path}' 2>/dev/null)" || cwd=''
    fi
    test -n "${cwd}" || cwd="${PWD}"

    #: Outside tmux there is no pane to ask, and `tty' is no help either: the
    #: shell an agent runs its tools in has no controlling terminal, so it
    #: cannot name the terminal the *session* is attached to. The agent process
    #: can -- it is the one holding it.
    if test -z "${tty}" && test -n "${pid}" ; then
        local pts
        pts="$(command ps -o tty= -p "${pid}" 2>/dev/null)"
        pts="${pts//[[:space:]]/}"
        if test -n "${pts}" && [[ "${pts}" != '??' ]] ; then
            tty="/dev/${pts}"
        fi
    fi
    test -n "${tty}" || tty="$(command tty 2>/dev/null)" || tty=''

    local dir report
    dir="$(h-agent-done-dir)" @TRET
    mkdir -p -- "${dir}" @RET
    report="${dir}/$(date '+%Y-%m-%dT%H%M%S')-${agent}${id:+-${id}}.txt"

    h-agent-done-report "${agent}" "${id}" "${name}" "${transcript}" "${cwd}" "${report}" @RET

    if bool "${dry_p}" ; then
        ec "would end: ${agent}${name:+ (${name})}${id:+ ${id}}"
        ec "would kill: pid=${pid:-<unknown>} pane=${pane:-<none>} tty=${tty:-<none>}"
        ec "report: ${report}"
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
        command tmux set-option -p -t "${pane}" remain-on-exit on 2>/dev/null || true
        #: Under the tmux server, so it survives this pane by construction.
        #: An absolute zsh, because `run-shell' inherits the server's
        #: environment, which is whatever the first client happened to have.
        #: `-c', not `-ic': ~/.zshenv loads zshlang for non-interactive shells
        #: too, so the function is there without paying for an interactive
        #: startup.
        local watch_cmd
        watch_cmd="h-agent-done-watch ${(q)pid} ${(q)pane} ${(q)report} ${(q)tty}"
        command tmux run-shell -b \
            "${commands[zsh]:-zsh} -c ${(qq)watch_cmd}" 2>/dev/null || true
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
