##
#: Cleaning up the tmux subagents the `tmux-subagents' skill launches.
#:
#: The skill (=~/code/skills/tmux-subagents=) starts child agents in detached
#: tmux sessions named `ag--<project>--<task>--<provider-model>--<suffix>' --
#: five fields, the last a random hex triple -- and registers each in a JSON
#: file keyed by node id, which is also the tmux session name. The run and the
#: lineage are fields of that entry and not of the name, which is why closing a
#: subtree reads the registry rather than parsing session names. Its `process_state' and `task_outcome' fields are written once
#: at launch and never updated -- two finished Codex children still read
#: `running' on 2026-09-09 -- so nothing here trusts them.
#:
#: Instead every state is *derived on each read*, from the five places that
#: cannot lie: whether the tmux session still exists, whether its pane is dead,
#: whether the task's result file exists and names this very node, the status
#: log the children's turn-end hooks append to, and the agent's own live
#: listing ([agfi:h-agent-session-live-list]). Storing a lifecycle field would
#: only add a second source of truth to disagree with the first.
#:
#: The family: [agfi:agent-subagents-list] shows what is out there,
#: [agfi:agent-subagents-reconcile] forgets entries whose session is gone,
#: [agfi:agent-subagents-close] kills one by node id and verifies it by pid,
#: and [agfi:agent-clean-fz] is the picker over the first that feeds the last.
#: See =docs/agent-sessions.md=.
##
#: Empty, not resolved at load time: the skill reads `TMUX_SUBAGENTS_STATE'
#: from the environment, and a test run points it at a scratch directory for
#: the length of one command. Baking the value into a `typeset -g' at source
#: time would make this family blind to that. See
#: [agfi:h-agent-subagents-state-dir].
typeset -g agent_subagents_state_dir=''
#: Where the skill's helper scripts live. Only `tmux-subagent-status.sh' is
#: used from here, so that the `closed' event goes through the same single
#: writer as every other status line.
typeset -g agent_subagents_skill_dir="${HOME}/code/skills/tmux-subagents/skills/tmux-subagents"
#: How long a live session with nothing to say may sit before it is called
#: `stuck' rather than `idle'. Ten minutes: a child that has not written a
#: status line, a result, or a transcript message in that long has usually
#: died in a way tmux cannot see -- waiting on a prompt nobody will answer, or
#: retrying an API forever.
typeset -g agent_subagents_stuck_after=600
#: Seconds to wait for the registry lock before giving up. Short on purpose:
#: the only other writers are launches, which hold it for one file rewrite.
typeset -g agent_subagents_lock_timeout=5
#: How many lines of a child's pane [agfi:agent-subagents-preview] shows by
#: default. Short enough that the result and the checkpoints below it are still
#: on the same screen; [agfi:subagents-of-fz] asks for more with `--pane-lines',
#: where reading what the child is doing is the whole of the question.
typeset -g agent_subagents_preview_pane_lines=25
##
function h-agent-subagents-state-dir {
    : "the tmux-subagents state directory, resolved exactly as the skill does"
    #: `agent_subagents_state_dir' wins so a caller can point one command at
    #: another tree; otherwise this is the skill's own resolution, verbatim.
    ##
    ec "${agent_subagents_state_dir:-${TMUX_SUBAGENTS_STATE:-${XDG_STATE_HOME:-$HOME/.local/state}/tmux-subagents}}"
}

function h-agent-subagents-registry {
    : "path of the subagent registry JSON"
    ec "$(h-agent-subagents-state-dir)/agents.json"
}

function h-agent-subagents-lock-do {
    : "usage: h-agent-subagents-lock-do <command> [<args>...]
Runs the command holding the registry's exclusive lock."
    #: The lock is the stable `agents.json.lock' sidecar, never `agents.json'
    #: itself: every writer replaces that file by rename, so two writers would
    #: otherwise hold locks on two different inodes and both win.
    #:
    #: `zsystem flock' for the same reason [agfi:h-ddc-lock-do] uses it -- the
    #: lock dies with its holder for free, and it is built into zsh rather than
    #: being homebrew's `flock(1)'. Unlike that one this *refuses* on timeout
    #: instead of proceeding unserialised: a brightness key that races loses a
    #: step, a registry write that races loses an agent.
    #:
    #: flock is per open file description, so a second lock from *this* process
    #: on a new fd would deadlock against the first. Hence everything that can
    #: recurse -- closing a subtree -- calls the already-under-lock helper
    #: directly and never re-enters this one.
    ##
    local lockfile
    lockfile="$(h-agent-subagents-registry).lock" @RET

    zmodload zsh/system @RET
    mkdir -p "${lockfile:h}" @TRET
    : >> "${lockfile}" 2>/dev/null || true

    local lock_fd
    if ! zsystem flock -t "${agent_subagents_lock_timeout:-5}" -f lock_fd "${lockfile}" 2>/dev/null ; then
        ecerr "$0: timed out waiting for the subagent registry lock: ${lockfile}"
        return 1
    fi

    {
        reval "$@"
    } always {
        exec {lock_fd}>&-
    }
}

function h-agent-subagents-registry-drop {
    : "usage: h-agent-subagents-registry-drop <node-id>...
Removes those entries from the registry. The caller must hold the lock."
    #: Published the way the skill's launcher publishes: a unique temporary
    #: file in the registry's own directory, then `mv'. A fixed `.tmp' name
    #: lets two writers clobber each other, and rename is only atomic inside
    #: one directory.
    ##
    (( $# )) || return 0

    local reg
    reg="$(h-agent-subagents-registry)" @RET
    test -e "${reg}" || return 0

    local tmp
    tmp="$(gmktemp -p "${reg:h}" --suffix=.tmp .agents.XXXXXXXX)" @TRET

    {
        #: `--args' so a node id is data and never part of the jq program.
        jq --args 'delpaths([$ARGS.positional[] | [.]])' "$@" < "${reg}" > "${tmp}" @RET
        command mv -f "${tmp}" "${reg}" @RET
    } always {
        test -e "${tmp}" && command rm -f "${tmp}"
        true
    }
}

function h-agent-subagents-result-head {
    : "usage: h-agent-subagents-result-head <result file>
Prints <task_id><TAB><node_id><TAB><outcome> from its front matter; \`-' each."
    #: The skill's contract is that a result file opens with those three lines
    #: and then a markdown body, so only the head is read. A file whose ids do
    #: not match the entry is not that entry's result: an assignment gets a new
    #: task id *and* a new result path precisely so an old file cannot satisfy
    #: a new one.
    ##
    local file="${1}"
    test -e "${file}" || return 1

    local line key v
    local task='-' node='-' outcome='-'
    local -i n=0
    while IFS= read -r line ; do
        (( ++n > 40 )) && break
        [[ "${line}" == (task_id|node_id|outcome):* ]] || continue
        key="${line%%:*}"
        #: The three values are two ids and a keyword, none of which carries an
        #: internal space, so dropping all whitespace is a safe trim.
        v="${${line#*:}//[[:space:]]/}"
        case "${key}" in
            task_id) task="${v:--}" ;;
            node_id) node="${v:--}" ;;
            outcome) outcome="${v:--}" ;;
        esac
    done < "${file}"

    print -r -- "${task}"$'\t'"${node}"$'\t'"${outcome}"
}

function agent-subagents-list {
    : "one TSV row per registered tmux subagent, with its state derived now"
    #: Columns: node id, state, outcome (`-' unless a result file says), tmux
    #: session id, tmux pane id, task id, parent, live descendant count, last
    #: activity as local time and as an epoch, workdir, transcript, agent.
    #:
    #: The states, in the order they are decided:
    #:
    #:   gone         the tmux session is not there any more
    #:   exited       its pane is dead (`remain-on-exit' keeps it inspectable)
    #:   done         a result file exists whose front matter names this node
    #:   mismatch     a result file exists naming *someone else*
    #:   needs-input  the needs-input sibling exists
    #:   busy         the agent's live listing says it is working
    #:   stuck        alive, silent for longer than the threshold
    #:   idle         alive, silent for less
    #:   unknown      alive, and nothing could be read about it
    #:
    #: Everything is answered in a fixed number of processes rather than a few
    #: per entry: one `tmux list-sessions', one `tmux list-panes -a', one `jq'
    #: over the registry, one over the status log, one live listing, and one
    #: `agent_session list' per agent that still has a row to date.
    ##
    local state_dir reg
    state_dir="$(h-agent-subagents-state-dir)" @RET
    reg="${state_dir}/agents.json"
    if ! test -e "${reg}" ; then
        ecerr "$0: no subagent registry at ${reg}"
        return 1
    fi

    zmodload zsh/datetime @RET
    zmodload -F zsh/stat b:zstat @RET

    local -a entries
    entries=( ${(f)"$(jq -r '
        def d: if . == null or . == "" then "-" else . end ;
        to_entries[] | [ .key,
                         (.value.task_id|d), (.value.parent|d),
                         (.value.tmux_session_id|d), (.value.tmux_pane_id|d),
                         (.value.workdir|d), (.value.created|d) ] | @tsv
    ' "${reg}")"} ) @TRET
    (( ${#entries} )) || return 0

    #: One listing for every entry; `tmux has-session' per entry would be a
    #: round trip each. The identity option the autoname hooks write is itself
    #: tab separated (agent, id, transcript), so it is the *tail* of the line.
    local -A name_of id_of ident_of dead_of pid_of
    local line
    local -a f
    for line in ${(f)"$(command tmux list-sessions -F '#{session_id}'$'\t''#{session_name}'$'\t''#{@agent_session}' 2>/dev/null)"} ; do
        test -n "${line}" || continue
        f=( "${(@ps:\t:)line}" )
        name_of[${f[1]}]="${f[2]}"
        id_of[${f[2]}]="${f[1]}"
        ident_of[${f[1]}]="${f[3]}"$'\t'"${f[4]}"$'\t'"${f[5]}"
    done

    for line in ${(f)"$(command tmux list-panes -a -F '#{pane_id}'$'\t''#{pane_dead}'$'\t''#{pane_pid}' 2>/dev/null)"} ; do
        test -n "${line}" || continue
        f=( "${(@ps:\t:)line}" )
        dead_of[${f[1]}]="${f[2]}"
        pid_of[${f[1]}]="${f[3]}"
    done

    #: Newest status line per task, from one pass over the log. ISO timestamps
    #: sort as strings, so the newest is the string maximum; `fromjson? //
    #: empty' drops a half-written last line rather than failing the run.
    local -A status_ts
    local log="${state_dir}/status.jsonl"
    local task ts
    if test -e "${log}" ; then
        for line in ${(f)"$(jq -R -r 'fromjson? // empty | select(.task_id != null and .ts != null) | [.task_id, .ts] | @tsv' "${log}" 2>/dev/null)"} ; do
            test -n "${line}" || continue
            task="${line%%$'\t'*}"
            ts="${line#*$'\t'}"
            if [[ -z "${status_ts[$task]}" ]] || [[ "${ts}" > "${status_ts[$task]}" ]] ; then
                status_ts[$task]="${ts}"
            fi
        done
    fi

    #: The live listing knows which agent processes are alive and what each is
    #: doing; `local' is dynamically scoped, so this one call is reused by
    #: everything below it.
    local agent_session_live_list_cache="${agent_session_live_list_cache:-$(h-agent-session-live-list)}"
    local -A live_status live_transcript
    local tname sid
    for line in ${(f)agent_session_live_list_cache} ; do
        test -n "${line}" || continue
        f=( "${(@ps:\t:)line}" )
        tname="${f[6]}"
        test -n "${tname}" && [[ "${tname}" != '-' ]] || continue
        sid="${id_of[${tname}]}"
        test -n "${sid}" || continue
        #: Two agents in one tmux session would be two rows; the last wins,
        #: which for an `ag--' session cannot happen -- the skill puts exactly
        #: one agent in each.
        live_status[${sid}]="${f[7]:--}"
        live_transcript[${sid}]="${f[5]}"
    done

    local -a root_agents
    root_agents=( ${(f)"$(h-agent-session-root-agents)"} )

    #: Pass one: everything that can be decided without dating a transcript.
    #: A state left empty here means "alive, not busy, still to be dated".
    local -A state_of outcome_of task_of parent_of sid_of pane_of workdir_of
    local -A created_of transcript_of agent_of act_of
    local -a nodes f2 idf
    local row node parent pane workdir created st outcome transcript agent head
    local result_file needs_file
    local -i ep
    for row in "${entries[@]}" ; do
        f=( "${(@ps:\t:)row}" )
        node="${f[1]}" ; task="${f[2]}" ; parent="${f[3]}"
        sid="${f[4]}" ; pane="${f[5]}" ; workdir="${f[6]}" ; created="${f[7]}"
        test -n "${node}" || continue

        nodes+=( "${node}" )
        task_of[$node]="${task}" ; parent_of[$node]="${parent}"
        sid_of[$node]="${sid}" ; pane_of[$node]="${pane}"
        workdir_of[$node]="${workdir}" ; created_of[$node]="${created}"

        st='' ; outcome='-' ; transcript='-' ; agent='-'

        if [[ "${sid}" == '-' ]] || test -z "${name_of[$sid]}" ; then
            st=gone
        elif [[ "${pane}" != '-' && "${dead_of[$pane]}" == 1 ]] ; then
            st=exited
        fi

        result_file='' ; needs_file=''
        if [[ "${task}" != '-' ]] ; then
            result_file="${state_dir}/tasks/${task}/result.md"
            needs_file="${state_dir}/tasks/${task}/result.needs-input.md"
        fi

        if test -z "${st}" && test -n "${result_file}" && test -e "${result_file}" ; then
            head="$(h-agent-subagents-result-head "${result_file}")"
            f2=( "${(@ps:\t:)head}" )
            outcome="${f2[3]:--}"
            if [[ "${f2[1]}" == "${task}" && "${f2[2]}" == "${node}" ]] ; then
                st=done
            else
                #: Someone else's result sitting at this task's path. Never
                #: believed, and never quietly closed.
                st=mismatch
            fi
        fi

        if test -z "${st}" && test -n "${needs_file}" && test -e "${needs_file}" ; then
            st=needs-input
        fi

        #: Who lives in this tmux session, for every session that still
        #: exists rather than only for the undecided ones: a child that has
        #: published its result is still running, and its transcript is what
        #: the picker previews. The autoname hooks record the identity on the
        #: session itself; a session they have not reached yet is still in the
        #: live listing, which knows the transcript but not which agent's
        #: store it came out of -- hence the fallback.
        #:
        #: `exited' is included for the same reason, even though nothing is
        #: running in it: the identity option sits on the *session*, which
        #: `remain-on-exit' keeps alive along with the child's last screen, so
        #: it is readable exactly as long as there is somewhere to go. Only
        #: `gone' has nothing left to ask.
        if [[ "${st}" != gone ]] ; then
            idf=( "${(@ps:\t:)${ident_of[$sid]}}" )
            if test -n "${idf[1]}" ; then
                agent="${idf[1]}"
                transcript="${idf[3]:--}"
            fi
            if [[ "${transcript}" == '-' ]] && test -n "${live_transcript[$sid]}" ; then
                transcript="${live_transcript[$sid]}"
            fi
            if [[ "${agent}" == '-' && "${transcript}" != '-' ]] ; then
                h-agent-session-agent-in-roots "${transcript}" "${root_agents[@]}"
                agent="${REPLY}"
            fi

            #: `-' is unknown, not idle: an adapter that does not report a
            #: status says nothing about the agent. Only an undecided row can
            #: become `busy'; a published result outranks a running turn.
            if test -z "${st}" && [[ "${live_status[$sid]}" == busy ]] ; then
                st=busy
            fi
        fi

        #: The activity clock, best source first: the child's own status lines,
        #: then its transcript (dated in the batch below), then the result
        #: file, then the registry's `created'.
        ep=0
        if [[ "${task}" != '-' ]] && test -n "${status_ts[$task]}" ; then
            strftime -rs ep '%Y-%m-%dT%H:%M:%S' "${status_ts[$task]}" 2>/dev/null || ep=0
        fi

        state_of[$node]="${st}"
        outcome_of[$node]="${outcome}"
        transcript_of[$node]="${transcript}"
        agent_of[$node]="${agent}"
        act_of[$node]=${ep}
    done

    #: One `agent_session list' per agent rather than per row: `-only' names
    #: the transcripts, so nothing walks the corpus. See
    #: [agfi:h-agent-session-annotate-rows], which batches the same way.
    local -A by_agent tepoch
    local t a
    for node in "${nodes[@]}" ; do
        (( act_of[$node] )) && continue
        t="${transcript_of[$node]}" ; a="${agent_of[$node]}"
        [[ "${t}" != '-' && "${a}" != '-' ]] || continue
        test -e "${t}" || continue
        by_agent[$a]+="${t}"$'\n'
    done

    local -a only roots
    for a in ${(k)by_agent} ; do
        roots=( ${(f)"$(h-agent-session-call "${a}" roots 2>/dev/null)"} ) || continue
        (( ${#roots} )) || continue

        only=()
        for t in ${(fu)by_agent[$a]} ; do
            test -n "${t}" || continue
            only+=( -only "${t}" )
        done
        (( ${#only} )) || continue

        for line in ${(f)"$(agent_session "${a}" list "${only[@]}" "${roots[@]}" 2>/dev/null)"} ; do
            test -n "${line}" || continue
            f=( "${(@ps:\t:)line}" )
            tepoch[${f[2]}]="${f[1]}"
        done
    done

    #: Pass two: date every row, and only then call an undated live session
    #: stuck or idle.
    local -i now=${EPOCHSECONDS}
    local -i threshold=${agent_subagents_stuck_after:-600}
    local -a st_arr
    for node in "${nodes[@]}" ; do
        ep=${act_of[$node]:-0}

        if (( ep == 0 )) ; then
            t="${transcript_of[$node]}"
            [[ "${t}" != '-' ]] && ep=${tepoch[$t]:-0}
        fi

        st="${state_of[$node]}"
        if test -z "${st}" ; then
            if (( ep > 0 )) ; then
                if (( now - ep > threshold )) ; then
                    st=stuck
                else
                    st=idle
                fi
            else
                st=unknown
            fi
            state_of[$node]="${st}"
        fi

        if (( ep == 0 )) ; then
            task="${task_of[$node]}"
            if [[ "${task}" != '-' ]] && test -e "${state_dir}/tasks/${task}/result.md" ; then
                st_arr=()
                zstat -A st_arr +mtime "${state_dir}/tasks/${task}/result.md" 2>/dev/null && ep=${st_arr[1]:-0}
            fi
        fi
        if (( ep == 0 )) && [[ "${created_of[$node]}" != '-' ]] ; then
            strftime -rs ep '%Y-%m-%dT%H:%M:%S' "${created_of[$node]}" 2>/dev/null || ep=0
        fi

        act_of[$node]=${ep}
    done

    #: Live descendants: the direct children still capable of doing anything.
    #: A `done' child is finished, and a `gone' or `exited' one has no process
    #: left, so none of the three blocks a parent's closure.
    local -A desc_of
    for node in "${nodes[@]}" ; do
        parent="${parent_of[$node]}"
        [[ "${parent}" != '-' ]] || continue
        case "${state_of[$node]}" in
            gone|exited|done) continue ;;
        esac
        (( desc_of[$parent]++ ))
    done

    local when
    for node in "${nodes[@]}" ; do
        ep=${act_of[$node]:-0}
        when='-'
        (( ep > 0 )) && strftime -s when '%Y-%m-%d %H:%M' ${ep}

        print -r -- "${node}"$'\t'"${state_of[$node]}"$'\t'"${outcome_of[$node]}"$'\t'"${sid_of[$node]}"$'\t'"${pane_of[$node]}"$'\t'"${task_of[$node]}"$'\t'"${parent_of[$node]}"$'\t'"${desc_of[$node]:-0}"$'\t'"${when}"$'\t'"${ep}"$'\t'"${workdir_of[$node]}"$'\t'"${transcript_of[$node]}"$'\t'"${agent_of[$node]}"
    done
}

function h-agent-subagents-reconcile-locked {
    : "the body of [agfi:agent-subagents-reconcile]; the caller holds the lock"
    local -a rows gone f
    rows=( ${(f)"$(agent-subagents-list)"} ) @TRET

    local row
    for row in "${rows[@]}" ; do
        f=( "${(@ps:\t:)row}" )
        [[ "${f[2]}" == gone ]] || continue
        gone+=( "${f[1]}" )
    done
    (( ${#gone} )) || return 0

    h-agent-subagents-registry-drop "${gone[@]}" @RET

    local node
    for node in "${gone[@]}" ; do
        ecgray "$0: dropped ${node} (tmux session gone)"
    done
}

function agent-subagents-reconcile {
    : "forgets registry entries whose tmux session no longer exists"
    #: Only `gone' entries, and only ever the registry: there is no process to
    #: kill and no task directory is touched. An `exited' entry keeps its dead
    #: pane, which is still inspectable and is [agfi:tmuxzombie-kill]'s job.
    #:
    #: Under the lock and *re-derived inside it*, so an entry a launcher added
    #: a moment ago cannot be dropped for having no session yet.
    ##
    h-agent-subagents-lock-do h-agent-subagents-reconcile-locked
}

function h-agent-subagents-descendant-pids {
    : "usage: h-agent-subagents-descendant-pids <pid>...
Prints each pid and every process below it, one per line."
    #: Collected *before* the kill, because after it there is no tree left to
    #: walk and the verification would have nothing to check.
    ##
    local pid
    for pid in "$@" ; do
        test -n "${pid}" || continue
        print -r -- "${pid}"
        ps-grandchildren "${pid}" 2>/dev/null
    done
}

function h-agent-subagents-pids-alive {
    : "usage: h-agent-subagents-pids-alive <pid>...
Prints the given pids that still exist."
    local pid
    for pid in "$@" ; do
        test -n "${pid}" || continue
        kill -0 "${pid}" 2>/dev/null && print -r -- "${pid}"
    done
    return 0
}

function h-agent-subagents-close-one {
    : "usage: h-agent-subagents-close-one <node-id>
Closes one subagent. The caller must hold the registry lock."
    #: Split out from [agfi:agent-subagents-close] because closing a subtree
    #: recurses, and `zsystem flock' is per file descriptor: re-entering
    #: [agfi:h-agent-subagents-lock-do] from inside itself would deadlock this
    #: process against its own lock.
    ##
    local node="${1}"
    assert-args node @RET

    #: Re-derived here, under the lock, not read from whatever list the caller
    #: was looking at: a picker's rows are as old as the person reading them,
    #: and the child may have started a new turn since.
    local -a rows f
    rows=( ${(f)"$(agent-subagents-list)"} ) @TRET

    local row found=''
    local state outcome sid pane task parent
    for row in "${rows[@]}" ; do
        f=( "${(@ps:\t:)row}" )
        [[ "${f[1]}" == "${node}" ]] || continue
        found=y
        state="${f[2]}" ; outcome="${f[3]}" ; sid="${f[4]}" ; pane="${f[5]}"
        task="${f[6]}" ; parent="${f[7]}"
        break
    done
    if test -z "${found}" ; then
        ecerr "$0: no such subagent in the registry: ${node}"
        return 1
    fi

    local force="${agent_subagents_close_force:-n}"
    local subtree="${agent_subagents_close_subtree:-n}"

    if [[ "${state}" == busy ]] && ! bool "${force}" ; then
        ecerr "$0: ${node} is busy; pass -f (or agent_subagents_close_force=y) to close it anyway"
        return 1
    fi

    #: Never the session this very shell is sitting in. The registry cannot
    #: normally hold it, but a hand-written entry could, and the failure mode
    #: is killing the parent mid-close.
    if test -n "${TMUX_PANE}" ; then
        local own
        own="$(command tmux display-message -p -t "${TMUX_PANE}" '#{session_id}' 2>/dev/null)" || own=''
        if test -n "${own}" && [[ "${own}" == "${sid}" ]] ; then
            ecerr "$0: ${node} is the tmux session this shell runs in; refusing"
            return 1
        fi
    fi

    #: Live descendants, direct children only: the recursion below reaches the
    #: rest, deepest first, because each child closes its own children before
    #: itself.
    local -a kids
    for row in "${rows[@]}" ; do
        f=( "${(@ps:\t:)row}" )
        [[ "${f[7]}" == "${node}" ]] || continue
        case "${f[2]}" in
            gone|exited|done) continue ;;
        esac
        kids+=( "${f[1]}" )
    done

    if (( ${#kids} )) ; then
        if ! bool "${subtree}" ; then
            ecerr "$0: ${node} has ${#kids} live descendant(s): ${(j:, :)kids}"
            ecerr "$0: close them first, or set agent_subagents_close_subtree=y"
            return 1
        fi

        local kid
        for kid in "${kids[@]}" ; do
            h-agent-subagents-close-one "${kid}" @RET
        done
    fi

    case "${state}" in
        gone)
            #: Nothing to kill; the entry is all that is left of it.
            h-agent-subagents-registry-drop "${node}" @RET
            ecgray "$0: ${node}: session already gone, entry dropped"
            return 0
            ;;
        exited)
            #: The pane is dead but retained on purpose, so the child's last
            #: screen can still be read. Removing dead panes is
            #: [agfi:tmuxzombie-kill]'s job, and it does it for every session
            #: at once.
            ecerr "$0: ${node}: pane is dead and kept for inspection; use tmuxzombie-kill (tzkill), then reconcile"
            return 0
            ;;
    esac

    #: Every pane of the session, and everything under each of them: an agent
    #: forks editors, language servers and shells, and killing the pane proves
    #: nothing about those.
    local -a pane_pids all_pids
    pane_pids=( ${(f)"$(command tmux list-panes -s -t "${sid}" -F '#{pane_pid}' 2>/dev/null)"} )
    if ! (( ${#pane_pids} )) ; then
        ecerr "$0: ${node}: no pane pids for ${sid}; refusing to guess"
        return 1
    fi
    all_pids=( ${(f)"$(h-agent-subagents-descendant-pids "${pane_pids[@]}")"} )

    ecgray "$0: ${node}: TERM on ${#all_pids} process(es) under ${sid}"
    kill-withchildren -15 "${pane_pids[@]}" &>/dev/null

    #: Up to five seconds for a graceful exit, then no more waiting: an agent
    #: that ignores TERM has usually wedged, and the pane is being closed
    #: either way.
    local -a alive
    local -i i
    for (( i = 0 ; i < 50 ; i++ )) ; do
        alive=( ${(f)"$(h-agent-subagents-pids-alive "${all_pids[@]}")"} )
        (( ${#alive} )) || break
        command sleep 0.1
    done
    if (( ${#alive} )) ; then
        ecgray "$0: ${node}: ${#alive} process(es) survived TERM; KILL"
        kill-withchildren -9 "${pane_pids[@]}" &>/dev/null
        command sleep 0.5
    fi

    command tmux kill-session -t "${sid}" &>/dev/null

    #: Verified by pid, never by session name. Right after a `kill-session' on
    #: 2026-09-09 one child `claude' was still listed and exited a second
    #: later, so a session that has vanished is no proof its processes have.
    alive=( ${(f)"$(h-agent-subagents-pids-alive "${all_pids[@]}")"} )
    if (( ${#alive} )) ; then
        ecerr "$0: ${node}: still alive after kill: ${(j:, :)alive}; entry kept"
        return 1
    fi

    h-agent-subagents-registry-drop "${node}" @RET

    #: The `closed' event goes through the skill's own appender, so that log
    #: keeps exactly one writer and one format.
    local status_sh="${agent_subagents_skill_dir}/scripts/tmux-subagent-status.sh"
    if test -x "${status_sh}" ; then
        "${status_sh}" "${task}" "${node}" closed "closed by ${0}" &>/dev/null || true
    fi

    ecgray "$0: ${node}: closed; was ${state}, outcome ${outcome}, ${sid}"
    return 0
}

function agent-subagents-close {
    : "usage: agent-subagents-close [-f] <node-id>...
Closes the named subagents: kills their processes, their tmux session, and
their registry entry."
    #: `-f' (or agent_subagents_close_force=y) closes a busy child;
    #: agent_subagents_close_subtree=y closes its live descendants with it,
    #: deepest first. Neither is on by default: the whole point of the check is
    #: that a list is stale by the time somebody acts on it.
    #:
    #: Task directories are never touched. A result file is the record of what
    #: the child did, and closing a terminal is not deleting a report.
    ##
    local force="${agent_subagents_close_force:-n}"
    if [[ "${1}" == -f ]] ; then
        force=y
        shift
    fi

    local -a nodes
    nodes=( "$@" )
    assert-args nodes @RET

    local node ret=0
    for node in "${nodes[@]}" ; do
        #: One lock acquisition per node, not one for the batch: a long kill
        #: should not hold the registry against a launcher for the whole run.
        agent_subagents_close_force="${force}" \
            h-agent-subagents-lock-do h-agent-subagents-close-one "${node}" || ret=$?
    done

    return ${ret}
}

function agent-subagents-preview {
    : "usage: agent-subagents-preview [--pane-lines <n>] <node-id>
Plain-text summary of one subagent, for an fzf preview."
    #: Never `eval', never a shell expansion of anything captured: the pane
    #: holds whatever the child printed. The capture is taken without `-e', so
    #: no escape sequences are requested in the first place, and the control
    #: bytes an agent TUI writes anyway are deleted rather than rendered --
    #: a preview that interprets them can be made to rewrite the screen.
    #:
    #: Order: what it is, then what is on its screen, then what it has written
    #: down. The screen comes second rather than last because it is the part
    #: that answers "what is this one doing *now*", and a preview pane is read
    #: from the top -- with the result and the checkpoints above it, the live
    #: half was reliably the half nobody scrolled to.
    #:
    #: `--pane-lines' (default `agent_subagents_preview_pane_lines') is an
    #: argument and not only a knob because a preview reaches us through the
    #: garden, and `brishzq.zsh' takes a command and its arguments -- not a
    #: shell line -- so an `var=value' prefix in front of the call comes back
    #: as `command not found: var=value'. [agfi:subagents-of-fz] asks for more
    #: lines than [agfi:agent-clean-fz], whose question is only whether to
    #: close the thing.
    #:
    #: Transcripts are deliberately not read here, in any form. Everything
    #: below comes from the registry, the task directory, the status log and
    #: the pane -- which is what the child chose to put on a screen -- so that
    #: browsing a fan-out never pages other conversations past you.
    ##
    local pane_lines="${agent_subagents_preview_pane_lines:-25}"
    while (( $# )) ; do
        case "${1}" in
            --pane-lines) pane_lines="${2}" ; shift 2 ;;
            --) shift ; break ;;
            *) break ;;
        esac
    done

    local node="${1}"
    assert-args node @RET

    local state_dir
    state_dir="$(h-agent-subagents-state-dir)" @RET

    local -a rows f
    rows=( ${(f)"$(agent-subagents-list)"} ) @TRET

    local row found=''
    local state outcome sid pane task parent kids when ep workdir transcript agent
    for row in "${rows[@]}" ; do
        f=( "${(@ps:\t:)row}" )
        [[ "${f[1]}" == "${node}" ]] || continue
        found=y
        state="${f[2]}" ; outcome="${f[3]}" ; sid="${f[4]}" ; pane="${f[5]}"
        task="${f[6]}" ; parent="${f[7]}" ; kids="${f[8]}" ; when="${f[9]}"
        ep="${f[10]}" ; workdir="${f[11]}" ; transcript="${f[12]}" ; agent="${f[13]}"
        break
    done
    if test -z "${found}" ; then
        ec "no such subagent in the registry: ${node}"
        return 0
    fi

    ec "node         ${node}"
    ec "state        ${state}"
    ec "outcome      ${outcome}"
    ec "agent        ${agent}"
    ec "tmux         ${sid}  ${pane}"
    ec "task         ${task}"
    ec "parent       ${parent}"
    ec "descendants  ${kids}"
    ec "activity     ${when}"
    ec "workdir      ${workdir}"
    ec "transcript   ${transcript}"

    #: The launch metadata, from [agfi:h-agent-subagents-registry-meta]: what
    #: this child was asked to be, as against the derived state above, which is
    #: what it turned out to be.
    local meta
    local -a mf
    meta="$(h-agent-subagents-registry-meta 2>/dev/null | command grep -F -m1 -- "${node}"$'\t')" || meta=''
    if test -n "${meta}" ; then
        mf=( "${(@ps:\t:)meta}" )
        ec "launched as  ${mf[4]:--}/${mf[5]:--}  model ${mf[6]:--}"
        ec "created      ${mf[7]:--}"
        ec "lineage      ${mf[3]:--}"
        ec "socket       ${mf[8]:--}"
        ec "resume       ${mf[9]:--}"
    fi

    #: Captured first and printed only if there is anything: a `gone' child's
    #: pane id still reads well in the header above, and an empty section under
    #: it says nothing and costs a screenful of the preview.
    local pane_text=''
    if [[ "${pane}" != '-' ]] ; then
        pane_text="$(command tmux capture-pane -p -t "${pane}" -S -"${pane_lines}" 2>/dev/null |
            command tr -d '\000-\010\013\014\016-\037\177')"
    fi
    if test -n "${pane_text//[[:space:]]/}" ; then
        ec ''
        ec "--- pane ${pane} ---"
        ec "${pane_text}"
    fi

    local result_file="${state_dir}/tasks/${task}/result.md"
    local needs_file="${state_dir}/tasks/${task}/result.needs-input.md"

    if [[ "${task}" != '-' ]] && test -e "${result_file}" ; then
        ec ''
        ec '--- result ---'
        #: Front matter and the first paragraph, which the skill's contract
        #: makes the summary; the rest is artifacts and verification notes.
        command awk 'NR <= 40 {
                #: The front matter and its fences first, then the first
                #: paragraph of the body, which the skill makes the summary.
                if ($0 ~ /^(task_id|node_id|outcome):/) { print ; next }
                if ($0 ~ /^-{3,}$/) { print ; next }
                #: A blank line ends the summary, but only once there is one:
                #: a result whose body opens with a Summary heading would
                #: otherwise preview as that one word. No apostrophes in here:
                #: the awk program is itself single quoted.
                if ($0 ~ /^[[:space:]]*$/) {
                    if (body >= 3) exit
                    if (body > 0) print ""
                    next
                }
                body++ ; print
            }' "${result_file}" |
            command tr -d '\000-\010\013\014\016-\037\177'
    elif [[ "${task}" != '-' ]] && test -e "${needs_file}" ; then
        ec ''
        ec '--- needs input ---'
        command head -n 20 "${needs_file}" | command tr -d '\000-\010\013\014\016-\037\177'
    fi

    #: The newest checkpoint, which is where a long-running child writes what
    #: it has decided so far. A result file ends the story and a checkpoint is
    #: the middle of it, so both are shown when both exist rather than one
    #: standing in for the other.
    if [[ "${task}" != '-' ]] ; then
        local -a checkpoints
        checkpoints=( "${state_dir}/tasks/${task}"/checkpoint*.md(N.om) )
        if (( ${#checkpoints} )) ; then
            ec ''
            ec "--- ${checkpoints[1]:t} ---"
            command tail -n 20 "${checkpoints[1]}" |
                command tr -d '\000-\010\013\014\016-\037\177'
        fi
    fi

    if [[ "${task}" != '-' ]] && test -e "${state_dir}/status.jsonl" ; then
        local -a status_lines
        status_lines=( ${(f)"$(jq -R -r --arg t "${task}" '
            fromjson? // empty | select(.task_id == $t)
            | [ (.ts // "-"), (.state // "-"), ((.summary // "") | gsub("\\s+"; " ") | .[0:120]) ]
            | @tsv' "${state_dir}/status.jsonl" 2>/dev/null | command tail -n 5)"} )
        if (( ${#status_lines} )) ; then
            ec ''
            ec '--- status ---'
            print -rl -- "${status_lines[@]}" | command tr -d '\000-\010\013\014\016-\037\177'
        fi
    fi

    return 0
}

function h-agent-session-subagent-rows {
    #: One row per registered tmux subagent, in the five column layout
    #: [agfi:h-agent-session-annotate-rows] reads -- the same shape
    #: [agfi:h-agent-session-tmux-dead-rows] emits -- so that
    #: [agfi:h-agent-session-tmux-rows] can offer the skill's children beside
    #: your own sessions. [agfi:fftmux-agent-subagents] is the picker.
    #:
    #: The rows come from [agfi:agent-subagents-list] and never from
    #: [agfi:h-agent-session-live-list], even though a running child *is* a
    #: live agent sitting in a tmux session. Two reasons, and both are the
    #: point of the family: the registry still knows a child whose agent lost
    #: its own liveness record -- one stayed invisible for fifteen hours behind
    #: a mangled `sessions/<pid>.json', see
    #: [agfi:h-agent-session-records-unreadable] -- and it knows what the child
    #: is *doing*, which no live row can say: whether it published a result,
    #: whether it is waiting on a person, whether it has gone quiet.
    #:
    #: That derived state is the badge, from
    #: [agfi:h-agent-subagents-state-rank], and it doubles as the mark saying
    #: this row is a child: no other row kind in this family carries one of
    #: those glyphs.
    #:
    #: `gone' is skipped -- there is no session left to go to. `exited' is
    #: kept, unlike in [agfi:agent-clean-fz], which drops it because clearing a
    #: dead pane is [agfi:tmuxzombie-kill]'s job in one pass. Here the opposite
    #: holds: `remain-on-exit' left that child's last screen readable, and
    #: going there to read it is the whole of what a goto picker does.
    #:
    #: A child with no transcript yet has nothing to preview and gets no row,
    #: which is the rule [agfi:h-agent-session-tmux-rows] already applies to
    #: its live half.
    #:
    #: Prints nothing and fails when there are none, like the dead rows, so a
    #: caller can add this half without having to test for emptiness first.
    ##
    local -a rows
    rows=( ${(f)"$(agent-subagents-list 2>/dev/null)"} )
    (( ${#rows} )) || return 1

    local out='' row state sid transcript agent
    local -a f
    for row in "${rows[@]}" ; do
        test -n "${row}" || continue

        f=( "${(@ps:\t:)row}" )
        state="${f[2]}" ; sid="${f[4]}" ; transcript="${f[12]}" ; agent="${f[13]}"

        [[ "${state}" == gone ]] && continue
        test -n "${sid}" && [[ "${sid}" != '-' ]] || continue
        test -n "${transcript}" && [[ "${transcript}" != '-' ]] && test -e "${transcript}" || continue

        h-agent-subagents-state-rank "${state}"

        #: The label ends `  -', which is how a caller tells
        #: [agfi:h-agent-session-annotate-rows] that the name position is open:
        #: the node id is the tmux session name, and the child's own name for
        #: the conversation is read out of the transcript and put after it. The
        #: full task id is deliberately not here -- the node id already spells
        #: its slug, and [agfi:agent-subagents-preview] has the rest.
        out+="${sid}"$'\t'"${transcript}"$'\t'"${agent}"$'\t'"${f[1]}  -"$'\t'"${REPLY#*$'\t'}"$'\n'
    done

    test -n "${out}" || return 1

    ec "${out%$'\n'}"
}

function h-agent-subagents-state-rank {
    : "usage: h-agent-subagents-state-rank <state>
Sets REPLY to the picker's sort rank for that state, and to its emoji."
    #: The order the picker offers things in, and the reason for it: what is
    #: finished and safe to close first, what is broken next, then what is
    #: waiting on a person, then the live ones from most abandoned to least.
    #: `busy' comes last and is normally not shown at all.
    #:
    #: `exited' is in the table for [agfi:h-agent-session-subagent-rows], which
    #: offers those rows where [agfi:agent-clean-fz] skips them. `🏁' rather
    #: than the `💀' of [agfi:h-agent-session-tmux-dead-rows]: that one means a
    #: `/done' report is sitting on the screen, and a child's pane can die any
    #: number of other ways. `gone' shares `exited''s rank and carries `👻',
    #: for [agfi:h-agent-subagents-child-rows], which is the one picker that
    #: offers a child with no tmux session left: there is nothing to attach to,
    #: and saying so is better than leaving it out of a list of what you
    #: launched.
    ##
    case "${1}" in
        done)        REPLY=$'1\t✅' ;;
        exited)      REPLY=$'2\t🏁' ;;
        gone)        REPLY=$'2\t👻' ;;
        mismatch)    REPLY=$'3\t❌' ;;
        needs-input) REPLY=$'4\t❓' ;;
        stuck)       REPLY=$'5\t🪦' ;;
        idle)        REPLY=$'6\t💤' ;;
        unknown)     REPLY=$'7\t❔' ;;
        busy)        REPLY=$'8\t⏳' ;;
        *)           REPLY=$'9\t❔' ;;
    esac
}

function agent-clean-fz {
    : "usage: agent-clean-fz [<query>]
Fuzzy-picks finished tmux subagents and closes them; busy ones are hidden, see agent-clean-all-fz."
    #: `gone' and `exited' rows are never offered: a gone entry is dropped by
    #: the reconcile above before the list is even built, and a dead pane is
    #: [agfi:tmuxzombie-kill]'s business -- it removes every one of them in one
    #: go, and this picker would only be a slower way to do the same.
    #:
    #: `busy' rows are hidden unless agent_clean_fz_all_p=y, so that the
    #: fastest thing to do -- select everything and hit enter -- cannot close
    #: a child that is in the middle of a turn.
    ##
    local query="$*"

    bella_zsh_disable1

    agent-subagents-reconcile

    local -a rows
    rows=( ${(f)"$(agent-subagents-list)"} ) @TRET
    (( ${#rows} )) || {
        ecerr "$0: no registered subagents"
        return 1
    }

    local all_p="${agent_clean_fz_all_p:-n}"

    local -a f out
    local row state emoji rank label task workdir kids
    for row in "${rows[@]}" ; do
        f=( "${(@ps:\t:)row}" )
        state="${f[2]}"

        case "${state}" in
            gone|exited) continue ;;
            busy) bool "${all_p}" || continue ;;
        esac

        h-agent-subagents-state-rank "${state}"
        rank="${REPLY%%$'\t'*}"
        emoji="${REPLY#*$'\t'}"

        #: Enough to tell two children of one run apart without reading the
        #: node id twice: where it works, and what it was asked to do.
        workdir="${f[11]}" ; task="${f[6]}"
        label="${workdir:t}"
        [[ "${task}" != '-' ]] && label+="  ${task}"

        kids=''
        (( ${f[8]} )) && kids="+${f[8]}"

        #: [agfi:h-agent-session-fz]'s layout: a caller column, the transcript,
        #: the agent, and then the display columns. The node id is the caller
        #: column, so that is what comes back.
        out+=( "${rank}"$'\t'"${f[10]}"$'\t'"${f[1]}"$'\t'"${f[12]}"$'\t'"${f[13]}"$'\t'"${emoji} ${f[1]}"$'\t'"${f[3]}"$'\t'"${f[9]}"$'\t'"${kids}"$'\t'"${label}" )
    done

    if ! (( ${#out} )) ; then
        ecerr "$0: nothing to clean up (busy children are hidden; agent_clean_fz_all_p=y shows them)"
        return 1
    fi

    #: State first, newest activity first inside a state. Sorted on a prefixed
    #: copy and the prefix cut off again, the way
    #: [agfi:h-agent-session-annotate-rows] does it, so the row layout never
    #: sees the sort keys.
    local sorted
    sorted="$(print -rl -- "${out[@]}" | command sort -t $'\t' -k1,1n -k2,2nr | command cut -f3-)" @RET

    #: Ours wins because fzf takes the last `--preview': the subagent summary
    #: first, then the transcript preview the Go binary renders, and that one
    #: only when the row has a transcript at all. A dash-compatible one-liner,
    #: since fzf runs it in its own `sh'.
    local go_preview brishzq preview
    go_preview="$(h-agent-session-preview-cmd '{3}')" @RET
    brishzq="$(gquote "${commands[brishzq.zsh]:-brishzq.zsh}")" @RET
    preview="p={2}; ${brishzq} agent-subagents-preview {1}; test \"\$p\" != - && ${go_preview} \"\$p\""

    local picks
    picks="$(ec "${sorted}" | h-agent-session-fz multi --query "${query}" --preview "${preview}")" || return $?
    test -n "${picks}" || return 0

    local -a nodes
    local pick
    for pick in ${(f)picks} ; do
        test -n "${pick}" || continue
        nodes+=( "${pick%%$'\t'*}" )
    done
    (( ${#nodes} )) || return 0

    agent-subagents-close "${nodes[@]}"
}
function agent-clean-all-fz {
    : "usage: agent-clean-all-fz [<query>]
Like agent-clean-fz, but offers busy children too; closing one still needs -f (agent_subagents_close_force=y)."

    agent_clean_fz_all_p=y agent-clean-fz "$@"
}
##
#: Browsing the subagents *one* agent launched
#:
#: [agfi:subagents-of-fz] is two pickers in a row: which agent, and then which
#: of that agent's children to attach to. The fan-out pickers answer a
#: different question -- [agfi:fftmux-agent-subagents] offers every registered
#: child on the machine, which during a run with several coordinators is dozens
#: of rows with nothing saying whose they are -- and the parentage that would
#: say it lives in the registry and nowhere else: a child's tmux session name
#: spells its project and its task, never its parent.
#:
#: The first picker takes `--select-1', so `subagents-of-fz <agent>' skips it
#: when the query matches one agent. The second never does, deliberately: the
#: list is the point, and a fan-out that produced exactly one child is
#: precisely when you want to see what state it is in before landing in it.
##
#: The reset every coloured cell ends with. A literal SGR rather than
#: [agfi:colorfg] and its family, which print nothing unless the destination is
#: a colour tty: a picker row's destination is a pipe into fzf, and
#: [agfi:h-agent-session-fz] gives fzf `--ansi' so it renders them for us.
typeset -g agent_subagents_color_reset=$'\e[0m'
#: Set by [agfi:h-agent-subagents-index] in its caller's scope, so a second
#: call is a no-op. Global and empty so that a function which never declares it
#: still works, just without the sharing.
typeset -g agent_subagents_index_loaded=''

function h-agent-subagents-state-color {
    : "usage: h-agent-subagents-state-color <state>
Sets REPLY to the SGR sequence that state's cell is written in."
    #: Green is "a process is running", dim is "nothing is left to run", yellow
    #: is "alive and nobody can say what it is doing", and the three that want a
    #: person -- a foreign result, a question, a crash -- are the loud ones.
    #:
    #: Split from [agfi:h-agent-subagents-state-rank] rather than folded into
    #: it: that one's emoji is read at a glance and its rank orders a list,
    #: while this is a property of a *cell*, and only a picker that prints the
    #: state as a word has anywhere to put it.
    ##
    case "${1}" in
        busy|idle)     REPLY=$'\e[32m' ;;
        done)          REPLY=$'\e[36m' ;;
        exited|gone)   REPLY=$'\e[2m'  ;;
        mismatch)      REPLY=$'\e[31m' ;;
        needs-input)   REPLY=$'\e[35m' ;;
        stuck|unknown) REPLY=$'\e[33m' ;;
        *)             REPLY=$'\e[33m' ;;
    esac
}

function h-agent-subagents-registry-meta {
    : "one TSV row per registry entry: node, parent, lineage, provider, launcher, requested_model, created, socket, resume_state"
    #: The launch metadata: what the child was *asked* to be, written once and
    #: never revisited. [agfi:agent-subagents-list] deliberately carries none of
    #: it -- every column that one prints is worked out afresh on each read --
    #: so the two are joined by node id where both are wanted, rather than
    #: either growing the other's job.
    #:
    #: `process_state' and `task_outcome' are not offered here and never will
    #: be. They are written at launch and never updated, so they lie: two
    #: finished children read `running' for as long as the file stood. The
    #: state worth showing is the derived one.
    ##
    local reg
    reg="$(h-agent-subagents-registry)" @RET
    test -e "${reg}" || return 1

    jq -r '
        def d: if . == null or . == "" then "-" else . end ;
        to_entries[] | [ .key,
                         (.value.parent|d), (.value.lineage|d),
                         (.value.provider|d), (.value.launcher|d),
                         (.value.requested_model|d), (.value.created|d),
                         (.value.tmux_socket|d), (.value.resume_state|d) ] | @tsv
    ' "${reg}"
}

function h-agent-subagents-index {
    : "fills the caller's maps with everything known about every registered subagent"
    #: Dynamically scoped on purpose, the way `agent_session_live_list_cache'
    #: is: the caller declares the maps `local' and every helper below it reads
    #: them, so one [agfi:agent-subagents-list] and one
    #: [agfi:h-agent-subagents-registry-meta] answer a whole command instead of
    #: a pair per row builder. A second call is a no-op, so each builder can
    #: call it and still work on its own.
    #:
    #: What the caller declares, all keyed by node id:
    #:   local agent_subagents_index_loaded=''
    #:   local -a sub_nodes
    #:   local -A sub_state sub_outcome sub_sid sub_pane sub_task sub_parent
    #:   local -A sub_kids sub_when sub_epoch sub_workdir sub_transcript
    #:   local -A sub_agent sub_lineage sub_provider sub_launcher sub_model
    #:   local -A sub_created sub_socket sub_resume
    ##
    test -z "${agent_subagents_index_loaded}" || return 0

    local -a rows f
    rows=( ${(f)"$(agent-subagents-list)"} ) @TRET

    local row node
    for row in "${rows[@]}" ; do
        test -n "${row}" || continue
        f=( "${(@ps:\t:)row}" )
        node="${f[1]}"
        test -n "${node}" || continue

        sub_nodes+=( "${node}" )
        sub_state[$node]="${f[2]}"       ; sub_outcome[$node]="${f[3]}"
        sub_sid[$node]="${f[4]}"         ; sub_pane[$node]="${f[5]}"
        sub_task[$node]="${f[6]}"        ; sub_parent[$node]="${f[7]}"
        sub_kids[$node]="${f[8]}"        ; sub_when[$node]="${f[9]}"
        sub_epoch[$node]="${f[10]}"      ; sub_workdir[$node]="${f[11]}"
        sub_transcript[$node]="${f[12]}" ; sub_agent[$node]="${f[13]}"
    done

    for row in ${(f)"$(h-agent-subagents-registry-meta 2>/dev/null)"} ; do
        test -n "${row}" || continue
        f=( "${(@ps:\t:)row}" )
        node="${f[1]}"
        test -n "${node}" || continue

        #: The parent is read here as well as from the derived list -- it is
        #: the same field -- so that an entry the derived list dropped still
        #: has an ancestry.
        sub_parent[$node]="${sub_parent[$node]:-${f[2]}}"
        sub_lineage[$node]="${f[3]}"   ; sub_provider[$node]="${f[4]}"
        sub_launcher[$node]="${f[5]}"  ; sub_model[$node]="${f[6]}"
        sub_created[$node]="${f[7]}"   ; sub_socket[$node]="${f[8]}"
        sub_resume[$node]="${f[9]}"
    done

    agent_subagents_index_loaded=y
}

function h-agent-subagents-lineage-of {
    : "usage: h-agent-subagents-lineage-of <node id>
Sets REPLY to that node's ancestor chain, root first, \`/' separated, without the node itself."
    #: The registry writes the chain into `lineage' at launch, and a launcher
    #: with no parent to record wrote the node's *own* id there; that entry
    #: would otherwise come out as its own ancestor and its own descendant. So
    #: a segment equal to the node is dropped rather than believed.
    #:
    #: Reads the caller's `sub_lineage'; see [agfi:h-agent-subagents-index].
    ##
    local node="${1}"

    local -a segs keep
    segs=( ${(s:/:)${sub_lineage[$node]}} )

    local seg
    for seg in "${segs[@]}" ; do
        test -n "${seg}" && [[ "${seg}" != '-' ]] || continue
        [[ "${seg}" == "${node}" ]] && continue
        keep+=( "${seg}" )
    done

    REPLY="${(j:/:)keep}"
}

function h-agent-subagents-depth-under {
    : "usage: h-agent-subagents-depth-under <node id> <ancestor id>
Sets REPLY to how many generations below that ancestor the node sits; fails when it is not below it."
    #: `lineage' runs root first and ends at the parent, so the ancestor's
    #: distance from the *end* is the answer: the last segment is the parent,
    #: which makes the node a direct child, which is depth 1.
    ##
    local node="${1}" ancestor="${2}"

    h-agent-subagents-lineage-of "${node}"
    test -n "${REPLY}" || return 1

    local -a segs
    segs=( ${(s:/:)REPLY} )

    local -i i
    for (( i = ${#segs} ; i >= 1 ; i-- )) ; do
        if [[ "${segs[i]}" == "${ancestor}" ]] ; then
            REPLY=$(( ${#segs} - i + 1 ))
            return 0
        fi
    done

    return 1
}

function h-agent-subagents-tmux-cmd {
    : "usage: h-agent-subagents-tmux-cmd <tmux socket or ->
Sets the array \`reply' to the tmux command addressing that socket."
    #: The socket comes from the registry entry rather than from our own
    #: environment: a child launched against another tmux server is invisible
    #: to a plain `tmux', and calling it dead on that basis would be a lie.
    ##
    local socket="${1}"

    reply=( command tmux )
    if test -n "${socket}" && [[ "${socket}" != '-' ]] ; then
        reply+=( -S "${socket}" )
    fi
}

function h-agent-subagents-alive-p {
    : "usage: h-agent-subagents-alive-p <tmux socket or -> <session name or id>
True when that tmux session still exists on that socket."
    #: `=' is tmux's exact-name match. Without it a name is a prefix, and these
    #: names -- `ag--<project>--<task>--<provider-model>--<suffix>' -- share
    #: long prefixes by construction. An id already declares its own target
    #: type through its sigil and is passed through untouched; `=' in front of
    #: one is not a target at all. See =docs/tmux-session-rename.md=.
    ##
    local socket="${1}" session="${2}"
    test -n "${session}" && [[ "${session}" != '-' ]] || return 1

    local -a reply
    h-agent-subagents-tmux-cmd "${socket}"

    local target="=${session}"
    [[ "${session}" == [\$@%][0-9]## ]] && target="${session}"

    "${reply[@]}" has-session -t "${target}" 2>/dev/null
}

function h-agent-subagents-session-id {
    : "usage: h-agent-subagents-session-id <tmux socket or -> <session name>
Prints that session's current tmux id, on that socket."
    #: Asked of tmux now rather than taken from the registry: the registry's id
    #: is from launch time, and a tmux server restarted since hands out fresh
    #: ones. Here the *name* is the stable key, which is the reverse of the
    #: rule everywhere else in this family and for the same underlying reason
    #: -- the autoname hooks rename everything except `ag--' sessions, and
    #: these are the exception.
    #:
    #: A listing and a string compare rather than `display-message -p -t
    #: "=${session}"', which looks like the direct way to ask and is not: `-t'
    #: there is a *pane* target, and a bare `=name' names no pane, so it
    #: resolves to the empty string and succeeds. Measured: it printed nothing,
    #: with status 0, for a session that was demonstrably alive. Adding `:'
    #: would fix it and is one keystroke away from being forgotten again;
    #: matching the name leaves nothing to parse. [agfi:tmux-session-id] does
    #: the same, and is the version to use when the socket is ours.
    ##
    local socket="${1}" session="${2}"
    assert-args session @RET

    local -a reply
    h-agent-subagents-tmux-cmd "${socket}"

    local line
    for line in ${(f)"$("${reply[@]}" list-sessions -F '#{session_id}'$'\t''#{session_name}' 2>/dev/null)"} ; do
        if [[ "${line#*$'\t'}" == "${session}" ]] ; then
            ec "${line%%$'\t'*}"
            return 0
        fi
    done

    return 1
}

function h-agent-subagents-parent-identity {
    : "usage: h-agent-subagents-parent-identity <parent id>
Sets REPLY to <tmux session id><TAB><transcript><TAB><agent><TAB><tmux name>, each \`-' when unknown."
    #: A parent is named by whatever id it registered under, and that is three
    #: different kinds of string depending on who launched it. A child of the
    #: skill registers under its tmux session name (`ag--...'), which resolves
    #: directly. A top-level agent registers under its *agent session* id,
    #: which the autoname hooks have stored on its tmux session in the
    #: `@agent_session' option. And a coordinator started by hand tends to
    #: invent a label ending in the first field of that id -- which nothing can
    #: resolve exactly, hence the suffix pass. That one is a heuristic and says
    #: so: the name it returns is prefixed with `~'.
    #:
    #: Unresolved is a normal answer and not a failure. An external parent --
    #: one that registered a child without ever being an agent in a tmux
    #: session on this machine -- is a case the registry exists to allow.
    ##
    local parent="${1}"

    local line sid name
    local -a f idf
    local by_name='' by_id='' by_suffix=''

    for line in ${(f)"$(command tmux list-sessions -F '#{session_id}'$'\t''#{session_name}'$'\t''#{@agent_session}' 2>/dev/null)"} ; do
        test -n "${line}" || continue
        f=( "${(@ps:\t:)line}" )
        sid="${f[1]}" ; name="${f[2]}"
        #: The identity option is itself tab separated (agent, session id,
        #: transcript), so it is the tail of the line.
        idf=( "${f[3]:--}" "${f[4]:--}" "${f[5]:--}" )

        if [[ "${name}" == "${parent}" ]] ; then
            by_name="${sid}"$'\t'"${idf[3]}"$'\t'"${idf[1]}"$'\t'"${name}"
        fi
        if [[ "${idf[2]}" != '-' && "${idf[2]}" == "${parent}" ]] ; then
            by_id="${sid}"$'\t'"${idf[3]}"$'\t'"${idf[1]}"$'\t'"${name}"
        fi
        if [[ "${idf[2]}" != '-' && "${parent}" == *-${idf[2]%%-*} ]] ; then
            by_suffix="${sid}"$'\t'"${idf[3]}"$'\t'"${idf[1]}"$'\t'"~${name}"
        fi
    done

    #: Built rather than written into the `:-' default: `$'\t'' is not
    #: interpreted inside a parameter expansion's replacement text, so the
    #: default there arrived as the four literal characters `$'\t''.
    local unknown="-"$'\t'"-"$'\t'"-"$'\t'"-"

    REPLY="${by_name:-${by_id:-${by_suffix:-${unknown}}}}"
}

function h-agent-subagents-parent-rows {
    : "pre-annotation picker rows for the agents that have launched tmux subagents"
    #: The four (here five) column layout [agfi:h-agent-session-annotate-rows]
    #: reads: a caller column, the transcript, the agent, the label, and the
    #: badge that goes ahead of the agent glyph. The caller column is the
    #: parent's *node id* and not a tmux id, because an external parent has no
    #: tmux session of its own, and because the id is what
    #: [agfi:h-agent-subagents-child-rows] filters on.
    #:
    #: Who is offered: with subagents_of_fz_recursive_p off, every agent with at
    #: least one direct child; with it on, every agent with at least one
    #: descendant, which is the larger set -- a coordinator's coordinator has
    #: grandchildren and no children of its own. Offering an agent whose second
    #: picker would be empty is worse than leaving it out, so the two lists
    #: genuinely differ.
    #:
    #: A parent that is itself a registered child carries its own derived
    #: state. One that is not carries 🧑, which is the honest answer: nothing
    #: here knows what a top-level agent is doing, and [agfi:fftmux-agent] is
    #: the picker that does.
    ##
    h-agent-subagents-index @RET

    local recursive_p="${subagents_of_fz_recursive_p:-n}"

    local -A kids desc
    local node parent seg
    local -a segs
    for node in "${sub_nodes[@]}" ; do
        parent="${sub_parent[$node]}"
        if test -n "${parent}" && [[ "${parent}" != '-' && "${parent}" != "${node}" ]] ; then
            (( kids[$parent]++ ))
        fi

        h-agent-subagents-lineage-of "${node}"
        test -n "${REPLY}" || continue
        segs=( ${(s:/:)REPLY} )
        for seg in "${segs[@]}" ; do
            (( desc[$seg]++ ))
        done
    done

    local -a wanted
    local key
    for key in ${(u)${(k)kids}} ${(u)${(k)desc}} ; do
        if bool "${recursive_p}" ; then
            (( ${desc[$key]:-0} )) || continue
        else
            (( ${kids[$key]:-0} )) || continue
        fi
        wanted+=( "${key}" )
    done
    wanted=( ${(u)wanted} )

    (( ${#wanted} )) || return 1

    #: Column widths from the rows themselves, so the label reads as columns
    #: without a fixed width the next long node id would blow past.
    local -i w_id=0
    for key in "${wanted[@]}" ; do
        (( ${#key} > w_id )) && w_id=${#key}
    done

    local out='' state badge color transcript agent sid tname label counts
    local -a idf
    for key in "${wanted[@]}" ; do
        state="${sub_state[$key]}"
        if test -n "${state}" ; then
            h-agent-subagents-state-rank "${state}"
            badge="${REPLY#*$'\t'}"
            h-agent-subagents-state-color "${state}"
            color="${REPLY}"
            transcript="${sub_transcript[$key]:--}"
            agent="${sub_agent[$key]:--}"
            tname="${key}"
        else
            #: Not a registered child: an agent of yours, or something outside
            #: this machine's registry entirely.
            badge='🧑'
            color=$'\e[2m'
            h-agent-subagents-parent-identity "${key}"
            idf=( "${(@ps:\t:)REPLY}" )
            sid="${idf[1]}" ; transcript="${idf[2]}" ; agent="${idf[3]}" ; tname="${idf[4]}"
        fi

        counts="${kids[$key]:-0} direct"
        (( ${desc[$key]:-0} != ${kids[$key]:-0} )) && counts+=", ${desc[$key]:-0} total"

        label="${(r:${w_id}:)key}  ${color}${counts}${agent_subagents_color_reset}"
        #: The tmux name only when it adds something: for a registered child
        #: it *is* the node id, already the first column.
        test -n "${tname}" && [[ "${tname}" != '-' && "${tname}" != "${key}" ]] && label+="  ${tname}"
        test -n "${sub_task[$key]}" && [[ "${sub_task[$key]}" != '-' ]] && label+="  ${sub_task[$key]}"

        out+="${key}"$'\t'"${transcript:--}"$'\t'"${agent:--}"$'\t'"${label}"$'\t'"${badge}"$'\n'
    done

    ec "${out%$'\n'}"
}

function h-agent-subagents-child-rows {
    : "usage: h-agent-subagents-child-rows <parent node id>
Pre-annotation picker rows for that agent's subagents."
    #: subagents_of_fz_recursive_p=y takes the whole subtree instead of the
    #: direct children, matched on `lineage'; each row then opens with its
    #: depth, `d1' being a direct child.
    #:
    #: What the label carries, in the order the columns answer "which one did I
    #: mean": the node id, what it was launched as (provider, launcher, and the
    #: model asked for), the state derived now, whether its tmux session is
    #: still there, its task id, and when it was created. The state is the
    #: derived one and never the registry's `process_state'; see
    #: [agfi:h-agent-subagents-registry-meta] for why that field is not even
    #: offered.
    #:
    #: A `gone' child is kept, unlike in [agfi:h-agent-session-subagent-rows]
    #: which drops it. This is the one picker where "you launched a child and
    #: its session is gone" is an answer worth printing: the act step then says
    #: where the task directory is instead of failing at a missing target.
    ##
    local parent="${1}"
    assert-args parent @RET

    h-agent-subagents-index @RET

    local recursive_p="${subagents_of_fz_recursive_p:-n}"

    local -a wanted
    local -A depth_of
    local node
    for node in "${sub_nodes[@]}" ; do
        if bool "${recursive_p}" ; then
            h-agent-subagents-depth-under "${node}" "${parent}" || continue
            depth_of[$node]="${REPLY}"
        else
            [[ "${sub_parent[$node]}" == "${parent}" ]] || continue
            depth_of[$node]=1
        fi
        wanted+=( "${node}" )
    done

    (( ${#wanted} )) || return 1

    local -i w_id=0 w_launch=0 w_state=0
    local -A launch_of
    local launch
    for node in "${wanted[@]}" ; do
        launch="${sub_provider[$node]:--}/${sub_launcher[$node]:--}"
        test -n "${sub_model[$node]}" && [[ "${sub_model[$node]}" != '-' ]] && launch+=" ${sub_model[$node]}"
        launch_of[$node]="${launch}"

        (( ${#node} > w_id )) && w_id=${#node}
        (( ${#launch} > w_launch )) && w_launch=${#launch}
        (( ${#sub_state[$node]} > w_state )) && w_state=${#sub_state[$node]}
    done

    local out='' badge color state alive label
    for node in "${wanted[@]}" ; do
        state="${sub_state[$node]:-unknown}"

        h-agent-subagents-state-rank "${state}"
        badge="${REPLY#*$'\t'}"
        h-agent-subagents-state-color "${state}"
        color="${REPLY}"

        if h-agent-subagents-alive-p "${sub_socket[$node]}" "${node}" ; then
            alive='alive'
        else
            alive='dead '
        fi

        label=''
        bool "${recursive_p}" && label+="d${depth_of[$node]} "
        label+="${(r:${w_id}:)node}  ${(r:${w_launch}:)${launch_of[$node]}}"
        label+="  ${color}${(r:${w_state}:)state}${agent_subagents_color_reset} ${alive}"
        label+="  ${sub_task[$node]:--}  ${sub_created[$node]:--}"

        out+="${node}"$'\t'"${sub_transcript[$node]:--}"$'\t'"${sub_agent[$node]:--}"$'\t'"${label}"$'\t'"${badge}"$'\n'
    done

    ec "${out%$'\n'}"
}

function agent-subagents-parent-preview {
    : "usage: agent-subagents-parent-preview <parent node id> [-r]
Plain-text summary of one agent and the subagents it launched, for an fzf preview."
    #: `-r' as an argument rather than through the knob, because fzf runs a
    #: preview in its own `sh' and hands it to the garden: a dynamically scoped
    #: zsh variable does not survive that trip.
    ##
    local parent="${1}"
    assert-args parent @RET
    local recursive_p=n
    [[ "${2}" == -r ]] && recursive_p=y

    local agent_subagents_index_loaded=''
    local -a sub_nodes
    local -A sub_state sub_outcome sub_sid sub_pane sub_task sub_parent
    local -A sub_kids sub_when sub_epoch sub_workdir sub_transcript sub_agent
    local -A sub_lineage sub_provider sub_launcher sub_model sub_created
    local -A sub_socket sub_resume

    h-agent-subagents-index @RET

    ec "agent        ${parent}"

    if test -n "${sub_state[$parent]}" ; then
        ec "kind         a registered subagent itself"
        ec "state        ${sub_state[$parent]}"
        ec "task         ${sub_task[$parent]:--}"
        ec "workdir      ${sub_workdir[$parent]:--}"
        ec "parent       ${sub_parent[$parent]:--}"
    else
        local -a idf
        h-agent-subagents-parent-identity "${parent}"
        idf=( "${(@ps:\t:)REPLY}" )
        ec "kind         not a registered subagent itself"
        ec "tmux         ${idf[1]}  ${idf[4]}"
    fi

    local -a kids all
    local node
    for node in "${sub_nodes[@]}" ; do
        [[ "${sub_parent[$node]}" == "${parent}" ]] && kids+=( "${node}" )
        h-agent-subagents-depth-under "${node}" "${parent}" && all+=( "${node}" )
    done

    ec "children     ${#kids} direct, ${#all} in the subtree"
    ec ''

    local -a show
    if bool "${recursive_p}" ; then
        show=( "${all[@]}" )
        ec '--- subtree ---'
    else
        show=( "${kids[@]}" )
        ec '--- direct children ---'
    fi

    if ! (( ${#show} )) ; then
        ec '(none)'
        return 0
    fi

    local depth
    for node in "${show[@]}" ; do
        depth=1
        h-agent-subagents-depth-under "${node}" "${parent}" && depth="${REPLY}"
        print -r -- "d${depth}  ${(r:12:)${sub_state[$node]:-unknown}}  ${node}  ${sub_task[$node]:--}"
    done
}

function h-agent-subagents-attach-readonly {
    : "usage: h-agent-subagents-attach-readonly <tmux session id>
Attaches to that session read-only, in a popup when we are already inside tmux."
    #: Read-only is a property of a *client*, and attaching cannot nest, so
    #: from inside tmux this has to be a new client rather than a switch:
    #: `display-popup -E' with TMUX unset gives one over the current pane, and
    #: closing it leaves everything as it was.
    #:
    #: Not `switch-client -r', which is the obvious-looking answer and the
    #: wrong one: it *toggles* the flag on the client you are sitting in, so it
    #: would hand you a read-only terminal of your own and leave it that way
    #: after you came back.
    ##
    local target="${1}"
    assert-args target @RET

    if isTmux ; then
        command tmux display-popup -E -w 95% -h 95% \
            "env -u TMUX tmux attach-session -r -t $(gquote-sq "${target}")"
    else
        command tmux attach-session -r -t "${target}"
    fi
}

function h-subagents-of-fz-act {
    : "usage: h-subagents-of-fz-act <read-only?> <node id>...
Goes to each child's tmux session, or says where its state lives when there is none."
    #: A gone child is why this is not simply [agfi:h-fftmux-act]: that one is
    #: handed a session id and would fail on a session that no longer exists,
    #: silently, behind its `2>/dev/null'. What a person actually wants then is
    #: the task directory -- the brief, the checkpoints and the result outlive
    #: the tmux session by design -- and the resume state, which is what
    #: `prefix-r' would have respawned from.
    ##
    local readonly_p="${1}"
    shift

    local agent_subagents_index_loaded=''
    local -a sub_nodes
    local -A sub_state sub_outcome sub_sid sub_pane sub_task sub_parent
    local -A sub_kids sub_when sub_epoch sub_workdir sub_transcript sub_agent
    local -A sub_lineage sub_provider sub_launcher sub_model sub_created
    local -A sub_socket sub_resume

    h-agent-subagents-index @RET

    local state_dir
    state_dir="$(h-agent-subagents-state-dir)" @RET

    local -a ftE
    bool "${readonly_p}" && ftE=( h-agent-subagents-attach-readonly )

    #: Which tmux server *we* would attach to. Empty when none is running, in
    #: which case nothing can be on the wrong one either.
    local our_socket
    our_socket="$(command tmux display-message -p '#{socket_path}' 2>/dev/null)" || our_socket=''

    local node sid
    local -i ret=0
    for node in "$@" ; do
        test -n "${node}" || continue

        if ! h-agent-subagents-alive-p "${sub_socket[$node]}" "${node}" ; then
            ecerr "$0: ${node}: its tmux session is gone. What it left behind:"
            if test -n "${sub_task[$node]}" && [[ "${sub_task[$node]}" != '-' ]] ; then
                ecerr "  task      ${state_dir}/tasks/${sub_task[$node]}"
            elif test -z "${sub_state[$node]}" ; then
                #: Not merely gone but forgotten: [agfi:agent-subagents-reconcile]
                #: drops an entry whose session has disappeared, and a picker
                #: started before that ran still has the row.
                ecerr "  the registry has no entry for it any more either"
            fi
            if test -n "${sub_resume[$node]}" && [[ "${sub_resume[$node]}" != '-' ]] ; then
                ecerr "  resume    ${sub_resume[$node]}"
            fi
            ecerr "  registry  $(h-agent-subagents-registry)"
            ret=1
            continue
        fi

        sid="$(h-agent-subagents-session-id "${sub_socket[$node]}" "${node}")" || sid=''
        if test -z "${sid}" ; then
            ecerr "$0: ${node}: its tmux session exists but has no resolvable id"
            ret=1
            continue
        fi

        #: Everything above addresses the entry's own socket; everything below
        #: -- [agfi:h-fftmux-act] through [agfi:tmux-session-goto] -- addresses
        #: ours, because attaching is a thing a *client* does and our client is
        #: on our server. Normally they are the same socket and the question
        #: never arises. When they are not, say so and hand over the command
        #: rather than attaching to whatever happens to carry that id here: two
        #: servers number their sessions independently, so the same `$N' on the
        #: wrong socket is a different session entirely.
        if test -n "${sub_socket[$node]}" && [[ "${sub_socket[$node]}" != '-' && "${sub_socket[$node]}" != "${our_socket}" ]] ; then
            ecerr "$0: ${node} lives on another tmux server (${sub_socket[$node]}), not ours (${our_socket:-none})"
            ecerr "  attach it yourself with:"
            ecerr "    env -u TMUX tmux -S ${(q-)sub_socket[$node]} attach-session -t ${(q-)sid}"
            ret=1
            continue
        fi

        h-fftmux-act "${sid}" || ret=$?
    done

    return ${ret}
}

function subagents-of-fz {
    : "usage: subagents-of-fz [-r|--recursive] [<agent query>]
Picks an agent that has launched tmux subagents, then one of its subagents to attach to."
    #: The first picker takes `--select-1' when a query is given, so naming the
    #: agent skips the dialogue. The second never takes it, which is the whole
    #: point of the command: reading what a child is doing before landing in it
    #: matters most when there is exactly one, and `--select-1' would be
    #: precisely the case that skipped the look.
    #:
    #: Enter goes to the child's session -- switching the client when we are
    #: already inside tmux, through [agfi:h-fftmux-act] like every other picker
    #: here -- and ctrl-r attaches read-only. The key comes back through fzf's
    #: `--expect' rather than through a binding, so the choice is made in the
    #: picker and acted on in the shell that called it, which is the only place
    #: that can attach a terminal to anything.
    #:
    #: The preview is [agfi:agent-subagents-preview] and never the fftmux
    #: family's Go transcript renderer: what the registry and the pane say is
    #: enough to choose between children, and a picker over a fan-out should
    #: not page other conversations' transcripts past you to get there.
    ##
    local recursive_p="${subagents_of_fz_recursive_p:-n}"
    local -a args
    while (( $# )) ; do
        case "${1}" in
            -h|--help)
                ec 'usage: subagents-of-fz [-r|--recursive] [<agent query>]'
                ec ''
                ec 'Two pickers. The first offers every agent that has launched tmux'
                ec 'subagents; a query is matched against it with --select-1, so naming'
                ec 'the agent skips that dialogue. The second offers that agent'\''s'
                ec 'children and never skips, however few there are.'
                ec ''
                ec '  -r, --recursive   the whole subtree rather than the direct children,'
                ec '                    with a depth column (d1 is a direct child)'
                ec '  -h, --help        this'
                ec ''
                ec 'In the second picker: enter goes to the child'\''s tmux session, ctrl-r'
                ec 'attaches to it read-only. A child whose session is gone prints where'
                ec 'its task directory and resume state live instead of failing.'
                ec ''
                ec 'Knobs: subagents_of_fz_recursive_p, subagents_of_fz_sort,'
                ec 'agent_subagents_preview_pane_lines.'
                return 0
                ;;
            -r|--recursive) recursive_p=y ; shift ;;
            --) shift ; args+=( "$@" ) ; break ;;
            *) args+=( "${1}" ) ; shift ;;
        esac
    done
    local query="${(j: :)args}"

    bella_zsh_disable1

    #: One live listing and one pass over the registry for both pickers and for
    #: the act step; `local' is dynamically scoped, so everything below sees
    #: these.
    local agent_session_live_list_cache="${agent_session_live_list_cache:-$(h-agent-session-live-list)}"
    local subagents_of_fz_recursive_p="${recursive_p}"
    local agent_subagents_index_loaded=''
    local -a sub_nodes
    local -A sub_state sub_outcome sub_sid sub_pane sub_task sub_parent
    local -A sub_kids sub_when sub_epoch sub_workdir sub_transcript sub_agent
    local -A sub_lineage sub_provider sub_launcher sub_model sub_created
    local -A sub_socket sub_resume

    local parent_rows
    if ! parent_rows="$(h-agent-subagents-parent-rows)" ; then
        if bool "${recursive_p}" ; then
            ecerr "$0: no agent in the registry has any subagent in its subtree"
        else
            ecerr "$0: no agent in the registry has a direct subagent (-r also counts grandchildren)"
        fi
        return 1
    fi

    local sort_by="${subagents_of_fz_sort:-last}"
    local brishzq
    brishzq="$(gquote "${commands[brishzq.zsh]:-brishzq.zsh}")" @RET

    local recursive_arg=''
    bool "${recursive_p}" && recursive_arg=' -r'

    #: A dash-compatible preview line, since fzf runs it in its own `sh'.
    local -a pick_opts
    pick_opts=(
        --prompt 'agent> '
        --preview "${brishzq} agent-subagents-parent-preview {1}${recursive_arg}"
    )
    #: `--select-1' only here, and only with something to match on.
    #: `--exit-0' comes from [agfi:fz] already.
    test -n "${query}" && pick_opts+=( --query "${query}" --select-1 )

    local agent_session_fz_header='enter: list this agent'\''s subagents'

    local parent_pick
    parent_pick="$(ec "${parent_rows}" |
        agent_session_rows_sort="${sort_by}" h-agent-session-annotate-rows |
        h-agent-session-fz no-multi "${pick_opts[@]}")" || parent_pick=''

    local parent="${parent_pick%%$'\t'*}"
    if test -z "${parent}" ; then
        ecerr "$0: no agent picked"
        return 1
    fi

    local child_rows
    if ! child_rows="$(h-agent-subagents-child-rows "${parent}")" ; then
        if bool "${recursive_p}" ; then
            ecerr "$0: ${parent} has no subagent anywhere in its subtree"
        else
            ecerr "$0: ${parent} has no direct subagent (-r also counts grandchildren)"
        fi
        return 1
    fi

    agent_session_fz_header='enter: go to it, ctrl-r: attach read-only'

    local child_pick
    child_pick="$(ec "${child_rows}" |
        agent_session_rows_sort="${sort_by}" h-agent-session-annotate-rows |
        h-agent-session-fz no-multi \
            --prompt "subagents of ${parent}> " \
            --expect=ctrl-r \
            --preview "${brishzq} agent-subagents-preview --pane-lines 40 {1}")" || child_pick=''

    #: `--expect' puts the key that ended fzf on the first line -- empty for
    #: enter -- so the row is the second. `(@f)' keeps that empty field, which
    #: plain `${(f)...}' would drop and shift everything up by one.
    local -a lines
    lines=( "${(@f)child_pick}" )

    local key="${lines[1]}" row="${lines[2]}"
    if test -z "${row}" ; then
        ecerr "$0: no subagent picked"
        return 1
    fi

    local readonly_p=n
    [[ "${key}" == ctrl-r ]] && readonly_p=y

    h-subagents-of-fz-act "${readonly_p}" "${row%%$'\t'*}"
}
##
