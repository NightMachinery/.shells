##
#: Cleaning up the tmux subagents the `tmux-subagents' skill launches.
#:
#: The skill (=~/code/skills/tmux-subagents=) starts child agents in detached
#: tmux sessions named `ag--<project>--<run>--<lineage>--<model>--<role>' and
#: registers each in a JSON file keyed by node id, which is also the tmux
#: session name. Its `process_state' and `task_outcome' fields are written once
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
        if [[ "${st}" != (gone|exited) ]] ; then
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
    : "usage: agent-subagents-preview <node-id>
Plain-text summary of one subagent, for an fzf preview."
    #: Never `eval', never a shell expansion of anything captured: the pane
    #: holds whatever the child printed. The capture is taken without `-e', so
    #: no escape sequences are requested in the first place, and the control
    #: bytes an agent TUI writes anyway are deleted rather than rendered --
    #: a preview that interprets them can be made to rewrite the screen.
    ##
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

    if [[ "${pane}" != '-' ]] ; then
        ec ''
        ec "--- pane ${pane} ---"
        command tmux capture-pane -p -t "${pane}" -S -25 2>/dev/null |
            command tr -d '\000-\010\013\014\016-\037\177'
    fi

    return 0
}

function h-agent-subagents-state-rank {
    : "usage: h-agent-subagents-state-rank <state>
Sets REPLY to the picker's sort rank for that state, and to its emoji."
    #: The order the picker offers things in, and the reason for it: what is
    #: finished and safe to close first, what is broken next, then what is
    #: waiting on a person, then the live ones from most abandoned to least.
    #: `busy' comes last and is normally not shown at all.
    ##
    case "${1}" in
        done)        REPLY=$'1\t✅' ;;
        mismatch)    REPLY=$'2\t❌' ;;
        needs-input) REPLY=$'3\t❓' ;;
        stuck)       REPLY=$'4\t🪦' ;;
        idle)        REPLY=$'5\t💤' ;;
        unknown)     REPLY=$'6\t❔' ;;
        busy)        REPLY=$'7\t⏳' ;;
        *)           REPLY=$'8\t❔' ;;
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
