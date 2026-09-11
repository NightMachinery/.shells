##
#: `/auto-continue': a session arms itself to be resumed when its usage limit
#: resets, for Claude Code, Codex and Antigravity alike. See
#: =docs/agent-auto-continue.md=.
#:
#: The skill is one tracked file, =configFiles/agent-skills/auto-continue/=,
#: symlinked into each agent's skills directory by [agfi:agent-skills-link]
#: exactly as `/done' is. It runs [agfi:agent-auto-continue-on] from the
#: agent's own shell tool, which is the one place that knows which session
#: this is and how a resume can reach it.
#:
#: Everything after that is the armed engine in =agent-usage.zsh=. What this
#: file adds is the part the engine deliberately leaves to a person -- WHEN to
#: arm and WHAT to resume -- done for a registered session by a watcher:
#:
#:   - A registration is one file per session under a *scope*, the account
#:     whose limit the session shares: a Claude seat (`claude-work'), a Codex
#:     auth (`codex-<alias>'), or Antigravity (`agy'). It records how the
#:     resume reaches the session, in the target syntax
#:     [agfi:h-agent-usage-continue-send] takes.
#:   - One watcher tmux session per scope polls that scope's usage. When the
#:     scope is blocked it calls the scope's own deadline source
#:     ([agfi:h-claude-code-usage-arm], [agfi:h-codex-status-arm-auth],
#:     [agfi:h-agy-status-arm]) with every registration's target preset, so
#:     the picker is skipped and all of them resume at the reset. It prunes
#:     registrations whose session has ended and exits when none is left.
#:   - Where an agent has a hook that sees the failed turn -- Claude Code's
#:     `StopFailure', Antigravity's `Stop' -- the hook only makes the check
#:     run sooner. Codex fires nothing on a usage-limit turn, so there the poll
#:     is the whole of the detection.
#:
#: A watcher's armed jobs are ordinary engine jobs: [agfi:agent-usage-armed-status]
#: lists them, [agfi:agent-usage-armed-cancel] cancels them, and the idle
#: gate, grace and log apply unchanged.
##
#: How often a watcher re-reads its scope's usage.
typeset -g agent_auto_continue_poll_s="${agent_auto_continue_poll_s:-300}"
#: The least time between two hook-triggered checks of one scope, so a burst
#: of failed turns costs one usage call rather than one each.
typeset -g agent_auto_continue_kick_min_s="${agent_auto_continue_kick_min_s:-60}"
#: The Claude Code windows every registration is checked against; the
#: session's own model-scoped weekly window is added per session
#: ([agfi:h-agent-auto-continue-claude-family]).
typeset -g agent_auto_continue_claude_roles="${agent_auto_continue_claude_roles:-session weekly_all}"
#: The tmux sessions this file creates all start with this; the watchers are
#: `<prefix>-watch-<scope>', the armed jobs `<prefix>-<scope>[-<group>]'.
typeset -g agent_auto_continue_session_prefix="${agent_auto_continue_session_prefix:-agent-auto-continue}"
##
#: Naming
##
function h-agent-auto-continue-dir {
    #: Where registrations live: beside the kitty-window registry rather than
    #: under `~/tmp', for the reason [agfi:h-agent-session-registry-dir] gives.
    ec "${agent_auto_continue_dir:-$(h-agent-session-registry-dir)/auto-continue}"
}

function h-agent-auto-continue-scope {
    #: The scope this shell's session belongs to: the account whose limit it
    #: shares. Read from the environment the agent exports, so it is the
    #: session's own answer and not a guess from a listing.
    #: Usage: h-agent-auto-continue-scope <agent>
    ##
    local agent="${1}"
    assert-args agent @RET

    case "${agent}" in
        claude)
            local profile
            profile="$(claude-code-profile-current)" @RET
            ec "claude-${profile}"
            ;;
        codex)
            local auth_alias
            auth_alias="$(h-codex-status-active-alias 2>/dev/null)" || auth_alias=auth
            ec "codex-${auth_alias}"
            ;;
        agy)
            ec agy
            ;;
        *)
            ecerr "$0: unknown agent: ${agent}"
            return 1
            ;;
    esac
}

function h-agent-auto-continue-scope-agent {
    #: The agent a scope name belongs to: the part before the first `-'.
    local scope="${1}"
    assert-args scope @RET

    ec "${scope%%-*}"
}

function h-agent-auto-continue-file {
    #: Usage: h-agent-auto-continue-file <scope> <agent> <id>
    local scope="${1}" agent="${2}" id="${3}"
    assert-args scope agent id @RET

    ec "$(h-agent-auto-continue-dir)/${scope}/${agent}-${id}"
}

function h-agent-auto-continue-watch-session {
    #: The tmux session a scope's watcher runs in.
    local scope="${1}"
    assert-args scope @RET

    h-tmux-session-name-sanitize "${agent_auto_continue_session_prefix}-watch-${scope}"
}

function h-agent-auto-continue-job-session {
    #: The tmux session a scope's armed job lives in. Claude Code gets one per
    #: model family, since a model-scoped weekly window blocks only the
    #: sessions on that model; see [agfi:agent-auto-continue-check].
    #: Usage: h-agent-auto-continue-job-session <scope> [group]
    ##
    local scope="${1}" group="${2}"
    assert-args scope @RET

    h-tmux-session-name-sanitize "${agent_auto_continue_session_prefix}-${scope}${group:+-${group}}"
}

function h-agent-auto-continue-tmux-sessions {
    #: The tmux sessions with our prefix, armed jobs or watchers as $1 says
    #: (`jobs' or `watchers'), narrowed to one scope when $2 is given.
    #: Usage: h-agent-auto-continue-tmux-sessions <jobs|watchers> [scope]
    ##
    local kind="${1}" scope="${2}"
    assert-args kind @RET

    local prefix="${agent_auto_continue_session_prefix}"
    local watch_prefix="${prefix}-watch-"

    local -a names
    names=( ${(f)"$(command tmux list-sessions -F '#{session_name}' 2>/dev/null)"} )

    local n
    for n in "${names[@]}" ; do
        case "${kind}" in
            watchers)
                [[ "${n}" == "${watch_prefix}"* ]] || continue
                if test -n "${scope}" ; then
                    [[ "${n}" == "$(h-agent-auto-continue-watch-session "${scope}")" ]] || continue
                fi
                ;;
            jobs)
                [[ "${n}" == "${prefix}-"* ]] || continue
                [[ "${n}" == "${watch_prefix}"* ]] && continue
                if test -n "${scope}" ; then
                    local base
                    base="$(h-agent-auto-continue-job-session "${scope}")"
                    [[ "${n}" == "${base}" || "${n}" == "${base}-"* ]] || continue
                fi
                ;;
            *)
                ecerr "$0: unknown kind: ${kind} (jobs, watchers)"
                return 1
                ;;
        esac

        ec "${n}"
    done
}

function agent-auto-continue-armed-sessions {
    #: Every tmux session an auto-continue armed job can live in, one per line,
    #: for [agfi:agent-usage-armed-sessions]. Listed from tmux rather than
    #: generated from a table, because the set depends on what is registered.
    ##
    h-agent-auto-continue-tmux-sessions jobs
}
##
#: Registrations
##
function h-agent-auto-continue-registrations {
    #: The registrations of one scope, one per line, tab separated: agent, id,
    #: transcript, targets (space separated), registered-at epoch.
    #: Usage: h-agent-auto-continue-registrations <scope>
    ##
    setopt localoptions bareglobqual

    local scope="${1}"
    assert-args scope @RET

    local dir
    dir="$(h-agent-auto-continue-dir)/${scope}"
    test -d "${dir}" || return 0

    local f
    for f in "${dir}"/*(N.) ; do
        command cat -- "${f}" 2>/dev/null
    done
}

function h-agent-auto-continue-scopes {
    #: Every scope with at least one registration, one per line. A directory
    #: left behind with only its `.kicked' stamp is not a scope anyone is
    #: waiting on.
    setopt localoptions bareglobqual

    local d
    local -a regs
    for d in "$(h-agent-auto-continue-dir)"/*(N/) ; do
        regs=( "${d}"/*(N.) )
        (( ${#regs} )) || continue
        ec "${d:t}"
    done
}

function h-agent-auto-continue-live-p {
    #: Whether a registration's session is still running, against the live
    #: listing in `agent_auto_continue_live_list' (dynamically scoped, so a
    #: caller lists once for many). The id is in column 2 of the listing and
    #: the transcript in column 5; either match counts, since Claude Code's
    #: live listing knows the id before the transcript exists.
    #: Usage: h-agent-auto-continue-live-p <id> <transcript>
    ##
    local id="${1}" transcript="${2}"

    local list="${agent_auto_continue_live_list}"
    test -n "${list}" || return 1

    local row
    local -a f
    for row in ${(f)list} ; do
        f=( "${(@ps:\t:)row}" )
        if test -n "${id}" && [[ "${f[2]}" == "${id}" ]] ; then
            return 0
        fi
        if test -n "${transcript}" && [[ "${f[5]}" == "${transcript}" ]] ; then
            return 0
        fi
    done

    return 1
}

function h-agent-auto-continue-prune {
    #: Drops the registrations of one scope whose session is gone, and prints
    #: the ones kept. A session that ended has nothing to resume, and leaving
    #: its target in place would have the job type into whatever replaced it
    #: -- the engine refuses that too, but the notification would still name
    #: it as a failure.
    #: Usage: h-agent-auto-continue-prune <scope>
    ##
    setopt localoptions bareglobqual

    local scope="${1}"
    assert-args scope @RET

    local dir
    dir="$(h-agent-auto-continue-dir)/${scope}"
    test -d "${dir}" || return 0

    local agent_auto_continue_live_list="${agent_auto_continue_live_list:-$(h-agent-session-live-list 2>/dev/null)}"

    local f line
    local -a fields
    for f in "${dir}"/*(N.) ; do
        line="$(command cat -- "${f}" 2>/dev/null)"
        fields=( "${(@ps:\t:)line}" )

        if h-agent-auto-continue-live-p "${fields[2]}" "${fields[3]}" ; then
            ec "${line}"
        else
            ecgray "$0: ${scope}: ${f:t} is no longer live, forgetting it"
            command rm -f -- "${f}" 2>/dev/null || true
        fi
    done
}
##
#: This session
##
function h-agent-auto-continue-targets {
    #: How a resume reaches THIS session, one target per line, in the syntax of
    #: [agfi:h-agent-usage-continue-send]. In order of how little they need:
    #: a Codex thread is queued by id and needs no terminal at all; a tmux
    #: pane needs no window manager, focus or awake display; a kitty window
    #: needs kitty's remote control. `frontmost' -- blind typing into whatever
    #: holds the keyboard -- is never chosen here; `--frontmost' asks for it.
    #: Usage: h-agent-auto-continue-targets <agent> <id> <transcript>
    ##
    local frontmost_p="${agent_auto_continue_frontmost_p:-n}"

    local agent="${1}" id="${2}" transcript="${3}"
    assert-args agent id @RET

    if bool "${frontmost_p}" ; then
        ec frontmost
        return 0
    fi

    if [[ "${agent}" == codex ]] ; then
        ec "codex:${id}"
        return 0
    fi

    local -a tried
    local rows
    if test -n "${TMUX_PANE}" ; then
        #: The pane is ours by construction -- this shell runs inside it --
        #: but it must hold a *live agent* for the engine to type into it
        #: later, and asking now is what makes a "no" readable.
        rows="$(h-agent-session-tmux-panes 2>/dev/null)" || rows=''
        if [[ $'\n'"${rows}" == *$'\n'"${TMUX_PANE}"$'\t'* ]] ; then
            ec "tmux:${TMUX_PANE}"
            return 0
        fi
        tried+=( "tmux pane ${TMUX_PANE} is not listed as holding a live agent session" )
    else
        tried+=( "not inside tmux" )
    fi

    if test -n "${transcript}" ; then
        rows="$(h-agent-session-live-pairs 2>/dev/null)" || rows=''
        local row
        local -a f
        for row in ${(f)rows} ; do
            f=( "${(@ps:\t:)row}" )
            if [[ "${f[2]}" == "${transcript}" ]] && test -n "${f[1]}" && [[ "${f[1]}" != '-' ]] ; then
                ec "kitty:${f[1]}"
                return 0
            fi
        done
        tried+=( "no kitty window shows this session" )
    else
        tried+=( "no transcript yet, so no kitty window can be matched" )
    fi

    ecerr "$0: no way to reach this session: ${(j:; :)tried}. Run with --frontmost to type wherever the keyboard focus is at the time."
    return 1
}

function h-agent-auto-continue-here {
    #: This shell's agent, session id, transcript and scope, tab separated.
    #: The shared front half of `on', `off' and `status'.
    ##
    local agent
    agent="$(ai-agent-name)" || {
        ecerr "$0: not inside an agent session"
        return 1
    }

    local id
    id="$(h-agent-session-call "${agent}" current-id)" @RET

    local transcript
    transcript="$(h-agent-session-call "${agent}" resolve "${id}" 2>/dev/null | command head -n1)" || transcript=''

    local scope
    scope="$(h-agent-auto-continue-scope "${agent}")" @RET

    printf '%s\t%s\t%s\t%s\n' "${agent}" "${id}" "${transcript}" "${scope}"
}

function agent-auto-continue-on {
    : "usage: agent-auto-continue-on [--frontmost]
Registers the calling agent session to be resumed when its usage limit resets,
and starts the watcher for its scope. Meant to be run by the shared
/auto-continue skill from the agent's own shell tool."
    ##
    local poll_s="${agent_auto_continue_poll_s:-300}"
    local frontmost_p=n

    while (( $# )) ; do
        case "${1}" in
            --frontmost) frontmost_p=y ; shift ;;
            --) shift ; break ;;
            -*)
                ecerr "$0: unknown option: ${1}"
                return 1
                ;;
            *) break ;;
        esac
    done

    zmodload zsh/datetime 2>/dev/null

    local here
    here="$(h-agent-auto-continue-here)" @RET
    local -a h
    h=( "${(@ps:\t:)here}" )
    local agent="${h[1]}" id="${h[2]}" transcript="${h[3]}" scope="${h[4]}"

    local -a targets
    targets=( ${(f)"$(agent_auto_continue_frontmost_p="${frontmost_p}" h-agent-auto-continue-targets "${agent}" "${id}" "${transcript}")"} ) @RET
    (( ${#targets} )) || return 1

    local file
    file="$(h-agent-auto-continue-file "${scope}" "${agent}" "${id}")" @RET
    mkdir -p -- "${file:h}" @RET
    #: Sorted, so the same set always spells the same string: the check
    #: compares it against what the armed job recorded to decide whether to
    #: re-arm.
    printf '%s\t%s\t%s\t%s\t%s\n' "${agent}" "${id}" "${transcript}" "${(j: :)${(o)targets}}" "${EPOCHSECONDS}" > "${file}" @RET

    h-agent-auto-continue-watch-ensure "${scope}" @RET

    ec "auto-continue on: ${scope}, resumes ${(j:, :)targets} when the limit resets; the watcher polls every $(seconds-fmt-short "${poll_s}")"

    #: Once, now: usually a gray "usage already possible" line, which doubles
    #: as proof the scope's usage can be read at all.
    agent-auto-continue-check "${scope}" >&2 || true
}

function agent-auto-continue-off {
    : "usage: agent-auto-continue-off
Forgets the calling session's registration; with nothing left in its scope,
cancels the scope's armed job and stops its watcher."
    ##
    local here
    here="$(h-agent-auto-continue-here)" @RET
    local -a h
    h=( "${(@ps:\t:)here}" )
    local agent="${h[1]}" id="${h[2]}" scope="${h[4]}"

    local file
    file="$(h-agent-auto-continue-file "${scope}" "${agent}" "${id}")" @RET
    if ! test -e "${file}" ; then
        ec "auto-continue was not on for this session"
        return 0
    fi
    command rm -f -- "${file}" @RET

    #: With others left, this re-arms an armed job without our target; with
    #: none left, it cancels the job.
    agent-auto-continue-check "${scope}" >&2 || true

    local left
    left="$(h-agent-auto-continue-registrations "${scope}")"
    if test -z "${left}" ; then
        h-agent-auto-continue-watch-stop "${scope}" >&2 || true
        #: The kick stamp and the directory go too, so an emptied scope leaves
        #: nothing behind for [agfi:agent-auto-continue-list] to show.
        command rm -f -- "${file:h}/.kicked" 2>/dev/null || true
        command rmdir -- "${file:h}" 2>/dev/null || true
        ec "auto-continue off: ${scope} has no registered session left; its watcher is stopped"
    else
        ec "auto-continue off for this session; ${scope} still has $(ec "${left}" | command wc -l | tr -d ' ') registered"
    fi
}

function agent-auto-continue-status {
    : "usage: agent-auto-continue-status
This session's registration, its scope's watcher, and the scope's armed jobs."
    ##
    local here
    here="$(h-agent-auto-continue-here)" @RET
    local -a h
    h=( "${(@ps:\t:)here}" )
    local agent="${h[1]}" id="${h[2]}" scope="${h[4]}"

    local file
    file="$(h-agent-auto-continue-file "${scope}" "${agent}" "${id}")" @RET
    if test -e "${file}" ; then
        local -a f
        f=( "${(@ps:\t:)$(command cat -- "${file}")}" )
        ec "this session: on (${scope}, targets: ${f[4]})"
    else
        ec "this session: off (${scope})"
    fi

    h-agent-auto-continue-scope-status "${scope}"
}

function h-agent-auto-continue-scope-status {
    #: One scope: its watcher and its armed jobs.
    local scope="${1}"
    assert-args scope @RET

    local watch
    watch="$(h-agent-auto-continue-watch-session "${scope}")"
    if tmux-alive-p "${watch}" ; then
        ec "watcher: running (${watch})"
    else
        ec "watcher: not running"
    fi

    local -a jobs
    jobs=( ${(f)"$(h-agent-auto-continue-tmux-sessions jobs "${scope}")"} )
    if (( ${#jobs} )) ; then
        h-agent-usage-armed-status "${jobs[@]}"
    else
        ec "armed job: none"
    fi
}

function agent-auto-continue-list {
    : "usage: agent-auto-continue-list [--prune]
Every registered session, grouped by scope and marked live or dead, then the
watchers and armed jobs. --prune forgets the dead ones."
    ##
    local prune_p=n
    while (( $# )) ; do
        case "${1}" in
            --prune) prune_p=y ; shift ;;
            *)
                ecerr "$0: unknown option: ${1}"
                return 1
                ;;
        esac
    done

    local -a scopes
    scopes=( ${(f)"$(h-agent-auto-continue-scopes)"} )
    if (( ${#scopes} == 0 )) ; then
        ecgray "$0: nothing registered"
    fi

    local agent_auto_continue_live_list
    agent_auto_continue_live_list="$(h-agent-session-live-list 2>/dev/null)"

    local scope line state
    local -a f
    for scope in "${scopes[@]}" ; do
        ecbold "${scope}"
        if bool "${prune_p}" ; then
            h-agent-auto-continue-prune "${scope}" >/dev/null
        fi
        for line in ${(f)"$(h-agent-auto-continue-registrations "${scope}")"} ; do
            f=( "${(@ps:\t:)line}" )
            if h-agent-auto-continue-live-p "${f[2]}" "${f[3]}" ; then
                state=live
            else
                state=dead
            fi
            ec "  ${state}  ${f[1]} ${f[2]}  -> ${f[4]}"
        done
        h-agent-auto-continue-scope-status "${scope}" | command sed 's/^/  /'
    done
}
##
#: The watcher
##
function h-agent-auto-continue-watch-ensure {
    #: Starts the scope's watcher unless it is already running. Through
    #: [agfi:tmuxnewsh2], like the armed jobs: the tmux server outlives the
    #: agent, the shell and the brish garden, and the session name is the
    #: lock -- [agfi:tmuxnew] kills a previous, fired one before creating the
    #: replacement.
    #: Usage: h-agent-auto-continue-watch-ensure <scope>
    ##
    local poll_s="${agent_auto_continue_poll_s:-300}"
    local claude_roles="${agent_auto_continue_claude_roles:-session weekly_all}"

    local scope="${1}"
    assert-args scope @RET

    ensure-cmd tmux @RET

    local session
    session="$(h-agent-auto-continue-watch-session "${scope}")" @RET

    if tmux-alive-p "${session}" ; then
        return 0
    fi

    #: The knobs travel as arguments: the watcher runs in its own interactive
    #: shell, and a value scoped to this call would otherwise be lost there.
    silent tmuxnewsh2 "${session}" \
        agent_auto_continue_poll_s="${poll_s}" \
        agent_auto_continue_claude_roles="${claude_roles}" \
        h-agent-auto-continue-watch "${scope}" @RET

    #: The name is how the watcher is found; the agent it serves must not
    #: rename it ([agfi:claude-code-session-tmux-autoname]).
    silent tmux set-option -t "${session}" "${agent_tmux_autoname_option}" off || true

    ecgray "$0: started ${session}"
}

function h-agent-auto-continue-watch-stop {
    #: Usage: h-agent-auto-continue-watch-stop <scope>
    local scope="${1}"
    assert-args scope @RET

    local session
    session="$(h-agent-auto-continue-watch-session "${scope}")" @RET

    silent tmux has-session -t "=${session}" || return 0
    silent tmux-session-processes-kill "${session}"
    ecgray "$0: stopped ${session}"
}

function h-agent-auto-continue-watch {
    #: The watcher body, running inside the tmux session
    #: [agfi:h-agent-auto-continue-watch-ensure] creates. A function, not a
    #: loop of bare commands, for the reason given in =PE/Zsh.org=: a bare
    #: `sleep' does not keep the session alive.
    #:
    #: Each tick prunes the scope's registrations against one live listing,
    #: runs the check, and exits once nothing is registered -- an empty scope
    #: has nothing to resume, and polling an endpoint for it would be waste.
    #: Usage: h-agent-auto-continue-watch <scope>
    ##
    local poll_s="${agent_auto_continue_poll_s:-300}"

    local scope="${1}"
    assert-args scope @RET

    local kept
    while true ; do
        kept="$(h-agent-auto-continue-prune "${scope}")"

        #: With nothing left this cancels the scope's armed job; otherwise it
        #: arms, re-arms or leaves well alone.
        agent-auto-continue-check "${scope}" || true

        if test -z "${kept}" ; then
            ecgray "$0: ${scope}: nothing registered, exiting"
            h-agent-usage-arm-log "auto-continue watch ${scope}: nothing registered, exiting"
            return 0
        fi

        sleep "${poll_s}"
    done
}
##
#: The check
##
function h-agent-auto-continue-claude-family {
    #: The model family a Claude Code session is on -- `fable', `opus',
    #: `sonnet' -- from the model named on its most recent assistant record,
    #: so the check can add that family's weekly window
    #: (`weekly:<family>' in [agfi:h-claude-code-usage-arm-window], which
    #: matches the window label case-insensitively). Only the model id is
    #: read out of the transcript. Fails when there is none yet.
    #: Usage: h-agent-auto-continue-claude-family <transcript>
    ##
    local transcript="${1}"
    assert-args transcript @RET
    test -r "${transcript}" || return 1

    ensure-cmd jq @RET

    #: The tail, not the file: a transcript can run to many megabytes, and the
    #: current model is on the last assistant record.
    local model
    model="$(command tail -n 200 -- "${transcript}" 2>/dev/null |
        jq -r 'select(type == "object" and .type == "assistant") | .message.model // empty' 2>/dev/null |
        command tail -n 1)" || model=''
    test -n "${model}" || return 1

    model="${model#claude-}"
    ec "${model%%-*}"
}

function h-agent-auto-continue-arm-group {
    #: Arms one job for one target set through the scope's deadline source,
    #: unless that job is already armed for exactly these targets -- a blocked
    #: scope must not kill and recreate its job every tick. A different set
    #: (a session registered or pruned while blocked) re-arms, which is what
    #: makes the job's targets follow the registrations.
    #: Usage: h-agent-auto-continue-arm-group <job-session> <targets> <arm-fn> [args...]
    ##
    local session="${1}" targets="${2}"
    shift 2
    assert-args session targets @RET
    (( $# )) || {
        ecerr "$0: no arm command given"
        return 1
    }

    if tmux-alive-p "${session}" ; then
        local recorded deadline
        recorded="$(tmux show-options -qv -t "${session}" '@agent_usage_arm_targets' 2>/dev/null)" || recorded=''
        deadline="$(tmux show-options -qv -t "${session}" '@agent_usage_arm_deadline' 2>/dev/null)" || deadline=''
        if test -n "${deadline}" && [[ "${recorded}" == "${targets}" ]] ; then
            ecgray "$0: ${session}: already armed for these targets"
            return 0
        fi
    fi

    #: `local' is dynamically scoped, so [agfi:h-agent-usage-arm] reads these
    #: from here, and presetting the targets is what skips its picker.
    local agent_usage_arm_action=continue
    local agent_usage_arm_targets="${targets}"

    reval "$@"
}

function agent-auto-continue-check {
    : "usage: agent-auto-continue-check <scope>
Arms the scope's resume job if the scope's usage is blocked, over every
registered session's target; cancels it when nothing is registered."
    #: Idempotent, so the watcher, a hook, `on' and `off' can all call it.
    #: The decision "blocked, and until when" is each agent's own deadline
    #: source, which also refuses when usage is possible and honours `deus'.
    ##
    local claude_roles="${agent_auto_continue_claude_roles:-session weekly_all}"

    local scope="${1}"
    assert-args scope @RET

    local -a regs
    regs=( ${(f)"$(h-agent-auto-continue-registrations "${scope}")"} )

    if (( ${#regs} == 0 )) ; then
        local -a jobs
        jobs=( ${(f)"$(h-agent-auto-continue-tmux-sessions jobs "${scope}")"} )
        if (( ${#jobs} )) ; then
            h-agent-usage-armed-cancel "${jobs[@]}"
        fi
        return 0
    fi

    local agent
    agent="$(h-agent-auto-continue-scope-agent "${scope}")" @RET

    #: Targets per group, space separated and sorted, in the form
    #: `agent_usage_arm_targets' takes. Only Claude Code has more than one
    #: group: sessions on different models are blocked by different weekly
    #: windows, so each family gets its own job with its own roles. A blocked
    #: `session' or `weekly_all' window arms all of them.
    local -A group_targets
    local reg family
    local -a f
    for reg in "${regs[@]}" ; do
        f=( "${(@ps:\t:)reg}" )
        test -n "${f[4]}" || continue

        family=''
        if [[ "${agent}" == claude ]] ; then
            family="$(h-agent-auto-continue-claude-family "${f[3]}" 2>/dev/null)" || family=''
        fi

        group_targets[${family:-_}]+=" ${f[4]}"
    done

    local group targets
    local -a words roles
    for group in "${(@k)group_targets}" ; do
        words=( ${=group_targets[${group}]} )
        targets="${(j: :)${(ou)words}}"
        test -n "${targets}" || continue

        [[ "${group}" == '_' ]] && group=''

        case "${agent}" in
            claude)
                roles=( ${=claude_roles} )
                test -n "${group}" && roles+=( "weekly:${group}" )
                h-agent-auto-continue-arm-group \
                    "$(h-agent-auto-continue-job-session "${scope}" "${group}")" "${targets}" \
                    h-claude-code-usage-arm "$(h-agent-auto-continue-job-session "${scope}" "${group}")" \
                    "${scope#claude-}" "${roles[@]}"
                ;;
            codex)
                h-agent-auto-continue-arm-group \
                    "$(h-agent-auto-continue-job-session "${scope}")" "${targets}" \
                    h-codex-status-arm-auth "$(h-agent-auto-continue-job-session "${scope}")" \
                    "${scope#codex-}"
                ;;
            agy)
                h-agent-auto-continue-arm-group \
                    "$(h-agent-auto-continue-job-session "${scope}")" "${targets}" \
                    h-agy-status-arm "$(h-agent-auto-continue-job-session "${scope}")"
                ;;
            *)
                ecerr "$0: unknown agent in scope ${scope}: ${agent}"
                return 1
                ;;
        esac
    done
}
##
#: The hook
##
function agent-auto-continue-hook {
    : "hook body for Claude Code's StopFailure and Antigravity's Stop: <agent> [tmux-pane] [payload]"
    #: Makes the scope's check run now rather than at the next poll, for a
    #: session that is registered; a no-op costing one glob for every other
    #: session. Also revives the scope's watcher, which a reboot will have
    #: killed. Silent throughout; the hook lines discard output anyway.
    #:
    #: `$2' is the pane the hook line passes as "$TMUX_PANE"; unused here, the
    #: target having been fixed at registration, but kept so every hook body
    #: takes the same arguments.
    ##
    local kick_min_s="${agent_auto_continue_kick_min_s:-60}"

    local agent="${1}"
    test -n "${agent}" || return 0

    local input
    input="$(h-agent-hook-payload "${3}")"
    test -n "${input}" || return 0

    local id=''
    case "${agent}" in
        claude)
            id="$(ec "${input}" | jq -r '.session_id // empty' 2>/dev/null)"
            ;;
        codex)
            #: A subagent's payload carries `agent_id'; its `session_id' is
            #: the root thread's, which registered itself.
            id="$(ec "${input}" | jq -r 'select(.agent_id == null) | .session_id // empty' 2>/dev/null)"
            ;;
        agy)
            id="$(ec "${input}" | jq -r '.conversationId // empty' 2>/dev/null)"
            ;;
    esac
    test -n "${id}" || return 0

    setopt localoptions bareglobqual

    local -a files
    files=( "$(h-agent-auto-continue-dir)"/*/"${agent}-${id}"(N.) )
    (( ${#files} )) || return 0

    local scope="${files[1]:h:t}"

    #: Debounced per scope on a stamp file's mtime.
    zmodload zsh/datetime 2>/dev/null
    zmodload -F zsh/stat b:zstat 2>/dev/null
    local stamp="${files[1]:h}/.kicked"
    local -a st
    if zstat -A st +mtime "${stamp}" 2>/dev/null && (( EPOCHSECONDS - ${st[1]:-0} < kick_min_s )) ; then
        return 0
    fi
    command touch -- "${stamp}" 2>/dev/null || true

    h-agent-auto-continue-watch-ensure "${scope}" >/dev/null 2>&1 || true
    agent-auto-continue-check "${scope}" >/dev/null 2>&1 || true
}
