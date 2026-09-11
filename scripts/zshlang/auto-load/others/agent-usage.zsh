##
#: Waiting out a coding agent's rate limit, and picking the conversation back
#: up the moment it lifts -- for any agent, not just Claude Code.
#:
#: An agent-specific caller works out *when* the limits reset and what to say
#: about it ([agfi:h-claude-code-usage-arm] reads Claude Code's usage
#: endpoint); everything after that is here: arming a one-shot job, waiting out
#: the clock, deciding whether resuming is safe, and delivering the resume text
#: to whatever was picked. See =docs/agent-usage-armed.md=.
##
#: How often the armed job re-checks the wall clock.
typeset -g agent_usage_arm_poll_s="${agent_usage_arm_poll_s:-30}"
#: Fire this many seconds after the reset, so the endpoint has actually flipped
#: by the time we claim it has.
typeset -g agent_usage_arm_grace_s="${agent_usage_arm_grace_s:-30}"
#: What the armed job does once the limits reset: =notif= to tell you, or
#: =continue= to resume the session that was blocked.
typeset -g agent_usage_arm_action="${agent_usage_arm_action:-notif}"
#: How the resume reaches the session, and hence which picker arming opens:
#: =kitty= types into a kitty window, =tmux= types into a tmux pane, and
#: =frontmost= types wherever the keyboard focus happens to be, with no picker
#: at all. A Codex thread is queued rather than typed into whichever is chosen;
#: see [agfi:h-agent-usage-continue-rows-to-targets].
typeset -g agent_usage_continue_via="${agent_usage_continue_via:-kitty}"
#: Resuming waits longer after a reset than a notification does: an early
#: notification is harmless, an early resume is spent on a session that is
#: still blocked.
typeset -g agent_usage_continue_grace_s="${agent_usage_continue_grace_s:-60}"
#: Only resume when the keyboard has been untouched at least this long. If you
#: are at the machine you get a notification instead and can resume yourself.
typeset -g agent_usage_continue_idle_min_s="${agent_usage_continue_idle_min_s:-600}"
#: What gets sent. A carriage return is appended to submit it.
typeset -g agent_usage_continue_text="${agent_usage_continue_text:-Continue.}"
#: One line per fire.
typeset -g agent_usage_arm_log="${agent_usage_arm_log:-${HOME}/logs/agent-usage-arm.log}"
##
function h-agent-usage-arm {
    #: Arms, or re-arms, a one-shot job for a reset the caller has already
    #: worked out: $1 the tmux session it lives in, $2 the reset time as an
    #: epoch, $3 what to say when it fires.
    #:
    #: Re-arming cannot stack: [agfi:tmuxnew] kills the previous session's
    #: processes before creating the replacement, so the session name alone
    #: guarantees a single pending job -- no lock, marker or redis key. The
    #: tmux server is also independent of the brish garden, so =brishz-restart=
    #: does not silently disarm it. A reboot does.
    #:
    #: The session name doubles as the label the log and the notifications use,
    #: since it is the one string that identifies the job across agents.
    #: Usage: h-agent-usage-arm <session> <reset-epoch> <msg>
    ##
    local poll_s="${agent_usage_arm_poll_s:-30}"
    local action="${agent_usage_arm_action:-notif}"
    local idle_min_s="${agent_usage_continue_idle_min_s:-600}"
    local text="${agent_usage_continue_text:-Continue.}"

    #: Resuming gets the longer grace of the two; see the knobs above.
    local grace_s="${agent_usage_arm_grace_s:-30}"
    if [[ "${action}" == continue ]] ; then
        grace_s="${agent_usage_continue_grace_s:-60}"
    fi

    local session="${1}" reset_at="${2}" msg="${3}"
    assert-args session reset_at msg @RET

    ensure-cmd tmux @RET
    zmodload zsh/datetime 2>/dev/null

    #: `%.*' because a reset time can arrive as a float, and an integer
    #: assignment of one would truncate it silently rather than say so.
    integer deadline=$(( ${reset_at%.*} + grace_s ))

    if (( deadline <= EPOCHSECONDS )) ; then
        #: Named, because the two ways to get here read completely differently:
        #: a usage endpoint handing back a reset that has been and gone (stale
        #: data), and a person naming a time that is already behind us.
        ecgray "$0: ${session}: $(date-unix-to-3339 "${deadline}") is already past (stale data, or a time already gone), not arming"
        return 0
    fi

    #: Only now that we know we are going to arm, so a report that changes
    #: nothing never puts a picker in your way. Presetting the variable skips
    #: it, which is what makes this callable from a script or a test.
    local targets="${agent_usage_arm_targets}"
    if [[ "${action}" == continue ]] && test -z "${targets}" ; then
        local -a target_list
        target_list=("${(@f)$(h-agent-usage-continue-targets)}") @TRET
        #: Space separated, because that is what survives the trip into the
        #: tmux session's environment intact.
        targets="${(j: :)target_list}"

        if test -z "${targets}" ; then
            ecgray "$0: ${session}: no resume target chosen, not arming"
            return 0
        fi
    fi

    ecgray "$0: arming ${session} for $(date-unix-to-3339 "${deadline}") (in $(seconds-fmt-short $(( deadline - EPOCHSECONDS ))))"

    #: The job runs in its own interactive shell hours from now, so every knob
    #: it reads travels with it rather than being looked up there: a value
    #: scoped to this call -- a test's, a preset's -- would otherwise be lost
    #: and the file's default silently used instead.
    #:
    #: =silent= because [agfi:tmux-session-processes-kill] narrates every
    #: re-arm, which would otherwise land in the middle of a usage report.
    silent tmuxnewsh2 "${session}" \
        agent_usage_arm_poll_s="${poll_s}" \
        agent_usage_arm_action="${action}" \
        agent_usage_arm_targets="${targets}" \
        agent_usage_continue_idle_min_s="${idle_min_s}" \
        agent_usage_continue_text="${text}" \
        h-agent-usage-arm-wait "${deadline}" "${session}" "${msg}" @RET

    #: Recorded on the tmux session itself rather than in redis, so the
    #: bookkeeping cannot drift from whether the job actually exists.
    #:
    #: No `=' exact-match prefix on the target here: unlike =has-session=,
    #: =set-option= does not accept one and fails with "no such session".
    silent tmux set-option -t "${session}" '@agent_usage_arm_deadline' "${deadline}" || true
    silent tmux set-option -t "${session}" '@agent_usage_arm_action' "${action}" || true
    silent tmux set-option -t "${session}" '@agent_usage_arm_targets' "${targets}" || true
    #: The name is how this job is found and re-armed; the agent it resumes
    #: must not rename it ([agfi:claude-code-session-tmux-autoname]).
    silent tmux set-option -t "${session}" "${agent_tmux_autoname_option}" off || true
}

function h-agent-usage-arm-wait {
    #: The armed one-shot body, running inside the tmux session that
    #: [agfi:h-agent-usage-arm] creates. This has to be a function: a
    #: bare =sleep= does not keep the marked subshell alive (see =PE/Zsh.org=).
    ##
    local poll_s="${agent_usage_arm_poll_s:-30}"

    local deadline="${1}" label="${2}" msg="${3}"
    assert-args deadline label msg @RET

    zmodload zsh/datetime 2>/dev/null

    #: Poll the wall clock rather than issuing one long =sleep=: a suspend
    #: would skew a single five-hour sleep, and on wake we want to fire
    #: straight away instead of however long the machine slept later.
    while (( EPOCHSECONDS < deadline )) ; do
        sleep "${poll_s}"
    done

    h-agent-usage-arm-fire "${label}" "${msg}"
}

function h-agent-usage-arm-notify {
    #: A stable group, so a repeat replaces the previous notification instead
    #: of stacking up in Notification Center. See =docs/bell-auto.md=.
    ##
    local msg="${1}"
    assert-args msg @RET

    notif_group='agent-usage' notif "${msg}"
}

function h-agent-usage-arm-log {
    #: One line per fire. The tmux pane a fired job leaves behind says the same
    #: thing, but only until the next reboot, and a job that types into your
    #: session while you are away should stay answerable for it afterwards.
    ##
    local msg="${1}"
    assert-args msg @RET

    zmodload zsh/datetime 2>/dev/null

    local log="${agent_usage_arm_log:-${HOME}/logs/agent-usage-arm.log}"
    ensure-dir "${log:h}" || return 0

    print -r -- "$(strftime '%Y-%m-%d %H:%M:%S' "${EPOCHSECONDS}") ${msg}" >> "${log}"
}

function h-agent-usage-idle-s {
    #: How long the keyboard and mouse have been untouched, in whole seconds.
    #:
    #: Through [agfi:h-hammerspoon-eval], which strips the extension-loading
    #: chatter that would otherwise turn a result into
    #: `0-- Loading extension: host`. Hammerspoon exits 0 whether or not the Lua
    #: found anything, so what gets checked is the RESULT: a non-number means
    #: "cannot tell", and the caller declines to type on that.
    ##
    local out
    out="$(h-hammerspoon-eval 'return hs.host.idleTime()')" || return 1

    [[ "${out}" =~ '^[0-9]+(\.[0-9]+)?$' ]] || return 1

    ec "${out%%.*}"
}

function h-agent-usage-screen-locked-p {
    #: True when the screen is locked. The key is *absent* rather than false
    #: when unlocked, so the Lua compares and we test the resulting string.
    ##
    local out
    out="$(h-hammerspoon-eval 'return tostring(hs.caffeinate.sessionProperties()["CGSSessionScreenIsLocked"] == true)')" || return 1

    [[ "${out}" == true ]]
}

function h-agent-usage-continue-send {
    #: Delivers the resume text to one target: `kitty:<window-id>` types into
    #: that window, `tmux:<pane-id>` types into that pane,
    #: `codex:<thread-id>` queues the message with Codex itself, and
    #: `frontmost` types wherever the keyboard focus happens to be.
    ##
    local target="${1}"
    assert-args target @RET

    local text="${agent_usage_continue_text:-Continue.}"

    if [[ "${target}" == codex:* ]] ; then
        #: Codex takes a message for a thread by name, which beats typing: it
        #: needs no window, no focus and no awake display, and it cannot land
        #: in the wrong place. See [agfi:h-codex-session-live-list] for where
        #: the thread id comes from.
        ensure-cmd codex @RET
        reval-ec command codex queue --thread "${target#codex:}" --message "${text}"
        return $?
    fi

    if [[ "${target}" == frontmost ]] ; then
        #: Wake the display first and give it a beat. `displaysleep` is ten
        #: minutes here -- the same as the idle threshold -- so by the time this
        #: fires the screen is asleep, and the first synthetic keypress would be
        #: eaten waking it, typing `ontinue.`
        silent h-hammerspoon-eval 'hs.caffeinate.declareUserActivity() ; return true' || true
        sleep 1

        #: Its sleep argument is mandatory, and 0 is right: the waiting was ours
        #: to do, by polling, so that a suspend could not skew it.
        hs-type-continue 0 "${text}" @RET
        return 0
    fi

    if [[ "${target}" == tmux:* ]] ; then
        local pane="${target#tmux:}"

        #: Being alive is not enough. `remain-on-exit' keeps a finished pane
        #: around and the shell under an agent that quit is alive in its own
        #: right, so the question is whether an agent is still running *there*
        #: -- asked again now, at fire time, rather than trusted from when the
        #: pane was picked hours ago. Without this, a session that exited in
        #: the meantime gets `Continue.` typed into its shell prompt.
        local panes
        panes="$(h-agent-session-tmux-panes)" || panes=''
        if [[ $'\n'"${panes}" != *$'\n'"${pane}"$'\t'* ]] ; then
            ecerr "$0: tmux pane ${pane} no longer holds a live agent session"
            return 1
        fi

        tmux-pane-send-text "${pane}" "${text}" @RET
        return 0
    fi

    if [[ ! "${target}" =~ '^kitty:[0-9]+$' ]] ; then
        ecerr "$0: unknown target: ${target}"
        return 1
    fi
    local id="${target#kitty:}"

    ensure-cmd kitty jq @RET

    local sock
    sock="$(h-agent-session-kitty-socket)" @RET

    #: `send-text` documents that it "always succeeds, even if no text was sent
    #: to any window", so its exit status proves nothing and the window has to
    #: be checked for separately. Without this a tab closed during the wait
    #: would swallow the resume while we reported success.
    if ! kitty @ --to "${sock}" ls |
            jq -e --argjson id "${id}" 'any(.[].tabs[].windows[] ; .id == $id)' >/dev/null ; then
        ecerr "$0: kitty window ${id} is gone"
        return 1
    fi

    kitty @ --to "${sock}" send-text --match "id:${id}" "${text}"$'\r' @RET
}

function h-agent-usage-arm-fire {
    #: What the armed job does once the deadline has passed: tell you, or
    #: resume the sessions that were blocked.
    ##
    local action="${agent_usage_arm_action:-notif}"
    local idle_min_s="${agent_usage_continue_idle_min_s:-600}"

    local label="${1}" msg="${2}"
    assert-args label msg @RET

    if [[ "${action}" != continue ]] ; then
        h-agent-usage-arm-notify "${msg}"
        h-agent-usage-arm-log "${label}: notified"

        return 0
    fi

    local -a targets
    targets=(${=agent_usage_arm_targets})
    if (( ${#targets} == 0 )) ; then
        h-agent-usage-arm-notify "${msg} -- not resuming: no target was recorded"
        h-agent-usage-arm-log "${label}: notified only, no target recorded"

        return 0
    fi

    #: Failing safe: anything we cannot establish means we do not type. The
    #: notification goes out either way, so an unwanted resume is the worse
    #: error of the two.
    local idle_s reason=''
    if ! idle_s="$(h-agent-usage-idle-s)" ; then
        reason='could not read the idle time'
    elif (( idle_s < idle_min_s )) ; then
        reason="you were at the keyboard ($(seconds-fmt-short "${idle_s}") idle, needs $(seconds-fmt-short "${idle_min_s}"))"
    elif h-agent-usage-screen-locked-p ; then
        reason='the screen is locked'
    fi

    if test -n "${reason}" ; then
        h-agent-usage-arm-notify "${msg} -- not resuming: ${reason}"
        h-agent-usage-arm-log "${label}: notified only: ${reason}"

        return 0
    fi

    local target
    local -a resumed unreachable
    for target in "${targets[@]}" ; do
        if h-agent-usage-continue-send "${target}" ; then
            resumed+=("${target}")
        else
            unreachable+=("${target}")
        fi
    done

    local report="${msg}"
    if (( ${#resumed} )) ; then
        report+=" -- resumed ${(j:, :)resumed}"
    fi
    if (( ${#unreachable} )) ; then
        report+=" -- could not reach ${(j:, :)unreachable}"
    fi

    h-agent-usage-arm-notify "${report}"
    h-agent-usage-arm-log "${label}: ${report}"
}
##
#: Choosing what gets resumed
##
function h-agent-usage-continue-targets {
    #: The targets to resume, one per line, picked the way
    #: `agent_usage_continue_via' says. Chosen at ARM time, so what gets
    #: resumed is what you picked rather than whatever happens to hold the
    #: keyboard hours later.
    ##
    local via="${agent_usage_continue_via:-kitty}"

    case "${via}" in
        kitty) h-agent-usage-continue-targets-kitty-fz ;;
        tmux) h-agent-usage-continue-targets-tmux-fz ;;
        frontmost)
            #: Nothing to pick: there is one frontmost window, and which one it
            #: will be is not knowable now anyway.
            ec frontmost
            ;;
        *)
            ecerr "$0: unknown agent_usage_continue_via: ${via} (kitty, tmux, frontmost)"
            return 1
            ;;
    esac
}

function h-agent-usage-continue-rows-to-targets {
    #: Turns picked rows -- `<id><TAB><transcript>', as the pickers print them
    #: -- on stdin into one target per line, the kind of id in column 1 being
    #: $1 (`kitty' or `tmux').
    #:
    #: A Codex thread is reachable without a window at all, which is strictly
    #: better than typing, so it is queued whichever picker found it; anything
    #: else is typed into the window or pane the picker named.
    #: Usage: ... | h-agent-usage-continue-rows-to-targets <kitty|tmux>
    ##
    local kind="${1}"
    assert-args kind @RET

    case "${kind}" in
        kitty|tmux) : ;;
        *)
            ecerr "$0: unknown target kind: ${kind}"
            return 1
            ;;
    esac

    #: `(ps:\t:)' rather than `read': tab is IFS whitespace, so `read' would
    #: collapse an empty field rather than keep the columns lined up.
    local line agent
    local -a f
    while IFS= read -r line ; do
        test -n "${line}" || continue
        f=( "${(@ps:\t:)line}" )

        #: The synthetic row [agfi:h-agent-usage-continue-targets-kitty-fz]
        #: prepends, which names no session at all.
        if [[ "${f[1]}" == frontmost ]] ; then
            ec frontmost
            continue
        fi

        agent="$(h-agent-session-agent-of "${f[2]}" 2>/dev/null)" || agent=''
        if [[ "${agent}" == codex ]] ; then
            ec "codex:$(h-agent-session-call codex id-of "${f[2]}")"
            continue
        fi

        test -n "${f[1]}" && [[ "${f[1]}" != '-' ]] || continue
        ec "${kind}:${f[1]}"
    done
}

function h-agent-usage-continue-targets-kitty-fz {
    #: Picks among the sessions showing in a kitty window. Prints one target
    #: per line: `kitty:<window-id>`, `codex:<thread-id>`, or `frontmost`.
    #:
    #: Every agent's live sessions are offered, not only the rate-limited
    #: one's: what the reset unblocks is often one conversation among several,
    #: and the waiting one may be a Codex thread told to hold off.
    ##
    #: `local` is dynamically scoped in zsh, so the picker sees these without
    #: anything being exported. The row layout is
    #: [agfi:h-agent-session-live-rows]'s.
    local -a agent_session_live_fz_extra_rows
    agent_session_live_fz_extra_rows=(
        $'frontmost\t-\t-\tfrontmost\t-\t-\t-\twhatever holds the keyboard when the limits reset'
    )

    #: No header: the picker's own advertises alt+enter, which converts a
    #: transcript to org. This picker is choosing what to resume, and that is
    #: noise here. The binding still works, it is just not announced.
    local agent_session_fz_header=''

    local selected
    selected="$(agent-session-live-fz)" @RET

    ec "${selected}" | h-agent-usage-continue-rows-to-targets kitty
}

function h-agent-usage-continue-targets-tmux-fz {
    #: Picks among the sessions running in a tmux pane. Prints one target per
    #: line: `tmux:<pane-id>` or `codex:<thread-id>`.
    #:
    #: The tmux twin of [agfi:h-agent-usage-continue-targets-kitty-fz], and
    #: the one to use for a session you left running on a machine you are not
    #: sitting at: typing into a pane needs no window manager, no focus and no
    #: awake display. There is no `frontmost' row for the same reason -- it
    #: would be the one choice here that does need all three.
    #:
    #: `agent_usage_continue_profile' narrows the rows to one Claude Code seat,
    #: on the config home the annotator puts in column 5, so arming the work
    #: profile's resume cannot offer a personal session. `agent_session_agents'
    #: narrows them to one agent; both are read dynamically, from the caller's
    #: `local'.
    ##
    local profile="${agent_usage_continue_profile}"

    #: No header, for the reason given in the kitty picker.
    local agent_session_fz_header=''

    local rows
    rows="$(h-agent-session-tmux-panes | h-agent-session-annotate-rows)" || rows=''
    if test -z "${rows}" ; then
        ecerr "$0: no live agent session in a tmux pane"
        return 1
    fi

    if test -n "${profile}" ; then
        local home="${claude_code_profile_homes[$profile]}"
        if test -z "${home}" ; then
            ecerr "$0: unknown Claude Code profile: ${profile}"
            return 1
        fi

        rows="$(ec "${rows}" | gawk -F'\t' -v home="${home}" '$5 == home')"
        if test -z "${rows}" ; then
            ecerr "$0: no live ${profile} session in a tmux pane"
            return 1
        fi
    fi

    local selected
    selected="$(ec "${rows}" | h-agent-session-fz multi)" @TRET
    test -n "${selected}" || return 1

    #: The display columns were only ever for the person choosing.
    ec "${selected}" | command cut -f1,2 |
        h-agent-usage-continue-rows-to-targets tmux
}
##
#: A deadline you name yourself
##
function agent-usage-continue-at {
    #: Resumes the sessions you pick at a time you name, in whatever words --
    #: `in 2 hours', `tomorrow 9am', `at 17:30'.
    #:
    #: The agent-neutral way in, and the only one an agent with no usage
    #: endpoint has: Antigravity publishes no quota and no reset anywhere, so
    #: there is nothing to read a deadline out of, and a time you name by hand
    #: is the whole of what is left. It is equally the hatch for a limit this
    #: repository does not model at all -- an API tier, a team quota, a
    #: colleague saying "try again after lunch".
    #:
    #: Its own tmux session, so it coexists with the per-agent armed jobs rather
    #: than replacing one; [agfi:agent-usage-armed-sessions] lists it with them.
    #: Usage: agent-usage-continue-at <when...>
    ##
    local when="${*}"
    assert-args when @RET

    ensure-cmd datenat.js @RET

    #: [agfi:datenat-unix], not [agfi:datenat-future-unix]. The `no past' check
    #: truncates to midnight before comparing, so it rejects every time still
    #: to come *today* -- `in 2 hours' among them, which is the commonest thing
    #: anyone types here. The check that matters is the arm's own, which
    #: compares the actual instant and names it when it refuses.
    local deadline
    deadline="$(datenat-unix "${when}")" @TRET
    if test -z "${deadline}" ; then
        ecerr "$0: could not read a time out of: ${when}"
        return 1
    fi

    #: No grace, in either guise: grace exists because an endpoint's reset time
    #: is a claim we would rather not take at its word, and a time a person
    #: named is not a claim about anything. Fire when they said.
    local agent_usage_arm_grace_s=0
    local agent_usage_continue_grace_s=0

    #: Forced rather than defaulted: resuming is the entire point of this
    #: entry point, and a bare notification at a time you named is what
    #: [agfi:reminday] is for.
    local agent_usage_arm_action=continue

    #: No `agent_usage_continue_profile' and no `agent_session_agents': this
    #: belongs to no agent, so every live session is offered, an agy pane
    #: included.
    h-agent-usage-arm agent-usage-continue-at "${deadline}" \
        "Manual resume at $(date-unix-to-3339 "${deadline}")"
}

#: The mechanism goes in the name, as it does for the Claude entry points.
aliasfnq agent-usage-continue-at-kitty-fz agent_usage_continue_via=kitty agent-usage-continue-at
aliasfnq agent-usage-continue-at-tmux-fz agent_usage_continue_via=tmux agent-usage-continue-at
aliasfnq agent-usage-continue-at-frontmost agent_usage_continue_via=frontmost agent-usage-continue-at

#: `a' for agent, `ca' for continue-at, then the mechanism.
aliasfn acak agent-usage-continue-at-kitty-fz
aliasfn acat agent-usage-continue-at-tmux-fz
aliasfn acafront agent-usage-continue-at-frontmost
##
#: Bookkeeping, over whatever sessions the caller's agent owns
##
function agent-usage-armed-sessions {
    #: Every tmux session any of these armed jobs can live in, one per line.
    #: What the no-argument [agfi:agent-usage-armed-status] and
    #: [agfi:agent-usage-armed-cancel] work over, so "what am I waiting on?"
    #: is one question rather than one per agent.
    ##
    local out=()

    #: Each agent's own list, asked for rather than repeated here: the sessions
    #: are named after the functions that arm them, so a rename should be a
    #: one-file change. Claude Code's is per profile and generated
    #: ([agfi:claude-code-usage-armed-sessions]); Codex's and Antigravity's are
    #: one name each today, and say so themselves.
    #:
    #: Guarded on the function existing, because these three files are
    #: independent: one absent, disabled or mid-edit must narrow this list
    #: rather than break `status' and `cancel' for the others.
    local fn
    for fn in claude-code-usage-armed-sessions codex-status-armed-sessions \
        agy-status-armed-sessions ; do
        (( ${+functions[${fn}]} )) || continue

        out+=( "${(@f)$("${fn}")}" )
    done

    #: [agfi:agent-usage-continue-at] names its session after itself, and is
    #: defined right here, so it needs no guard.
    out+=( 'agent-usage-continue-at' )

    ec "${(F)out}"
}

function agent-usage-armed-status {
    #: Every armed job, whichever agent armed it. Named arguments narrow
    #: it to those sessions.
    ##
    local sessions=("$@")
    if (( ${#sessions} == 0 )) ; then
        sessions=("${(@f)$(agent-usage-armed-sessions)}") @TRET
    fi

    h-agent-usage-armed-status "${sessions[@]}"
}

function agent-usage-armed-cancel {
    local sessions=("$@")
    if (( ${#sessions} == 0 )) ; then
        sessions=("${(@f)$(agent-usage-armed-sessions)}") @TRET
    fi

    h-agent-usage-armed-cancel "${sessions[@]}"
}

function agent-usage-armed-cancel-fz {
    #: Cancels the armed jobs you pick, over every session
    #: [agfi:agent-usage-armed-sessions] knows about, whichever agent armed
    #: them.
    #:
    #: The middle ground between the two bulk commands: with several agents'
    #: jobs armed at once, [agfi:agent-usage-armed-status] prints them all but
    #: cancels nothing, and [agfi:agent-usage-armed-cancel] takes them all.
    #: Multi-select, because "cancel these two and leave the rest" is the
    #: actual request.
    #:
    #: Sessions that have already fired are offered too, for the reason
    #: [agfi:h-agent-usage-armed-cancel] reaps them: with =remain-on-exit= on,
    #: their sessions linger and clearing them out is part of the same job.
    ##
    bella_zsh_disable1

    zmodload zsh/datetime 2>/dev/null

    local -a sessions
    sessions=("${(@f)$(agent-usage-armed-sessions)}") @TRET

    #: Keyed on the deadline so the rows can be sorted before the key is
    #: dropped again; a fired job sorts last, being bookkeeping rather than
    #: something still pending.
    local s deadline action targets suffix
    integer remaining
    local -a keyed
    for s in "${sessions[@]}" ; do
        test -n "${s}" || continue

        #: The `=' exact-match prefix, so a session whose name merely starts
        #: with this one cannot answer for it.
        silent tmux has-session -t "=${s}" || continue

        action="$(tmux show-options -qv -t "${s}" '@agent_usage_arm_action' 2>/dev/null)" || action=''
        targets="$(tmux show-options -qv -t "${s}" '@agent_usage_arm_targets' 2>/dev/null)" || targets=''
        suffix=''
        if test -n "${action}" ; then
            suffix="  [${action}"
            if test -n "${targets}" ; then
                suffix+=" -> ${targets}"
            fi
            suffix+=']'
        fi

        if ! tmux-alive-p "${s}" ; then
            keyed+=( "9999999999"$'\t'"${s}"$'\t'"${s}  fired${suffix}" )
            continue
        fi

        deadline="$(tmux show-options -qv -t "${s}" '@agent_usage_arm_deadline' 2>/dev/null)" || deadline=''
        if test -z "${deadline}" ; then
            keyed+=( "0"$'\t'"${s}"$'\t'"${s}  armed (no deadline recorded)${suffix}" )
            continue
        fi

        remaining=$(( deadline - EPOCHSECONDS ))
        if (( remaining > 0 )) ; then
            keyed+=( "${deadline}"$'\t'"${s}"$'\t'"${s}  fires $(date-unix-to-3339 "${deadline}") (in $(seconds-fmt-short ${remaining}))${suffix}" )
        else
            keyed+=( "${deadline}"$'\t'"${s}"$'\t'"${s}  overdue by $(seconds-fmt-short $(( -remaining )))${suffix}" )
        fi
    done

    if (( ${#keyed} == 0 )) ; then
        ecgray "$0: nothing is armed"
        return 0
    fi

    local -a rows
    rows=( "${(@on)keyed}" )
    #: The sort key was ours; what fzf gets is the session in column 1 and the
    #: display after it.
    rows=( "${(@)rows#*$'\t'}" )

    #: `--exit-0' comes from [agfi:fz], so picking nothing is a 130 rather than
    #: an empty selection we would have to cancel on.
    local selected
    selected="$(printf '%s\n' "${rows[@]}" |
        fz_no_preview=y fz --multi --delimiter=$'\t' --with-nth='2..' \
            --header 'enter cancels the selected jobs (tab marks several)')" || return 0
    test -n "${selected}" || return 0

    #: `(ps:\t:)' rather than `read': tab is IFS whitespace, so `read' would
    #: collapse the columns rather than keep them lined up.
    local line
    local -a f picked
    while IFS= read -r line ; do
        test -n "${line}" || continue

        f=( "${(@ps:\t:)line}" )
        test -n "${f[1]}" || continue

        picked+=( "${f[1]}" )
    done <<< "${selected}"

    (( ${#picked} )) || return 0

    #: It narrates `cancelled'/`reaped' itself.
    h-agent-usage-armed-cancel "${picked[@]}"
}

function h-agent-usage-armed-cancel {
    #: Usage: h-agent-usage-armed-cancel <tmux-session>...
    ##
    local sessions=("$@")
    assert-args sessions @RET

    local s alive_p
    for s in "${sessions[@]}" ; do
        if ! silent tmux has-session -t "=${s}" ; then
            continue
        fi

        alive_p=n
        if tmux-alive-p "${s}" ; then
            alive_p=y
        fi

        #: Dead sessions get reaped too. With =remain-on-exit= on, a job that
        #: has already fired leaves its session behind, and clearing those out
        #: is what someone running a cancel actually wants.
        silent tmux-session-processes-kill "${s}"
        if bool "${alive_p}" ; then
            ecgray "$0: cancelled ${s}"
        else
            ecgray "$0: reaped ${s}, which had already fired"
        fi
    done
}

function h-agent-usage-armed-status {
    #: Usage: h-agent-usage-armed-status <tmux-session>...
    ##
    local sessions=("$@")
    assert-args sessions @RET

    zmodload zsh/datetime 2>/dev/null

    local s deadline
    integer remaining
    for s in "${sessions[@]}" ; do
        if ! tmux-alive-p "${s}" ; then
            #: With =remain-on-exit= on a fired job leaves its session behind,
            #: which answers "did my notification actually go off?".
            if silent tmux has-session -t "=${s}" ; then
                ecgray "${s}: not armed; a previous armed job has already fired"
            else
                ecgray "${s}: not armed"
            fi

            continue
        fi

        deadline="$(tmux show-options -qv -t "${s}" '@agent_usage_arm_deadline' 2>/dev/null)" || deadline=''

        #: Which action is pending matters as much as when: arming a resume
        #: replaces a plain notifier for that profile, and the reverse, so a
        #: downgrade should be visible rather than silent.
        local action targets suffix=''
        action="$(tmux show-options -qv -t "${s}" '@agent_usage_arm_action' 2>/dev/null)" || action=''
        targets="$(tmux show-options -qv -t "${s}" '@agent_usage_arm_targets' 2>/dev/null)" || targets=''
        if test -n "${action}" ; then
            suffix=" [action: ${action}"
            if test -n "${targets}" ; then
                suffix+=" -> ${targets}"
            fi
            suffix+=']'
        fi
        if test -z "${deadline}" ; then
            ec "${s}: armed (no deadline recorded)${suffix}"
            continue
        fi

        remaining=$(( deadline - EPOCHSECONDS ))
        if (( remaining > 0 )) ; then
            ec "${s}: armed for $(date-unix-to-3339 "${deadline}") (in $(seconds-fmt-short ${remaining}))${suffix}"
        else
            ec "${s}: armed, but its deadline passed $(seconds-fmt-short $(( -remaining ))) ago${suffix}"
        fi
    done
}
##
