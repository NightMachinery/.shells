##
function h-codex-notify {
    local info="$1"

    # ec "${info}" | jq . >> ~/logs/codex_notifs|| true
    #: These might leak private data, so only enable it if you need it for debugging.

    bell-codex "${info}"
}
##
function codex-ask {
    local model="${codex_model:-gpt-5.2}" reasoning_effort="${codex_reasoning_effort:-high}" color="${codex_color}"
    local inargs prompt out opts=()
    in-or-args2 "$@" @RET
    prompt="${inargs[*]}"

    typeset -x DISABLE_BRISH=y
    #: To forcefully disable [agfi:bell-codex]

    out="$(gmktemp --suffix=.md)" @TRET

    if test -n "${model}" ; then
        opts+=(--model="${model}")
    fi
    if test -n "${color}" ; then
        opts+=(--color="${color}")
        #: always, never, auto
    fi

    (
        assert cdm ~/tmp/codex_ask @RET
        assert silent git init @RET

        ec "${prompt}" | revaldbg codex -c model_reasoning_effort="${reasoning_effort}" -c model_reasoning_summary="detailed" --search --ask-for-approval on-failure --sandbox read-only exec "${opts}" --output-last-message="${out}" -
    ) >&2 @RET

    if isTty ; then
        ecgray $'\n'"$0: saved last message to: ${out}"
    fi

    cat "${out}" |
        cat-copy-if-tty
}

function codex-ask-low {
    codex_reasoning_effort=low codex-ask "$@"
}

function codex-ask-med {
    codex_reasoning_effort=medium codex-ask "$@"
}
##
function codex {
    #: The bare launcher: no instruction sync, which [agfi:codex-m] does.
    ##
    agent_launch_sync_p=n h-agent-launch codex codex "$@"
}

function codex-m {
    #: Keeps ~/.codex/AGENTS.md current with its sources; asks before
    #: launching with stale instructions.
    h-agents-md-sync-ask @RET

    memoi_expire=$(( 3600*24*1 )) reval-memoi codex-install
    #: run codex-install every once in a while
    ##
    ensure-array codex_security_opts
    local security_opts=( "${codex_security_opts[@]}" )
    if (( ${#security_opts[@]} == 0 )) ; then
        # security_opts=(--ask-for-approval on-failure --sandbox workspace-write)
    fi

    # -c model_reasoning_effort="high"
    #: Show the project and task name without the working spinner that triggers
    #: repeated Termux toasts. Thread-name generation can still briefly animate;
    #: TUI animations stay enabled. User arguments can override this default.
    #: See =docs/tmux-tty-title.md=.
    $proxyenv reval-ec codex "${security_opts[@]}" -c model_reasoning_summary="detailed" -c 'tui.terminal_title=["project-name","thread-name"]' --search --approve-for-me "$@"
    # -c web_search="true"
    # -c model_verbosity="high"
    #
    # --ask-for-approval:
    # - untrusted: Only run "trusted" commands (e.g. ls, cat, sed) without  asking for user approval. Will escalate to the user if the model proposes  a command that is not in the "trusted" set
    # - on-failure: Run all commands without asking for user approval. Only asks  for approval if a command fails to execute, in which case it will  escalate to the user to ask for un-sandboxed execution
    # - on-request: The model decides when to ask the user for approval
    # - never: Never ask for user approval Execution failures are immediately returned to the model
}

function codex-yolo {
    codex_security_opts=(--dangerously-bypass-approvals-and-sandbox) codex-m "$@"
}
##
function h-codex-install-pre {
    h-npm-install-clean-staging '@openai/codex' @RET

    if isDeus ; then
        #: Not just the staging leftovers: throw away the install itself and
        #: let it be rebuilt from scratch.
        trs "$(h-npm-global-dir '@openai/codex')"
    fi
}

function codex-install-npm {
    h-codex-install-pre @RET

    reval-ecgray npm-install-npm '@openai/codex' @RET
    h-npm-install-report codex
}

function codex-install-pnpm {
    #: Currently broken. codex's darwin-arm64 payload unpacks to ~288MB, and
    #: its tarball is over the size where pnpm's worker-thread integrity check
    #: aborts the process; see [agfi:npm-install] for the mechanism. Kept so
    #: the pnpm route stays one word away once pnpm or node fixes it.
    ##
    h-codex-install-pre @RET

    reval-ecgray npm-install-pnpm '@openai/codex' @RET
    h-npm-install-report codex
}

function codex-install {
    #: npm for now, because pnpm cannot install codex at all; see
    #: [agfi:codex-install-pnpm].
    ##
    codex-install-npm "$@"
}
##
function codex-clean-text {
    in-or-args "$@" |
        perl -CSD -pe 's/\x{258C}//g' |
        cat-copy-if-tty
}
##
function codex-status {
    local codex_status_timeout_s="${codex_status_timeout_s:-60}"
    local codex_status_retries="${codex_status_retries:-10}"
    local codex_status_profile="${codex_status_profile:-}"
    local codex_status_cd="${codex_status_cd:-$HOME/tmp}"
    local codex_status_strip_ansi_p="${codex_status_strip_ansi_p:-n}"

    ensure-array codex_status_args
    local codex_args=("${codex_status_args[@]}")

    if ! command -v -- codex_status.py >/dev/null 2>&1 ; then
        ecerr "codex-status: codex_status.py not found in PATH"
        return 127
    fi
    local script_args=(--timeout "${codex_status_timeout_s}" --retries "${codex_status_retries}")
    if test -n "${codex_status_profile}" ; then
        script_args+=(--profile "${codex_status_profile}")
    fi
    if test -n "${codex_status_cd}" ; then
        script_args+=(--cd "${codex_status_cd}")
    fi
    if bool "${codex_status_strip_ansi_p}" ; then
        script_args+=(--color never)
    fi

    local arg
    for arg in "${codex_args[@]}" ; do
        script_args+=(--codex-arg "${arg}")
    done

    (
        cdm ~/tmp/.codex-status/

        $proxyenv revaldbg command codex_status.py "$@" "${script_args[@]}"
    )
}
alias cs='codex-status'
##
#: Waiting out a Codex rate limit, and picking the threads back up once it
#: lifts. Only *when* the limit resets and *what to say* about it are Codex's;
#: arming the one-shot job, waiting out the clock, the idle gate and the
#: delivery are agent-neutral and live in =agent-usage.zsh=. See
#: =docs/agent-usage-armed.md= and =docs/codex_status.md=.
#:
#: Two things are worth knowing before reading the code.
#:
#: There are no =-continue-kitty-fz= / =-continue-tmux-fz= / =-continue-frontmost=
#: variants here, where Claude Code has three. Codex accepts a message for a
#: thread by *name* (`codex queue --thread'), so a resume needs no kitty
#: window, no tmux pane, no keyboard focus and no awake display, and it cannot
#: land in the wrong place. Queueing dominates typing, so it is the only
#: delivery, and every Codex row either picker offers is mapped to a
#: `codex:<thread>' target by [agfi:h-agent-usage-continue-rows-to-targets]
#: rather than to a window or a pane. That is also why the rows are widened to
#: every live thread, not only those showing in a window.
#:
#: =averageUsage.firstTimeToReset= is present only when EVERY checked auth
#: file is exhausted, so its absence does not mean "no reset is coming" -- it
#: means another auth still has room, and the move is to `swap' rather than to
#: wait. Arming then would only tell you hours later what the report is
#: telling you now, so we decline and say so.
##
function codex-status-armed-sessions {
    #: The tmux session Codex's armed job lives in, one per line. Plural,
    #: and a function rather than a literal, so it stays interchangeable with
    #: [agfi:claude-code-usage-armed-sessions] for anything gathering every
    #: agent's sessions at once.
    ##
    ec 'codex-status-armed'
}

function h-codex-status-reset-credit-note {
    #: A free "Full reset" grant ends a wait early, so say so at arm time:
    #: being told to come back in six days is the moment you want to know the
    #: wait is skippable. Reported, never redeemed -- spending a one-off grant
    #: is the user's call.
    #: Usage: h-codex-status-reset-credit-note <json>
    ##
    local json="${1}"
    test -n "${json}" || return 0

    local count
    count="$(ec "${json}" | jq -r '
        [.authFiles[]? // .
         | .quota.resetCredits.availableCount // 0] | add // 0')" || return 0

    if test -n "${count}" && (( count > 0 )) ; then
        local titles
        titles="$(ec "${json}" | jq -r '
            [.authFiles[]? // .
             | .quota.resetCredits.credits[]?
             | select(.status == "available") | .title]
            | unique | join(", ")')" || titles=''
        ecgray "$0: ${count} reset credit(s) available${titles:+ (${titles})} -- the wait can be skipped"
    fi
}

function h-codex-status-arm {
    #: Arms, or re-arms, a one-shot job for when Codex's quota comes back.
    ##
    ensure-cmd jq @RET

    local session
    session="$(codex-status-armed-sessions)" @TRET

    #: ANSI stripped, because the JSON is about to be parsed rather than read:
    #: [agfi:codex-status] passes our arguments before its own, so =--json=
    #: reaches the script cleanly.
    local json
    json="$(codex_status_strip_ansi_p=y codex-status --json)" @TRET

    #: `select' rather than a `// ""' default on each field, so the two
    #: columns are either both there or the output is empty: a defaulted alias
    #: beside an absent deadline would collapse to one field and be read as
    #: the deadline.
    local out
    out="$(ec "${json}" |
        jq -r '.averageUsage
            | select(.firstTimeToReset != null)
            | [(.firstTimeToReset | tostring), (.firstTimeToResetAlias // "?")]
            | @tsv')" @TRET

    local deadline="${out%%$'\t'*}" auth_alias="${out#*$'\t'}" msg=''
    if test -n "${deadline}" ; then
        msg="Codex (${auth_alias}): quota reset, usage available again"
    else
        if ! isDeus ; then
            ecgray "$0: usage already possible (some auth has quota), not arming (use \`deus\` to arm anyway)"
            return 0
        fi

        #: deus: arm for the earliest rollover among the auth files that
        #: answered, whether or not anything is exhausted, so the mechanism can
        #: be exercised without first running every account dry.
        #:
        #: The EARLIEST reset the auth reports anywhere, not
        #: `.rateLimits.primary': which slot holds the short window is
        #: plan-dependent, and on plans reporting a single weekly window
        #: `primary' IS that weekly one -- a week away, which exercises
        #: nothing. Per-model families are included here, and only here,
        #: because deus wants the soonest thing that will visibly roll over;
        #: the real arm above deliberately ignores them, since a spent
        #: per-model budget does not block ordinary usage.
        #:
        #: Read from the raw `.rateLimitsByLimitId' rather than `.quota.limits',
        #: which hides idle families -- and an idle family is exactly the one
        #: with a rollover soon. `.rateLimits.primary' stays as a fallback for
        #: a report predating `.quota'.
        #:
        #: `numbers' drops a null or a missing reset time rather than letting
        #: it sort to the front and arm us for the epoch.
        out="$(ec "${json}" |
            jq -r '[.authFiles[]
                    | select(.ok)
                    | {at: ([ (.quota.windows // [])[]?.resetsAt,
                              (.rateLimitsByLimitId // {} | .[]? | (.primary.resetsAt, .secondary.resetsAt)),
                              .rateLimits.primary.resetsAt ]
                            | map(numbers) | min),
                       alias: (.alias // "?")}
                    | select(.at != null)]
                | sort_by(.at) | first
                | select(. != null)
                | [(.at | tostring), .alias]
                | @tsv')" @TRET

        deadline="${out%%$'\t'*}"
        auth_alias="${out#*$'\t'}"
        if test -z "${deadline}" ; then
            ecgray "$0: no primary reset time in the report, not arming"
            return 0
        fi

        msg="Codex (${auth_alias}): earliest window rolled over"
    fi

    h-codex-status-reset-credit-note "${json}"

    #: A reset time arrives as a float often enough to matter, and integer
    #: arithmetic on one aborts rather than rounds. [agfi:h-agent-usage-arm]
    #: strips it too; doing it here as well keeps what we hand over honest
    #: rather than relying on the callee to clean it up.
    deadline="${deadline%.*}"

    #: `local' is dynamically scoped in zsh, so the picker inside the arm sees
    #: these without anything being exported. Only Codex rows, because only a
    #: Codex thread is unblocked by a Codex quota reset -- unlike Claude's
    #: kitty picker, which deliberately offers every agent.
    #:
    #: `all' rather than the default `windows': a thread is queueable whether
    #: or not anything happens to be showing it.
    #:
    #: `agent_usage_continue_via' is deliberately left alone. The kitty picker
    #: is the one that lists threads with no pane, and the tmux picker maps
    #: Codex rows to the same queue targets, so either choice works and the
    #: user's stays honoured.
    local agent_session_agents=codex
    local agent_session_live_rows_scope=all

    h-agent-usage-arm "${session}" "${deadline}" "${msg}"
}

#: Utilization at or above which one auth's window counts as blocking it. The
#: per-auth twin of =claude_code_usage_arm_full_pct=; only
#: [agfi:h-codex-status-arm-auth] reads it, since the all-auths arm above
#: leaves "exhausted" to the script.
typeset -g codex_status_arm_full_pct="${codex_status_arm_full_pct:-100}"

function h-codex-status-active-alias {
    #: The alias of the auth the running Codex was started with: the
    #: `auth_<alias>.json' snapshot whose bytes match `auth.json', else `auth',
    #: which is the label =codex_status.py= gives the bare file
    #: (`alias_from_auth_path'). The same rule as its
    #: `workspace_name_from_matching_auth_alias', so the scope
    #: [agfi:h-agent-auto-continue-scope] derives here names the same
    #: `.authFiles[].alias' the report prints.
    ##
    setopt localoptions bareglobqual

    local home
    home="$(h-codex-session-home)" @RET
    local bare="${home}/auth.json"
    test -e "${bare}" || return 1

    local f
    for f in "${home}"/auth[._-]*.json(N.) ; do
        if command cmp -s -- "${bare}" "${f}" ; then
            ec "${${f:t:r}#auth[._-]}"
            return 0
        fi
    done

    ec auth
}

function h-codex-status-arm-auth {
    #: Arms, or re-arms, a one-shot job for when ONE auth's quota comes back,
    #: unlike [agfi:h-codex-status-arm], which waits for every auth to be
    #: exhausted because a person can `swap'. A running thread cannot: it is
    #: signed in to one auth and stays blocked until that one resets, which is
    #: what [agfi:agent-auto-continue-check] is waiting on.
    #:
    #: The verdict comes from =codex_status.py='s `.quota' rather than being
    #: re-derived here: which windows a plan reports is not fixed (prolite
    #: reports one weekly window and no secondary), and a second copy of that
    #: rule in jq is a second copy to get wrong. `.quota.resetsAt' is already
    #: the LATEST reset among the blocked windows -- a short rollover buys
    #: nothing while a longer window is spent -- and `--full-pct' hands our
    #: threshold to the script so both agree on what "spent" means.
    #: Usage: h-codex-status-arm-auth <session> <alias>
    ##
    local full_pct="${codex_status_arm_full_pct:-100}"

    local session="${1}" auth_alias="${2}"
    assert-args session auth_alias @RET

    ensure-cmd jq @RET

    local json
    json="$(codex_status_strip_ansi_p=y codex-status --json --full-pct "${full_pct}")" @TRET

    local auth
    auth="$(ec "${json}" | jq -c --arg alias "${auth_alias}" \
        '[.authFiles[]? | select(.alias == $alias)] | first | select(. != null)')" || auth=''
    if test -z "${auth}" ; then
        ecerr "$0: no auth named ${auth_alias} in the report"
        return 1
    fi
    if ! ec "${auth}" | jq -e '.ok' >/dev/null ; then
        ecerr "$0: ${auth_alias}: $(ec "${auth}" | jq -r '.error // "status check failed"')"
        return 1
    fi

    #: `.quota' carries the verdict, the deadline and the human reasons. The
    #: `numbers' guard stays: a blocked auth whose windows report no reset time
    #: must not arm us for the epoch.
    local blocked_p reasons
    blocked_p="$(ec "${auth}" | jq -r '.quota.blocked // false')" || blocked_p=false
    integer blocked_at=0
    blocked_at="$(ec "${auth}" | jq -r '(.quota.resetsAt | numbers) // 0')" || blocked_at=0
    blocked_at=${blocked_at%.*}
    reasons="$(ec "${auth}" | jq -r '[.quota.reasons[]?] | join(", ")')" || reasons=''

    integer deadline=0
    local msg=''
    if ! bool "${blocked_p}" || (( blocked_at == 0 )) ; then
        if ! isDeus ; then
            ecgray "$0: ${auth_alias}: usage already possible, not arming (use \`deus\` to arm anyway)"
            return 0
        fi

        #: deus: the earliest rollover this auth reports anywhere, so the
        #: mechanism can be exercised without running the account dry. Not
        #: `primary', because which slot holds the short window is
        #: plan-dependent and on a single-window plan `primary' is the weekly
        #: one. Per-model families count here, and only here -- see the twin
        #: branch in [agfi:h-codex-status-arm].
        deadline="$(ec "${auth}" | jq -r '
            [ (.quota.windows // [])[]?.resetsAt,
              (.rateLimitsByLimitId // {} | .[]? | (.primary.resetsAt, .secondary.resetsAt)),
              .rateLimits.primary.resetsAt ]
            | map(numbers) | min // 0')" || deadline=0
        deadline=${deadline%.*}
        if (( deadline == 0 )) ; then
            ecgray "$0: ${auth_alias}: no reset time in the report, not arming"
            return 0
        fi
        msg="Codex (${auth_alias}): earliest window rolled over"
    else
        deadline=${blocked_at}
        msg="Codex (${auth_alias}): ${reasons:-quota} reset, usage available again"
    fi

    h-codex-status-reset-credit-note "${auth}"

    #: Same picker narrowing as [agfi:h-codex-status-arm], for a caller that
    #: presets no targets.
    local agent_session_agents=codex
    local agent_session_live_rows_scope=all

    h-agent-usage-arm "${session}" "${deadline}" "${msg}"
}

function h-codex-status-arm-schedule {
    #: Arming without printing a report, for when the report is already in
    #: front of you. The =h-= says the =-notify= forms below are the intended
    #: way in, not that this is off limits.
    ##
    h-codex-status-arm "$@"
}

function codex-status-notify {
    #: The ordinary report, then a notification armed for the reset. Mirrors
    #: [agfi:claude-code-usage-fable-notify].
    ##
    local retcode=0
    codex-status "$@" || retcode=$?

    if (( retcode == 0 )) ; then
        #: =>&2= because our stdout may be a JSON document a caller is about
        #: to parse; non-fatal because a failed schedule must not make a
        #: working report look broken.
        h-codex-status-arm-schedule >&2 || true
    fi

    return "${retcode}"
}

#: The report plus an arm whose action is to pick the threads back up, rather
#: than merely to announce that it is possible. One entry point, not three:
#: see the delivery note at the top of this block.
aliasfnq codex-status-continue-fz \
    agent_usage_arm_action=continue \
    codex-status-notify

aliasfn csc codex-status-continue-fz

#: Cancelling and reporting are the same act whatever armed the job, so both
#: are the shared helpers over the session Codex owns. Named arguments still
#: narrow it, which is only useful when a caller knows the session by name.
function codex-status-armed-cancel {
    local sessions=("$@")
    if (( ${#sessions} == 0 )) ; then
        sessions=("${(@f)$(codex-status-armed-sessions)}")
    fi

    h-agent-usage-armed-cancel "${sessions[@]}"
}

function codex-status-armed-status {
    local sessions=("$@")
    if (( ${#sessions} == 0 )) ; then
        sessions=("${(@f)$(codex-status-armed-sessions)}")
    fi

    h-agent-usage-armed-status "${sessions[@]}"
}
##
function image2remote {
    local input="${1}"
    if test -e "${input}" ; then
        reval-ecgray pbadd "${input}" @RET
    fi

    local fullhost="${fullhost:-pinky}"
    assert-args fullhost @RET

    local name="${EPOCHSECONDS}.png"
    local dest="tmp/screenshots/${name}"
    local src="${HOME}/${dest}"

    (
        cdtmp @RET

        assert pngpaste "${src}" @RET
        icat "${src}" || true

        assert reval-ecgray rsp-safe --mkpath -- "${src}" "${fullhost}:${dest}" @RET
    ) >&2 @RET

    ec "Look at \`~/${dest}\`. " |
        cat-copy-if-tty

    bell-sonic-fx-zone-moved
}
alias ire='image2remote'
##
function codex-auth2remote {
    local fullhost="${fullhost:-pinky}"
    assert-args fullhost @RET

    reval-ec rsp-safe ~/.codex/auth.json "${fullhost}:.codex/"
}
alias a2r='codex-auth2remote'

function codex-auth-from-remote {
    local fullhost="${fullhost:-pinky}"
    assert-args fullhost @RET

    reval-ec rsp-safe "${fullhost}:.codex/auth.json" ~/tmp/.codex-auths/"auth_$(str2filename-ascii ${fullhost}).json"
}
##
function codex-pioneer {
    #: @duplicateCode/60059a732ea0ddc623c1c01e78587d11
    ##
    local -x PIONEER_API_KEY="${pioneer_api_key}"

    codex \
        -c 'model_provider="pioneer"' \
        -c 'model="gpt-5.5"' \
        -c 'model_providers.pioneer.name="Pioneer"' \
        -c 'model_providers.pioneer.base_url="https://api.pioneer.ai/v1"' \
        -c 'model_providers.pioneer.wire_api="responses"' \
        -c 'model_providers.pioneer.env_key="PIONEER_API_KEY"' \
        -c 'model_providers.pioneer.request_max_retries=10' \
        -c 'model_providers.pioneer.stream_max_retries=10' \
        -c 'tui.status_line=["model-with-reasoning","current-dir","context-used"]' \
        -c 'features.plugins=false' \
        -c 'check_for_update_on_startup=false' \
        -c 'include_permissions_instructions=false' \
        "$@"
    #: [[id:1d18fd68-ab63-417f-9d17-93c965b48f5e][Codex + Pioneer: interactive =403 Forbidden=; root cause & mitigations]]
}
##

# Check whether Pioneer's /v1/responses streaming has been fixed for a model.
# Usage: pioneer-responses-test-streaming [gpt-5.5]
function pioneer-responses-test-streaming {
    emulate -L zsh
    local model="${1:-gpt-5.5}"
    local key="${PIONEER_API_KEY:-${pioneer_api_key}}"
    if [[ -z "$key" ]]; then
        print -u2 "pioneer-responses-test-streaming: no API key (set \$PIONEER_API_KEY or \$pioneer_api_key)"
        return 2
    fi

    local body
    body=$($proxyenv curl -s --max-time 60 \
        -N https://api.pioneer.ai/v1/responses \
        -H "Content-Type: application/json" \
        -H "Accept: text/event-stream" \
        -H "Authorization: Bearer ${key}" \
        -d "{\"model\":\"${model}\",\"input\":\"Reply with exactly STREAMOK\",\"stream\":true}" 2>&1)

    local delta_text
    delta_text=$(print -r -- "$body" \
        | grep '"type": "response.output_text.delta"' \
        | grep -oE '"delta": ?"[^"]*"' \
        | sed -E 's/"delta": ?"//; s/"$//' \
        | tr -d '\n')

    if [[ -n "$delta_text" ]]; then
        print -r -- "✅ $model: streaming FIXED — deltas received: ${delta_text}"
        return 0
    elif print -r -- "$body" | grep -q 'output_text.done'; then
        print -r -- "❌ $model: streaming STILL BROKEN — stream completed with NO text deltas (upstream bug)"
        return 1
    else
        print -r -- "⚠️  $model: no stream events — request error. Raw head:"
        print -r -- "$body" | head -c 600
        return 1
    fi
}
##
