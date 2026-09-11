##
#: Antigravity's remaining quota, and picking a conversation back up when it
#: resets.
#:
#: Two implementations of the same report, chosen by [agfi:agy_status_method]:
#:
#: - =direct= ([agfi:h-agy-status-direct]) asks the backend endpoint behind
#:   the numbers itself, with the OAuth token `agy' keeps in the login
#:   Keychain. One or two HTTP round trips, no CLI start-up. The work is in
#:   =python/agy_status.py=.
#: - =slow= ([agfi:h-agy-status-slow]) runs =agy -p /usage= (alias =/quota=),
#:   which prints one tab separated row per model group. Per the vendor's
#:   changelog it spends no quota and leaves no conversation behind, which is
#:   what makes it safe to call from a report you run all day. It is still a
#:   full CLI start-up plus five sequential backend round trips, so it runs
#:   under a timeout and in a throwaway directory -- Antigravity reads the
#:   cwd's project context, and a report must not attach itself to whatever
#:   repository you happen to be standing in.
#:
#: The fast path never falls back to the slow one on its own: a fallback that
#: fires silently is a breakage nobody notices for months. It says what failed
#: and names =agy-status-slow=, which is one word to type.
#:
#: Everything after "when does it reset" -- arming the job, waiting the clock
#: out, deciding whether resuming is safe, delivering the resume text -- is
#: agent-neutral and lives in =agent-usage.zsh=. See =docs/agy_status.md= and
#: =docs/agent-usage-armed.md=.
##
#: Which implementation [agfi:agy-status] runs. An enum rather than a
#: =_p= switch, because there is nothing boolean about "which of these two".
typeset -g agy_status_method="${agy_status_method:-slow}"
#: How long =agy -p= gets before we give up on it. The slow path's alone: it
#: is sized for a cold CLI start-up, which is far too generous for one HTTP
#: request.
typeset -g agy_status_timeout_s="${agy_status_timeout_s:-60}"
#: The direct path's per-request timeout.
typeset -g agy_status_direct_timeout_s="${agy_status_direct_timeout_s:-20}"
#: Whether the direct path delegates to the brish garden; see
#: [agfi:h-agy-status-garden-p] for what =auto= decides and why.
typeset -g agy_status_garden_p="${agy_status_garden_p:-auto}"
#: Emit the report as a JSON array instead of prose, for a caller that parses
#: it ([agfi:h-agy-status-arm-deadline] is one).
typeset -g agy_status_json_p="${agy_status_json_p:-n}"
#: Also show the pay-as-you-go credit balance. There is no endpoint of ours
#: for it, so it costs an `agy' start-up on either path -- hence off by
#: default.
typeset -g agy_status_credits_p="${agy_status_credits_p:-n}"
#: Utilization at or above which a model group counts as blocking us. The only
#: knob of the armed job that is Antigravity's own.
typeset -g agy_status_arm_full_pct="${agy_status_arm_full_pct:-100}"
#: Colour of the prose report: =auto= is "colour iff our stdout is a terminal",
#: resolved by [agfi:h-color-mode-p]. An enum rather than a switch,
#: because a caller that has already decided has to be able to say so:
#: [agfi:agent-status] runs us inside the garden, where stdout is a pipe no
#: matter what the user is looking at. The JSON never gets colour.
typeset -g agy_status_color="${agy_status_color:-auto}"
##
function h-agy-status-run {
    #: Runs one of Antigravity's print-mode slash commands and prints its
    #: output verbatim. The slow path's engine, and the only way to the
    #: credits, which have no endpoint of their own here.
    #: Usage: h-agy-status-run </usage|/credits>
    ##
    local timeout_s="${agy_status_timeout_s:-60}"

    local slash_cmd="${1}"
    assert-args slash_cmd @RET

    ensure-cmd agy @RET

    #: GNU coreutils first, since that is what the rest of zshlang assumes;
    #: plain =timeout= where the system ships it unprefixed. Running without
    #: one is still better than refusing to report at all.
    local -a runner=()
    if isdefined-cmd gtimeout ; then
        runner=(command gtimeout "${timeout_s}")
    elif isdefined-cmd timeout ; then
        runner=(command timeout "${timeout_s}")
    fi

    local tmp_dir
    tmp_dir="$(gmktemp -d)" @TRET

    #: `builtin cd' in the subshell only: the caller's directory is never
    #: touched, and no wrapper of `cd' gets a say. There are no chpwd hooks in
    #: zshlang, so nothing prints into the output we are capturing.
    local out='' ret=0
    out="$(builtin cd -- "${tmp_dir}" && "${runner[@]}" command agy -p "${slash_cmd}")" || ret=$?

    silent command rm -rf -- "${tmp_dir}" || true

    if (( ret != 0 )) ; then
        if (( ret == 124 )) ; then
            ecerr "$0: \`agy -p ${slash_cmd}' timed out after ${timeout_s}s"
        else
            ecerr "$0: \`agy -p ${slash_cmd}' failed (${ret})"
        fi

        return "${ret}"
    fi

    ec "${out}"
}

function h-agy-status-rows {
    #: One normalized row per model group:
    #:   group \t label \t remaining_percent \t reset_epoch \t reset_iso
    #:
    #: The upstream percent is what is LEFT, not what is spent -- the opposite
    #: of Claude Code's =utilization_percent= -- and the name here says so, so
    #: that nobody reads 98% as "nearly exhausted".
    ##
    ensure-cmd gdate @RET

    local raw
    raw="$(h-agy-status-run /usage)" @TRET

    local line pct iso epoch
    local -a f
    for line in "${(@f)raw}" ; do
        test -n "${line}" || continue

        #: `(ps:\t:)' rather than `read': tab is IFS whitespace, so `read'
        #: would collapse an empty field rather than keep the columns lined up.
        f=( "${(@ps:\t:)line}" )
        if (( ${#f} < 4 )) ; then
            #: A banner, a warning, an upsell line: anything that is not a
            #: quota row. Skipped rather than fatal, so one new line of chrome
            #: upstream does not take the report down with it.
            ecgray "$0: skipping unrecognized row: ${line}"
            continue
        fi

        pct="${f[3]%\%}"
        if [[ "${pct}" != <->(|.<->) ]] ; then
            ecgray "$0: skipping row with a non-numeric percent: ${line}"
            continue
        fi

        iso="${f[4]}"
        #: `gdate' because BSD date cannot read an ISO-8601 string without
        #: being told its format first.
        epoch="$(gdate -d "${iso}" +%s)" @TRET

        printf '%s\t%s\t%s\t%s\t%s\n' "${f[1]}" "${f[2]}" "${pct}" "${epoch}" "${iso}"
    done
}

function h-agy-status-json {
    #: Turns [agfi:h-agy-status-rows] on stdin into the JSON array
    #: =agy_status_json_p= promises.
    ##
    ensure-cmd jq @RET

    local line obj
    local -a f objects=()
    while IFS= read -r line ; do
        test -n "${line}" || continue
        f=( "${(@ps:\t:)line}" )

        #: `--arg'/`--argjson' throughout: a group name is upstream's text and
        #: must not be able to reach the jq program as syntax.
        obj="$(jq -n \
            --arg group "${f[1]}" \
            --arg label "${f[2]}" \
            --argjson remaining "${f[3]}" \
            --argjson resets "${f[4]}" \
            --arg iso "${f[5]}" \
            '{group: $group, label: $label, remaining_percent: $remaining, resets_at: $resets, resets_at_iso: $iso}')" @TRET
        objects+=("${obj}")
    done

    #: `-s' rather than building the array in zsh, so the result is jq's own
    #: idea of well-formed rather than ours.
    ec "${(F)objects}" | jq -s .
}

function h-agy-status-credits {
    #: The pay-as-you-go balance, as `Label: value' lines. Its rows are
    #: two-column (`Remaining credits', `Upgrade'), so they do not go through
    #: [agfi:h-agy-status-rows].
    ##
    local raw
    raw="$(h-agy-status-run /credits)" @TRET

    local line
    local -a f
    for line in "${(@f)raw}" ; do
        test -n "${line}" || continue

        f=( "${(@ps:\t:)line}" )
        if (( ${#f} < 2 )) ; then
            ec "${line}"
            continue
        fi

        ec "${f[1]}: ${f[2]}"
    done
}

function h-agy-status-slow {
    #: What is left of Antigravity's quota, per model group, and when it comes
    #: back, by asking =agy= itself. Reachable as =agy-status-slow=.
    #: See =docs/agy_status.md=.
    ##
    local json_p="${agy_status_json_p:-n}"
    local credits_p="${agy_status_credits_p:-n}"

    if (( $# > 0 )) ; then
        #: This path is driven entirely by knobs; only the direct one forwards
        #: arguments, to =agy_status.py=. Said out loud rather than dropped, so
        #: a =--host= aimed at the wrong implementation is visible.
        ecgray "$0: ignoring arguments: $*"
    fi

    local rows
    rows="$(h-agy-status-rows)" @TRET
    if test -z "${rows}" ; then
        ecerr "$0: no quota rows in \`agy -p /usage'"
        return 1
    fi

    if bool "${json_p}" ; then
        #: Credits are deliberately not in the JSON: they are a different
        #: currency from the quota windows, and nothing parses them.
        ec "${rows}" | h-agy-status-json
        return $?
    fi

    zmodload zsh/datetime 2>/dev/null

    #: Decided once, here, rather than per line: `auto' has to test the stdout
    #: of the report as a whole. Note the call is *not* in a command
    #: substitution -- see [agfi:h-color-mode-p] for why that matters.
    local color=never
    h-color-mode-p "${agy_status_color:-auto}" && color=always

    #: Raw SGR strings from zsh's own `colors', not [agfi:colorfg]: that one
    #: re-decides for itself from the environment ([agfi:isColor],
    #: [agfi:true-color-p]), which is the decision the knob above exists to
    #: take away from it.
    local c_group='' c_when='' c_off='' c_pct=''
    local -A pct_colors=()
    if [[ "${color}" == always ]] ; then
        c_group="${fg_bold[white]}"
        c_when="${fg[blue]}"
        c_off="${reset_color}"
        pct_colors=(
            low "${fg_bold[red]}"
            mid "${fg[yellow]}"
            high "${fg[green]}"
        )
    fi

    local line when
    local -a f
    integer remaining_s
    for line in "${(@f)rows}" ; do
        f=( "${(@ps:\t:)line}" )

        remaining_s=$(( f[4] - EPOCHSECONDS ))
        if (( remaining_s > 0 )) ; then
            when="resets $(date-unix-to-3339 "${f[4]}") (in $(seconds-fmt-short ${remaining_s}))"
        else
            #: `agy' reports a window's end even after it has passed, until
            #: the next call refreshes it.
            when="reset $(seconds-fmt-short $(( -remaining_s ))) ago"
        fi

        #: Presentation only, so deliberately not knobs: nothing branches on
        #: these but the escape codes, and [agfi:agy_status_arm_full_pct] is
        #: the one threshold that actually decides anything.
        if (( f[3] <= 10 )) ; then
            c_pct="${pct_colors[low]}"
        elif (( f[3] <= 33 )) ; then
            c_pct="${pct_colors[mid]}"
        else
            c_pct="${pct_colors[high]}"
        fi

        ec "${c_group}${f[1]}${c_off} (${f[2]}): ${c_pct}${f[3]}% remaining${c_off}, ${c_when}${when}${c_off}"
    done

    if bool "${credits_p}" ; then
        h-agy-status-credits
    fi
}
##
#: The direct path
##
function h-agy-status-garden-p {
    #: Whether to run the direct report inside the brish garden rather than
    #: here.
    #:
    #: The garden's worker shells are attached to the GUI session, so they can
    #: read the login keychain, which is where the OAuth token lives. A
    #: GUI-detached session cannot: its keychain search list collapses to the
    #: System keychain alone, so the lookup finds nothing and =security=
    #: reports the credential as simply absent. See =docs/agy_status.md=.
    #:
    #: Proactive rather than a retry, because over ssh the local read *cannot*
    #: succeed -- attempting it first would only buy a round trip and a
    #: misleading error. The same shape as
    #: [agfi:h-claude-code-usage-garden-p], for the same reason.
    ##
    local mode="${agy_status_garden_p:-auto}"

    case "${mode}" in
        y) return 0 ;;
        n) return 1 ;;
        auto) isSSH ;;
        *)
            ecerr "$0: unknown agy_status_garden_p: ${mode} (auto, y, n)"
            return 1
            ;;
    esac
}

function h-agy-status-run-direct {
    #: The one place =agy_status.py= is invoked, so the garden delegation
    #: cannot be forgotten by a caller.
    #:
    #: The whole invocation is delegated, not just the keychain read, so the
    #: token never leaves the GUI-attached process: only the rendered report or
    #: the JSON array comes back. See =docs/agy_status.md=.
    ##
    if h-agy-status-garden-p && brishz-alive-p ; then
        local -a color_opts=()
        #: The garden's stdout is a pipe, so =--color auto= would resolve to no
        #: colour for the command run most often. Placed first, so the
        #: =--color= our caller resolved still wins (argparse is last-wins).
        [[ -t 1 ]] && color_opts=(--color always)

        #: Non-ASCII survives the trip this way; the inline transport mangles
        #: it.
        brishz_out_file_p=y brishzq.zsh agy_status.py "${color_opts[@]}" "$@"
        return $?
    fi

    $proxyenv revaldbg command agy_status.py "$@"
}

function h-agy-status-direct {
    #: The same report as [agfi:h-agy-status-slow], read from the backend
    #: rather than from =agy=. Reachable as =agy-status-direct=.
    #: See =docs/agy_status.md=.
    ##
    local json_p="${agy_status_json_p:-n}"
    local credits_p="${agy_status_credits_p:-n}"
    local timeout_s="${agy_status_direct_timeout_s:-20}"

    ensure-cmd agy_status.py @RET

    #: Decided here rather than left to the script's own =auto=: the knob is
    #: about *our* stdout, and the script may well be running in the garden,
    #: where stdout is a pipe whatever the user is looking at. Note the call is
    #: not in a command substitution -- see [agfi:h-color-mode-p] for why that
    #: matters.
    local color=never
    h-color-mode-p "${agy_status_color:-auto}" && color=always

    local -a script_args=(--timeout "${timeout_s}" --color "${color}")
    if bool "${json_p}" ; then
        script_args+=(--json)
    fi

    local retcode=0
    #: Our own arguments come last, so anything the caller passes -- =--host=,
    #: =--no-relogin= -- overrides what we decided.
    h-agy-status-run-direct "${script_args[@]}" "$@" || retcode=$?

    if (( retcode == 0 )) && bool "${credits_p}" && ! bool "${json_p}" ; then
        #: Credits are the one number with no endpoint of ours, so this
        #: addendum is still an =agy= start-up even on the fast path. It is off
        #: by default, and asking for it is asking for that cost.
        #:
        #: Deliberately not in the JSON, exactly as on the slow path: they are
        #: a different currency from the quota windows, and nothing parses them.
        h-agy-status-credits
    fi

    return "${retcode}"
}
##
function agy-status {
    #: What is left of Antigravity's quota, per bucket, and when it comes back.
    #: The gateway: [agfi:agy_status_method] picks the implementation.
    #: See =docs/agy_status.md=.
    ##
    local method="${agy_status_method:-slow}"

    case "${method}" in
        direct)
            h-agy-status-direct "$@"
            ;;
        slow)
            h-agy-status-slow "$@"
            ;;
        *)
            ecerr "$0: unknown agy_status_method: ${method} (direct, slow)"
            return 1
            ;;
    esac
}
aliasfn agys agy-status

#: Named implementations, so a caller can demand one without knowing the knob.
#: The slow one is what a failing direct report names, and what to reach for
#: when you need the number =agy= itself would print.
aliasfnq agy-status-direct agy_status_method=direct agy-status
aliasfnq agy-status-slow agy_status_method=slow agy-status
##
#: Waiting the quota out
##
function h-agy-status-arm-deadline {
    #: Prints "<reset-epoch>\t<msg>" for the reset worth waiting on, or
    #: nothing at all when there is nothing to wait for.
    #:
    #: Split out from [agfi:h-agy-status-arm] because it is the only part
    #: with a decision in it, and the only part that can be exercised without
    #: arming a real job.
    ##
    local full_pct="${agy_status_arm_full_pct:-100}"

    ensure-cmd jq @RET

    local json
    json="$(agy_status_json_p=y agy-status)" @TRET

    #: The percent is what REMAINS, so a group is spent when at most
    #: `100 - full_pct' of it is left: the default 100 means "nothing left".
    integer max_remaining=$(( 100 - full_pct ))

    #: The EARLIEST reset among the spent groups, unlike Claude Code's latest:
    #: these are independent quotas rather than nested windows, so the first
    #: one back is genuinely usable. Hence also naming the group in the
    #: message -- which quota returned is the whole of what changed.
    local out=''
    out="$(ec "${json}" | jq -er --argjson max "${max_remaining}" '
        [.[] | select(.remaining_percent <= $max)]
        | select(length > 0)
        | [(map(.resets_at) | min), (map(.group) | join(", "))]
        | @tsv')" || out=''

    if test -n "${out}" ; then
        printf '%s\t%s\n' "${out%%$'\t'*}" \
            "Antigravity: ${out#*$'\t'} quota reset, usage available again"
        return 0
    fi

    if ! isDeus ; then
        ecgray "$0: usage already possible, not arming (use \`deus\` to arm anyway)"
        return 0
    fi

    #: deus: arm for whichever group rolls over first anyway, so the mechanism
    #: can be exercised without having to be rate-limited first.
    out="$(ec "${json}" | jq -er '
        min_by(.resets_at) | [.resets_at, .group] | @tsv')" @TRET

    printf '%s\t%s\n' "${out%%$'\t'*}" \
        "Antigravity: ${out#*$'\t'} quota window rolled over"
}

function h-agy-status-arm {
    #: Arms, or re-arms, a one-shot job for when Antigravity's quota comes
    #: back. $1 is the tmux session it lives in.
    ##
    local session="${1}"
    assert-args session @RET

    local out
    out="$(h-agy-status-arm-deadline)" @TRET
    #: Nothing to wait for; the reason was already said on stderr.
    test -n "${out}" || return 0

    #: Only Antigravity sessions are offered, unlike Claude Code's kitty
    #: picker: there is no cross-agent story here -- an Antigravity quota
    #: reset unblocks Antigravity conversations and nothing else. `local' is
    #: dynamically scoped in zsh, so the picker sees it without anything being
    #: exported.
    local agent_session_agents=agy

    h-agent-usage-arm "${session}" "${out%%$'\t'*}" "${out#*$'\t'}"
}

function h-agy-status-arm-schedule {
    #: Arms without printing a report, matching Claude Code's
    #: =h-...-arm-schedule= escape hatch. The tmux session it lives in is
    #: named once, here and in [agfi:agy-status-armed-sessions].
    ##
    h-agy-status-arm 'agy-status-armed'
}

function agy-status-notify {
    #: The quota report, then an armed job for when it comes back -- the
    #: intended way in, mirroring [agfi:claude-code-usage-fable-notify].
    ##
    local retcode=0
    agy-status "$@" || retcode=$?

    if (( retcode == 0 )) ; then
        #: =>&2= because our stdout may be a JSON document a caller is about
        #: to parse; non-fatal because a failed schedule must not make a
        #: working report look broken.
        h-agy-status-arm-schedule >&2 || true
    fi

    return "${retcode}"
}
##
#: Resuming rather than merely announcing, named after the delivery mechanism
#: exactly as Claude Code's are; see
#: [agfi:h-agent-usage-continue-targets].
##
aliasfnq agy-status-continue-kitty-fz \
    agent_usage_arm_action=continue agent_usage_continue_via=kitty \
    agy-status-notify

aliasfnq agy-status-continue-tmux-fz \
    agent_usage_arm_action=continue agent_usage_continue_via=tmux \
    agy-status-notify

aliasfnq agy-status-continue-frontmost \
    agent_usage_arm_action=continue agent_usage_continue_via=frontmost \
    agy-status-notify

aliasfn agyk agy-status-continue-kitty-fz
aliasfn agyt agy-status-continue-tmux-fz
aliasfn agyfront agy-status-continue-frontmost
##
function agy-status-armed-sessions {
    #: Every tmux session an Antigravity armed job can live in, one per line.
    #: One today; a function anyway, so the cancel/status wrappers below ask
    #: rather than each spelling the name out.
    #:
    #: [agfi:agent-usage-armed-sessions] calls this when it is defined and
    #: skips it otherwise, so the name lives here only and a renamed session
    #: needs changing in one place.
    ##
    ec 'agy-status-armed'
}

#: Cancelling and reporting are the same act whatever armed the job, so both
#: are [agfi:h-agent-usage-armed-cancel] and [agfi:h-agent-usage-armed-status]
#: over the sessions this family owns. Named arguments still narrow it to one
#: session.
function agy-status-armed-cancel {
    local sessions=("$@")
    if (( ${#sessions} == 0 )) ; then
        sessions=("${(@f)$(agy-status-armed-sessions)}")
    fi

    h-agent-usage-armed-cancel "${sessions[@]}"
}

function agy-status-armed-status {
    local sessions=("$@")
    if (( ${#sessions} == 0 )) ; then
        sessions=("${(@f)$(agy-status-armed-sessions)}")
    fi

    h-agent-usage-armed-status "${sessions[@]}"
}
##
