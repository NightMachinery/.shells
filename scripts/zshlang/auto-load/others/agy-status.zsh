##
#: Antigravity's remaining quota, and picking a conversation back up when it
#: resets.
#:
#: The report is agent-quotas' (=agent-quotas agy-status=,
#: https://github.com/NightMachinery/agent-quotas): it reads the cache agy's
#: own statusline hook fills (=zshlang/wrappers/agy_statusline.dash=), and runs
#: =agy -p /usage= when that cache has nothing to say or under =deus=.
#: [agfi:agy-status] only maps this file's knobs onto its flags, so the
#: quota rules live in one place. What stays here is the shell's side:
#: installing the hook into agy's settings ([agfi:agy-statusline-install]),
#: and arming a job for when the quota comes back
#: ([agfi:agy-status-notify]).
#:
#: Everything after "when does it reset" -- arming the job, waiting the clock
#: out, deciding whether resuming is safe, delivering the resume text -- is
#: agent-neutral and lives in =agent-usage.zsh=. See =docs/agy_status.md= and
#: =docs/agent-usage-armed.md=.
##
#: =statusline= serves the cache; =slow= runs =agy -p /usage= now
#: (=agent-quotas agy-status --refresh=). An enum rather than a =_p= switch,
#: because "which way to read" is not a yes/no question.
typeset -g agy_status_method="${agy_status_method:-statusline}"
#: The cache the statusline hook writes. agent-quotas reads each account's
#: from its own config; this is what the hook and the launchers use.
#:
#: =zshlang/wrappers/agy_statusline.dash= reads this same name out of its
#: *environment*, so overriding it for a test has to be an assignment the hook
#: can see, not merely a shell-local one.
typeset -g agy_status_cache_file="${agy_status_cache_file:-${HOME}/tmp/.agy-status/quota-cache.json}"
#: With =agy_status_cache_only_p=y=: how old a cached bucket may be and still
#: be reported, in seconds; negative means any age. Other reads use
#: =statusline_max_age_s= from agent-quotas' config.
typeset -g agy_status_statusline_max_age_s="${agy_status_statusline_max_age_s:-3600}"
#: Where agy keeps the settings [agfi:agy-statusline-install] writes into. A
#: knob mostly so the installer can be exercised against a copy.
typeset -g agy_statusline_settings_file="${agy_statusline_settings_file:-${HOME}/.gemini/antigravity-cli/settings.json}"
#: Whether to keep agy's own statusline and stack ours under it, rather than
#: replacing it. agy's own key, =stack_with_default=.
typeset -g agy_statusline_stack_with_default_p="${agy_statusline_stack_with_default_p:-y}"
#: Print the current account's flat bucket array instead of the report, for a
#: caller that parses it ([agfi:h-agy-status-arm-deadline] is one). An
#: explicit =--json= still means the account objects.
typeset -g agy_status_json_p="${agy_status_json_p:-n}"
#: Also show each account's pay-as-you-go credit balance. Nothing caches it,
#: so it costs an `agy' start-up per account -- hence off by default.
typeset -g agy_status_credits_p="${agy_status_credits_p:-n}"
#: Read only the caches, never starting agy, requesting authentication or
#: contacting a backend: a missing or stale cache is that account's error.
#: For pollers (the quota watchers).
typeset -g agy_status_cache_only_p="${agy_status_cache_only_p:-n}"
#: Utilization at or above which a model group counts as blocking us. The only
#: knob of the armed job that is Antigravity's own.
typeset -g agy_status_arm_full_pct="${agy_status_arm_full_pct:-100}"
#: =auto= colours a terminal without NO_COLOR; =always= and =never= force it.
typeset -g agy_status_color="${agy_status_color:-auto}"
##
function agy-status {
    : "all configured accounts; --json for account objects, --account N to select one"
    #: =agent-quotas agy-status= with this file's knobs as flags. It asks the
    #: daemon, or reads in this process (=--direct=) when the daemon does not
    #: answer or =agent_quotas_p=n=. The accounts are those in agent-quotas'
    #: config. See =docs/agy_status.md=.
    ##
    local method="${agy_status_method:-statusline}"
    local columns="${agy_status_columns:-${COLUMNS:-80}}"

    if ! whence -p agent-quotas >/dev/null ; then
        ecerr "$0: needs agent-quotas: go install github.com/NightMachinery/agent-quotas/cmd/agent-quotas@latest"
        return 1
    fi

    local -a args=()
    case "${method}" in
        statusline) ;;
        slow) args+=(--refresh) ;;
        *)
            ecerr "$0: unknown agy_status_method: ${method} (statusline, slow)"
            return 2
            ;;
    esac

    local json_p=n
    (( ${@[(Ie)--json]} )) && json_p=y
    if bool "${agy_status_json_p:-n}" && ! bool "${json_p}" ; then
        args+=(--flat)
    elif bool "${agy_status_credits_p:-n}" && ! bool "${json_p}" ; then
        args+=(--credits)
    fi

    if bool "${agy_status_cache_only_p:-n}" ; then
        args+=(--cache-only --max-age "${agy_status_statusline_max_age_s:-3600}")
    elif ! h-agent-quotas-p ; then
        args+=(--direct)
    fi

    [[ "${columns}" == <1-> ]] || columns=80
    args+=(--columns "${columns}" --color "${agy_status_color:-auto}"
        --absolute "${agy_status_absolute_p:-auto}")

    command agent-quotas agy-status "${args[@]}" "$@"
}
aliasfn ags agy-status
aliasfn agys agy-status

#: Named methods, so a caller can demand one without knowing the knob. The
#: slow one is what to reach for when you need the number =agy= itself would
#: print.
aliasfnq agy-status-statusline agy_status_method=statusline agy-status
aliasfnq agy-status-slow agy_status_method=slow agy-status
##
function h-agy-status-write-atomic {
    #: Writes $2 into $1 through a temp file in the same directory.
    #: Usage: h-agy-status-write-atomic <file> <content>
    #:
    #: For the settings installer: agy re-reads its settings, and a
    #: half-written file would be read as a broken one rather than as an old
    #: one.
    ##
    local dest="${1}" content="${2}"
    assert-args dest content @RET

    ensure-cmd gmktemp gmv @RET

    assert mkdir -p -- "${dest:h}" @RET

    local tmp
    tmp="$(gmktemp --tmpdir="${dest:h}" "${dest:t}.XXXXXX")" @TRET

    #: The temp file is cleaned up on either failure: it sits next to the real
    #: file, where a leftover would be noise in a directory somebody reads.
    ec "${content}" > "${tmp}" || {
        local retcode=$?
        silent command rm -f -- "${tmp}" || true
        return "${retcode}"
    }

    assert gmv -f -- "${tmp}" "${dest}" || {
        local retcode=$?
        silent command rm -f -- "${tmp}" || true
        return "${retcode}"
    }
}
##
#: Installing the hook into agy's settings
##
function h-agy-statusline-command {
    #: The absolute path agy is told to run. One place, so the installer, the
    #: uninstaller and the predicate cannot disagree about what "ours" means.
    ##
    ec "${NIGHTDIR:-${HOME}/scripts}/zshlang/wrappers/agy_statusline.dash"
}

function h-agy-statusline-desired {
    #: The =statusLine= object we want in agy's settings, as JSON.
    #:
    #: The key names are agy's own: =type=, =command=, =enabled= and
    #: =stack_with_default=. Confirmed against Antigravity's CLI documentation
    #: and against the changelog agy itself prints (`agy changelog'), which is
    #: where =stack_with_default= was announced. =padding= is a fifth key and is
    #: deliberately left unset, so agy's own default stands.
    ##
    local stack_p="${agy_statusline_stack_with_default_p:-y}"

    ensure-cmd jq @RET

    local stack=false
    bool "${stack_p}" && stack=true

    local cmd
    cmd="$(h-agy-statusline-command)" @TRET

    jq -n --arg command "${cmd}" --argjson stack "${stack}" \
        '{type: "command", command: $command, enabled: true, stack_with_default: $stack}'
}

function h-agy-statusline-current-command {
    #: The command agy is currently configured to run for its statusline, or
    #: nothing at all. The FIRST word only: an installed command may carry
    #: arguments (upstream's own renderer documents a =--classic= flag that
    #: way), and it is still the same command.
    ##
    local settings_file="${agy_statusline_settings_file:-${HOME}/.gemini/antigravity-cli/settings.json}"

    ensure-cmd jq @RET

    test -r "${settings_file}" || return 0

    #: The trailing `// ""' is not redundant: jq's `"" | split(" ")' is the
    #: EMPTY array, so indexing it yields null, and `jq -r' prints null as the
    #: four characters "null" -- which would then read as a foreign statusline
    #: to every caller here.
    jq -r '((.statusLine.command // "") | split(" ")[0]) // ""' "${settings_file}"
}

function agy-statusline-installed-p {
    #: Whether agy's statusline is pointed at OUR hook. Quiet: the exit status
    #: is the whole answer.
    #: See =docs/agy_status.md=.
    ##
    local current ours
    current="$(h-agy-statusline-current-command)" @TRET
    ours="$(h-agy-statusline-command)" @TRET

    test -n "${current}" && [[ "${current}" == "${ours}" ]]
}

function agy-statusline-install {
    #: Points agy's =statusLine= at our hook, idempotently.
    #: See =docs/agy_status.md=.
    ##
    local settings_file="${agy_statusline_settings_file:-${HOME}/.gemini/antigravity-cli/settings.json}"

    ensure-cmd jq @RET

    local ours desired
    ours="$(h-agy-statusline-command)" @TRET
    desired="$(h-agy-statusline-desired)" @TRET

    if ! test -x "${ours}" ; then
        #: Caught here rather than by agy, which would only disable the
        #: statusline after a few silent failures.
        ecerr "$0: the hook is missing or not executable: ${ours}"
        return 1
    fi

    local existing='{}'
    if test -e "${settings_file}" ; then
        #: Refused rather than overwritten: the file is agy's, and everything
        #: in it but our one key is the user's.
        silent jq -e . "${settings_file}" || {
            ecerr "$0: not valid JSON, refusing to touch it: ${settings_file}"
            return 1
        }

        existing="$(command cat -- "${settings_file}")" @TRET
    fi

    local current
    current="$(h-agy-statusline-current-command)" @TRET

    if test -n "${current}" && [[ "${current}" != "${ours}" ]] ; then
        if ! isDeus ; then
            ecerr "$0: a statusLine is already configured, and it is not ours:"
            ecerr "    ${current}"
            ecerr "  Run \`deus ${0}' to replace it, or remove it from agy with \`/statusline delete'."
            return 1
        fi

        ecgray "$0: deus: replacing a foreign statusLine: ${current}"
    fi

    if agy-statusline-installed-p ; then
        local installed
        installed="$(jq -Sc '.statusLine // {}' "${settings_file}")" @TRET

        local wanted
        wanted="$(ec "${desired}" | jq -Sc .)" @TRET

        if [[ "${installed}" == "${wanted}" ]] ; then
            #: Said out loud, so that running this twice is visibly a no-op
            #: rather than an invisible rewrite of a file we back up only once.
            ecgray "$0: already installed: ${ours}"
            return 0
        fi
    fi

    #: Once, and only before the first modification. A backup taken on every
    #: run would eventually be a backup of our own output, which is no backup.
    local backup="${settings_file}.agy-statusline.bak"
    if test -e "${settings_file}" && ! test -e "${backup}" ; then
        assert mkdir -p -- "${settings_file:h}" @RET
        assert cp -- "${settings_file}" "${backup}" @RET
        ecgray "$0: backed the original up to ${backup}"
    fi

    #: Read-modify-write of the whole document: every other key survives, and
    #: jq is what decides what well-formed means, not us.
    local updated
    updated="$(ec "${existing}" | jq --argjson statusline "${desired}" \
        '. + {statusLine: $statusline}')" @TRET

    h-agy-status-write-atomic "${settings_file}" "${updated}" @RET

    ecgray "$0: installed: ${ours}"
    ecgray "$0: agy picks it up on its next start; \`agy-status' then reads ${agy_status_cache_file:-${HOME}/tmp/.agy-status/quota-cache.json}"
}

function agy-statusline-uninstall {
    #: Removes OUR statusLine entry from agy's settings, and only ours.
    #: See =docs/agy_status.md=.
    ##
    local settings_file="${agy_statusline_settings_file:-${HOME}/.gemini/antigravity-cli/settings.json}"

    ensure-cmd jq @RET

    if ! test -e "${settings_file}" ; then
        ecgray "$0: nothing to remove: ${settings_file} does not exist"
        return 0
    fi

    silent jq -e . "${settings_file}" || {
        ecerr "$0: not valid JSON, refusing to touch it: ${settings_file}"
        return 1
    }

    local current ours
    current="$(h-agy-statusline-current-command)" @TRET
    ours="$(h-agy-statusline-command)" @TRET

    if test -z "${current}" ; then
        ecgray "$0: not installed; nothing to remove"
        return 0
    fi

    if [[ "${current}" != "${ours}" ]] ; then
        #: Never `deus'-overridable, unlike the install: removing somebody
        #: else's statusline is not a thing we should be able to be talked
        #: into, and agy's own `/statusline delete' is right there.
        ecgray "$0: leaving a statusLine that is not ours alone: ${current}"
        return 0
    fi

    local updated
    updated="$(jq 'del(.statusLine)' "${settings_file}")" @TRET

    h-agy-status-write-atomic "${settings_file}" "${updated}" @RET

    ecgray "$0: uninstalled"
}
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
    #:
    #: `unique' on the groups, because a group can contribute two rows rather
    #: than one: the statusline path reports the five-hour window beside the
    #: weekly one, and both being spent must not spell the group's name twice
    #: in a notification.
    local out=''
    out="$(ec "${json}" | jq -er --argjson max "${max_remaining}" '
        [.[] | select(.remaining_percent <= $max)]
        | select(length > 0)
        | [(map(.resets_at) | min), (map(.group) | unique | join(", "))]
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
aliasfnq agy-status-auto-continue-kitty-fz \
    agent_usage_arm_action=continue agent_usage_continue_via=kitty \
    agy-status-notify

aliasfnq agy-status-auto-continue-tmux-fz \
    agent_usage_arm_action=continue agent_usage_continue_via=tmux \
    agy-status-notify

aliasfnq agy-status-auto-continue-frontmost \
    agent_usage_arm_action=continue agent_usage_continue_via=frontmost \
    agy-status-notify

aliasfn agyk agy-status-auto-continue-kitty-fz
aliasfn agyt agy-status-auto-continue-tmux-fz
aliasfn agyfront agy-status-auto-continue-frontmost
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
