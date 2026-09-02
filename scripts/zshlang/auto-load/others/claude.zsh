##
function claude {
    #: @duplicateCode/fd706e6b8475e27ca5cf27951b1d8ddc
    ##
    local -x EDITOR=nvim
    local -x VISUAL="${EDITOR}"
    #: not sure if EDITOR is actually used

    local -x CLAUDE_CODE_MAX_RETRIES=2147483647

    #: [[https://code.claude.com/docs/en/monitoring-usage][Monitoring - Claude Code Docs]]
    # Make stalled streaming connections fail/retry instead of hanging forever-ish.
    local -x CLAUDE_ENABLE_STREAM_WATCHDOG=1
    local -x CLAUDE_ENABLE_BYTE_WATCHDOG=1
    # local -x CLAUDE_STREAM_IDLE_TIMEOUT_MS=90000
    local -x CLAUDE_CODE_DISABLE_FEEDBACK_SURVEY=1

    # Debug logging. Use a stable per-run file so you can tail it.
    local debug_file="${HOME}/tmp/claude-code/${EPOCHSECONDS}.debug.log"
    local -x CLAUDE_CODE_DEBUG_LOG_LEVEL=verbose

    #: Keeps ~/.claude/CLAUDE.md current with its sources; asks before
    #: launching with stale instructions.
    h-agents-md-sync-ask @RET

    #: The marker is how a work tab is told apart from a personal one; see
    #: [agfi:claude-work]. `local` is dynamically scoped in zsh, so a caller
    #: can set it without exporting anything.
    tty-title "${claude_tty_title_marker:-🍼}${PWD:t}"

    $proxyenv command claude "$@"
}
aliasfn claude-m claude
##
function claude-autocommit {
    local -x ANTHROPIC_MODEL="sonnet"

     reval-ec claude-m --verbose -p 'git-committer' --allowedTools 'Bash(git:*)'

     ecgray
     reval-ecgray glola 5
}
##
function claude-vcsh-commit {
    local target_dir="${1:-$NIGHTDIR}"
    local engine=("${claude_commit_engine[@]:-claude-m}")

    (
        local -x ANTHROPIC_MODEL="sonnet"

        cd "$target_dir" @RET
    
        reval-ecgray "${engine[@]}" -p "Read '${NIGHTDIR}/AGENTS.md' and '${NIGHTDIR}/PE/vcsh-commit.md' and start committing changes." --verbose --allowedTools 'Bash(vcsh night.sh:*)'

        ecgray
        reval-ecgray vcn-with glola 5
    )
}

function claude-night-sh {
    (
        cd "$NIGHTDIR" @RET
        
        claude-m "${NIGHTDIR}/prompt/night-sh.md"
    )
}
##
function claude-pioneer {
    local -x ANTHROPIC_AUTH_TOKEN="${pioneer_api_key}"
    local -x ANTHROPIC_BASE_URL="https://api.pioneer.ai/"

    claude "$@"
}

# aliasfn claude-m claude-pioneer
##
function claude-freemodel {
    local -x ANTHROPIC_AUTH_TOKEN="${freemodel_api_key}"
    local -x ANTHROPIC_API_KEY="${freemodel_api_key}"
    local -x ANTHROPIC_BASE_URL="https://cc.freemodel.dev"
    local -x CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC=1

    claude "$@"
}
##
function claude-highwayai {
    local -x ANTHROPIC_AUTH_TOKEN="NA"
    local -x ANTHROPIC_API_KEY="NA"
    local -x ANTHROPIC_BASE_URL="https://freeapi.highwayapi.ai/anthropic"
    local -x CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC=1

    claude "$@"
}
##
#: A profile's =CLAUDE_CONFIG_DIR=, or the empty string for the default profile,
#: which has no =CLAUDE_CONFIG_DIR= and keeps its config at =~/.claude.json=.
#: Everything else -- config file, Keychain service, cache dir -- derives from
#: this, so registering a profile is one line here. See [agfi:claude-work].
typeset -gA claude_code_profiles=(
    default  ''
    work     "${HOME}/.claude-work"
)
#: Iteration and display order for [agfi:claude-code-usage-all]; an associative
#: array has no order of its own.
typeset -ga claude_code_profile_order=( default work )

function h-claude-code-profile-assert {
    local profile="${1}"
    assert-args profile @RET

    if (( ${+claude_code_profiles[$profile]} == 0 )) ; then
        #: Listed from the ordered array, not the assoc, whose key order is
        #: arbitrary.
        ectrace "$0: unknown profile: ${profile} (known: ${(j:, :)claude_code_profile_order})"
        return 1
    fi
}

function claude-code-usage {
    #: Shows the usage stats of one Claude Code profile's plan (like the in-app
    #: =/usage=). [agfi:claude-code-usage-all] does every registered profile at
    #: once, and is what the bare =ccu=/=ccs= aliases run.
    #: See =docs/claude_code_usage.md=.
    ##
    local profile="${claude_code_usage_profile:-default}"
    local timeout_s="${claude_code_usage_timeout_s:-10}"
    local cache_ttl_s="${claude_code_usage_cache_ttl_s:-300}"
    local refresh_p="${claude_code_usage_refresh_p:-n}"
    local json_p="${claude_code_usage_json_p:-n}"
    local strip_ansi_p="${claude_code_usage_strip_ansi_p:-n}"
    local notif_p="${claude_code_usage_notif_p:-y}"

    ensure-cmd claude_code_usage.py @RET
    h-claude-code-profile-assert "${profile}" @RET

    #: Per-profile cache dir: profiles share the endpoint but not the account,
    #: so one shared cache file would have them overwrite each other.
    local script_args=(
        --profile-label "${profile}"
        --config-dir "${claude_code_profiles[$profile]}"
        --cache-dir "${HOME}/tmp/.claude-usage/${profile}"
        --timeout "${timeout_s}"
        --cache-ttl "${cache_ttl_s}"
    )
    if bool "${refresh_p}" ; then
        script_args+=(--refresh)
    fi
    if bool "${json_p}" ; then
        script_args+=(--json)
    fi
    if bool "${strip_ansi_p}" ; then
        script_args+=(--color never)
    fi

    #: =script_args= before user args so explicit CLI flags win (argparse last-wins).
    local retcode=0
    $proxyenv revaldbg command claude_code_usage.py "${script_args[@]}" "$@" || retcode=$?

    if (( retcode == 0 )) && bool "${notif_p}" ; then
        #: After the report, so the human output is not held up and the
        #: notifier reads the cache this call has just written.
        #:
        #: =>&2= because our stdout may be a JSON document that a caller is
        #: about to parse; and never fatal, since a failed arm must not make a
        #: working usage report look broken.
        h-claude-code-usage-notif-for-profile "${profile}" >&2 || true
    fi

    return "${retcode}"
}

function claude-code-usage-work {
    claude_code_usage_profile=work claude-code-usage "$@"
}
aliasfn claude-code-status-work claude-code-usage-work
alias ccu-work='claude-code-usage-work'
alias ccs-work='claude-code-usage-work'

function claude-code-usage-all {
    #: Every registered profile. Fetched in parallel -- each profile is a
    #: separate account and a separate request, so there is nothing to
    #: serialize -- but printed in =claude_code_profile_order= so the output
    #: does not shuffle with whichever request finished first.
    ##
    local profiles=("${claude_code_profile_order[@]}")
    assert-args profiles @RET

    local json_p="${claude_code_usage_json_p:-n}"
    local arg
    for arg in "$@" ; do
        #: =-all= splices several reports together, so it has to know whether
        #: they are JSON regardless of how that was asked for.
        if [[ "${arg}" == '--json' ]] ; then
            json_p=y
        fi
    done

    local tmp_dir
    tmp_dir="$(gmktemp -d)" @TRET

    {
        local p
        for p in "${profiles[@]}" ; do
            (
                claude_code_usage_profile="${p}" claude-code-usage "$@" \
                    >"${tmp_dir}/${p}.out" 2>"${tmp_dir}/${p}.err"
                ec "$?" >"${tmp_dir}/${p}.ret"
            ) &
        done
        wait

        local retcode=0 sep='' out=() ret=''
        for p in "${profiles[@]}" ; do
            out+=("${tmp_dir}/${p}.out")

            ret="$(<"${tmp_dir}/${p}.ret")" || ret=1
            if [[ "${ret}" != 0 ]] ; then
                #: One dead profile must not cost us the others' reports.
                retcode=1
                ecerr "$0: profile ${p} failed:"
                command cat -- "${tmp_dir}/${p}.err" >&2
            fi
        done

        if bool "${json_p}" ; then
            ensure-cmd jq @RET

            command cat -- "${out[@]}" | jq -s '.'
            #: Slurped into an array: two bare objects in a row are not JSON.
        else
            for p in "${profiles[@]}" ; do
                ecn "${sep}"
                sep=$'\n'

                command cat -- "${tmp_dir}/${p}.out"
            done
        fi

        return "${retcode}"
    } always {
        silent trs-rm "${tmp_dir}" || true
        #: =silent= because [agfi:trs-rm] narrates its own =rm=, which would
        #: land in the middle of the report.
    }
}
aliasfn claude-code-status claude-code-usage-all
alias ccu='claude-code-usage-all'
alias ccs='claude-code-usage-all'
##
#: How often the armed job re-checks the wall clock.
typeset -g claude_code_usage_notif_poll_s="${claude_code_usage_notif_poll_s:-30}"
#: Fire this many seconds after the reset, so the endpoint has actually flipped
#: by the time we claim it has.
typeset -g claude_code_usage_notif_grace_s="${claude_code_usage_notif_grace_s:-30}"
#: Utilization at or above which a window counts as blocking us.
typeset -g claude_code_usage_notif_full_pct="${claude_code_usage_notif_full_pct:-100}"

function h-claude-code-usage-notif-window {
    #: Prints "<percent>\t<resets_at_epoch>\t<label>" for one window of a
    #: =claude-code-usage --json= payload, and fails when that window is
    #: absent -- a team seat, for one, has no weekly window at all.
    #:
    #: Roles: =session=, =weekly_all=, =weekly:<ModelDisplayName>=.
    ##
    local json="${1}" role="${2}"
    assert-args json role @RET

    ensure-cmd jq @RET

    #: Matching the normalized =.windows[]= on =key= covers both payload
    #: shapes: the authoritative =limits[]= array (session, weekly_all,
    #: weekly_scoped) and the legacy objects (five_hour, seven_day,
    #: seven_day_*).
    local filter='' model=''
    case "${role}" in
        session)
            filter='.key == "session" or .key == "five_hour"'
            ;;
        weekly_all)
            filter='.key == "weekly_all" or .key == "seven_day"'
            ;;
        weekly:*)
            #: Model-scoped weekly windows all share the key "weekly_scoped",
            #: so the model itself only survives in the label ("7d Fable").
            #: =contains=, not =test=, so a model name is never read as a
            #: regex; and passed via =--arg=, so it cannot break out of the
            #: jq program either.
            model="${${role#weekly:}:l}"
            assert-args model @RET

            filter='(.key == "weekly_scoped" or (.key | startswith("seven_day_"))) and (.label | ascii_downcase | contains($model))'
            ;;
        *)
            ectrace "$0: unknown role: ${role}"
            return 1
            ;;
    esac

    ec "${json}" |
        jq -er --arg model "${model}" "[.windows[] | select(${filter})] | first
            | select(. != null)
            | [(.utilization_percent // 0), (.resets_at // 0), .label]
            | @tsv"
}

function h-claude-code-usage-notif-session {
    #: The tmux session a profile's notifier lives in. The default profile
    #: keeps the unqualified name, being the one armed by hand most often.
    local profile="${1}"
    assert-args profile @RET

    if [[ "${profile}" == default ]] ; then
        ec 'claude-code-usage-notif'
    else
        ec "claude-code-usage-${profile}-notif"
    fi
}

function h-claude-code-usage-notif-wait {
    #: The armed one-shot body, running inside the tmux session that
    #: [agfi:h-claude-code-usage-notif] creates. This has to be a function: a
    #: bare =sleep= does not keep the marked subshell alive (see =PE/Zsh.org=).
    ##
    local poll_s="${claude_code_usage_notif_poll_s:-30}"

    local deadline="${1}" msg="${2}"
    assert-args deadline msg @RET

    zmodload zsh/datetime 2>/dev/null

    #: Poll the wall clock rather than issuing one long =sleep=: a suspend
    #: would skew a single five-hour sleep, and on wake we want to fire
    #: straight away instead of however long the machine slept later.
    while (( EPOCHSECONDS < deadline )) ; do
        sleep "${poll_s}"
    done

    #: A stable group, so a repeat replaces the previous notification instead
    #: of stacking up in Notification Center. See =docs/bell-auto.md=.
    notif_group='claude-code-usage' notif "${msg}"
}

function h-claude-code-usage-notif {
    #: Arms, or re-arms, a one-shot notification for when the limits that
    #: currently block us have reset.
    #:
    #: $1 is the tmux session to live in, $2 the profile to read, and the rest
    #: the roles this variant cares about (see
    #: [agfi:h-claude-code-usage-notif-window]).
    #:
    #: Re-arming cannot stack: [agfi:tmuxnew] kills the previous session's
    #: processes before creating the replacement, so the session name alone
    #: guarantees a single pending notifier -- no lock, marker or redis key.
    #: The tmux server is also independent of the brish garden, so
    #: =brishz-restart= does not silently disarm it. A reboot does.
    ##
    local poll_s="${claude_code_usage_notif_poll_s:-30}"
    local grace_s="${claude_code_usage_notif_grace_s:-30}"
    local full_pct="${claude_code_usage_notif_full_pct:-100}"

    local session="${1}" profile="${2}"
    assert-args session profile @RET
    local roles=("${@[3,-1]}")
    assert-args roles @RET

    ensure-cmd jq tmux @RET
    zmodload zsh/datetime 2>/dev/null

    #: =claude_code_usage_notif_p=n= is the recursion guard, and load-bearing:
    #: the report arms the notifier and the notifier reads the report.
    local json
    json="$(claude_code_usage_notif_p=n claude_code_usage_json_p=y claude_code_usage_profile="${profile}" claude-code-usage)" @TRET

    local blocked_labels=() role out pct resets label
    integer blocked_at=0
    for role in "${roles[@]}" ; do
        if ! out="$(h-claude-code-usage-notif-window "${json}" "${role}")" ; then
            ecgray "$0: ${profile}: no ${role} window, skipping"
            continue
        fi

        pct="${out%%$'\t'*}"
        resets="${${out#*$'\t'}%%$'\t'*}"
        label="${out##*$'\t'}"

        if (( pct >= full_pct )) && (( resets > 0 )) ; then
            blocked_labels+=("${label}")

            #: The LATEST reset among the blocked windows is when we are
            #: actually free again: a 5h rollover buys nothing while the
            #: weekly limit is still spent.
            if (( resets > blocked_at )) ; then
                blocked_at=${resets%.*}
            fi
        fi
    done

    integer deadline=0
    local msg=''
    if (( ${#blocked_labels} == 0 )) ; then
        if ! isDeus ; then
            ecgray "$0: ${profile}: usage already possible, not arming (use \`deus\` to arm anyway)"
            return 0
        fi

        #: deus: arm for the next 5h rollover anyway, so the mechanism can be
        #: exercised without having to be rate-limited first.
        out="$(h-claude-code-usage-notif-window "${json}" session)" @RET
        deadline=${${${out#*$'\t'}%%$'\t'*}%.*}
        msg="Claude Code (${profile}): ${out##*$'\t'} window rolled over"
    else
        deadline=${blocked_at}
        msg="Claude Code (${profile}): ${(j:, :)blocked_labels} reset, usage available again"
    fi

    deadline=$(( deadline + grace_s ))

    if (( deadline <= EPOCHSECONDS )) ; then
        ecgray "$0: ${profile}: reset time is already past (stale data?), not arming"
        return 0
    fi

    ecgray "$0: arming ${session} for $(date-unix-to-3339 "${deadline}") (in $(seconds-fmt-short $(( deadline - EPOCHSECONDS ))))"

    #: =silent= because [agfi:tmux-session-processes-kill] narrates every
    #: re-arm, which would otherwise land in the middle of a usage report.
    silent tmuxnewsh2 "${session}" \
        claude_code_usage_notif_poll_s="${poll_s}" \
        h-claude-code-usage-notif-wait "${deadline}" "${msg}" @RET

    #: Recorded on the tmux session itself rather than in redis, so the
    #: bookkeeping cannot drift from whether the job actually exists.
    #:
    #: No `=` exact-match prefix on the target here: unlike =has-session=,
    #: =set-option= does not accept one and fails with "no such session".
    silent tmux set-option -t "${session}" '@ccu_notif_deadline' "${deadline}" || true
}

function h-claude-code-usage-notif-for-profile {
    local profile="${1}"
    assert-args profile @RET

    local session
    session="$(h-claude-code-usage-notif-session "${profile}")" @RET

    h-claude-code-usage-notif "${session}" "${profile}" session weekly_all
}

function claude-code-usage-notif {
    #: Arms a notification for when the default profile's limits have reset.
    #: On by default after every [agfi:claude-code-usage]; see
    #: =docs/claude_code_usage.md=.
    ##
    h-claude-code-usage-notif-for-profile default
}
aliasfn ccun claude-code-usage-notif

function claude-code-usage-work-notif {
    h-claude-code-usage-notif-for-profile work
}

function claude-code-usage-fable-notif {
    #: The weekly Fable window as well as the windows that block everything.
    #: Its own tmux session, so it can be armed alongside
    #: [agfi:claude-code-usage-notif] rather than replacing it.
    ##
    h-claude-code-usage-notif 'claude-code-usage-fable-notif' default \
        session weekly_all 'weekly:Fable'
}

function claude-code-usage-notif-sessions {
    #: Every tmux session a notifier can live in, one per line.
    local profile out=()
    for profile in "${claude_code_profile_order[@]}" ; do
        out+=("$(h-claude-code-usage-notif-session "${profile}")")
    done
    out+=('claude-code-usage-fable-notif')

    ec "${(F)out}"
}

function claude-code-usage-notif-cancel {
    local sessions=("$@")
    if (( ${#sessions} == 0 )) ; then
        sessions=("${(@f)$(claude-code-usage-notif-sessions)}")
    fi

    local s alive_p
    for s in "${sessions[@]}" ; do
        if ! silent tmux has-session -t "=${s}" ; then
            continue
        fi

        alive_p=n
        if tmux-alive-p "${s}" ; then
            alive_p=y
        fi

        #: Dead sessions get reaped too. With =remain-on-exit= on, a notifier
        #: that has already fired leaves its session behind, and clearing those
        #: out is what someone running a cancel actually wants.
        silent tmux-session-processes-kill "${s}"
        if bool "${alive_p}" ; then
            ecgray "$0: cancelled ${s}"
        else
            ecgray "$0: reaped ${s}, which had already fired"
        fi
    done
}

function claude-code-usage-notif-status {
    zmodload zsh/datetime 2>/dev/null

    local s deadline
    integer remaining
    for s in "${(@f)$(claude-code-usage-notif-sessions)}" ; do
        if ! tmux-alive-p "${s}" ; then
            #: With =remain-on-exit= on a fired notifier leaves its session
            #: behind, which answers "did my notification actually go off?".
            if silent tmux has-session -t "=${s}" ; then
                ecgray "${s}: not armed; a previous notifier has already fired"
            else
                ecgray "${s}: not armed"
            fi

            continue
        fi

        deadline="$(tmux show-options -qv -t "${s}" '@ccu_notif_deadline' 2>/dev/null)" || deadline=''
        if test -z "${deadline}" ; then
            ec "${s}: armed (no deadline recorded)"
            continue
        fi

        remaining=$(( deadline - EPOCHSECONDS ))
        if (( remaining > 0 )) ; then
            ec "${s}: armed for $(date-unix-to-3339 "${deadline}") (in $(seconds-fmt-short ${remaining}))"
        else
            ec "${s}: armed, but its deadline passed $(seconds-fmt-short $(( -remaining ))) ago"
        fi
    done
}
##
function claude-work {
    #: Claude Code on the CIS-OE67 LMU team seat: a second config home, so a
    #: separate account, history, projects and plugins. `settings.json` there
    #: symlinks to the same tracked file as the personal profile.
    #:
    #: Goes through [agfi:claude] rather than `command claude`, so it gets
    #: [agfi:h-agents-md-sync-ask] -- which is what keeps
    #: ~/.claude-work/CLAUDE.md current -- along with the watchdogs, the retry
    #: cap and `$proxyenv`. Two consequences worth knowing: `$proxyenv` now
    #: applies to work sessions too (a no-op unless proxy mode is on), and the
    #: exported CLAUDE_CONFIG_DIR is inherited, so anything the session
    #: launches -- [agfi:claude-autocommit], [agfi:claude-vcsh-commit] -- stays
    #: on this seat.
    #:
    #: The personal profile is deliberately *not* pinned the same way. Claude
    #: Code hashes the config dir into the keychain service name, using a bare
    #: `Claude Code-credentials` only while CLAUDE_CONFIG_DIR is unset, so
    #: setting it there would cost a re-login for nothing.
    ##
    local -x CLAUDE_CONFIG_DIR="${HOME}/.claude-work"
    local claude_tty_title_marker='🏛'

    claude "$@"
}
##
