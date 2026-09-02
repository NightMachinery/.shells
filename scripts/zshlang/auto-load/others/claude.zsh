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
    $proxyenv revaldbg command claude_code_usage.py "${script_args[@]}" "$@"
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
