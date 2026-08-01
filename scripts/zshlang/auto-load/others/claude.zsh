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

    # Debug logging. Use a stable per-run file so you can tail it.
    local debug_file="${HOME}/tmp/claude-code/${EPOCHSECONDS}.debug.log"
    local -x CLAUDE_CODE_DEBUG_LOG_LEVEL=verbose


    tty-title "🍼${PWD:t}"

    $proxyenv command claude "$@"
}
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

aliasfn claude-m claude-pioneer
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
function claude-code-usage {
    #: Shows the usage stats of the current Claude Code plan (like the in-app =/usage=).
    #: See =docs/claude_code_usage.md=.
    ##
    local claude_code_usage_timeout_s="${claude_code_usage_timeout_s:-10}"
    local claude_code_usage_cache_ttl_s="${claude_code_usage_cache_ttl_s:-300}"
    local claude_code_usage_refresh_p="${claude_code_usage_refresh_p:-n}"
    local claude_code_usage_json_p="${claude_code_usage_json_p:-n}"
    local claude_code_usage_strip_ansi_p="${claude_code_usage_strip_ansi_p:-n}"

    if ! command -v -- claude_code_usage.py >/dev/null 2>&1 ; then
        ecerr "claude-code-usage: claude_code_usage.py not found in PATH"
        return 127
    fi

    local script_args=(--timeout "${claude_code_usage_timeout_s}" --cache-ttl "${claude_code_usage_cache_ttl_s}")
    if bool "${claude_code_usage_refresh_p}" ; then
        script_args+=(--refresh)
    fi
    if bool "${claude_code_usage_json_p}" ; then
        script_args+=(--json)
    fi
    if bool "${claude_code_usage_strip_ansi_p}" ; then
        script_args+=(--color never)
    fi

    #: =script_args= before user args so explicit CLI flags win (argparse last-wins).
    $proxyenv revaldbg command claude_code_usage.py "${script_args[@]}" "$@"
}
alias ccu='claude-code-usage'
##
