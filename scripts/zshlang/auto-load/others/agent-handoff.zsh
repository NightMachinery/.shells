##
#: Cross-agent conversation handoffs. Native import and native compaction are
#: deliberately separate entry points; neither silently substitutes the other.
#: See =docs/agent-handoff.md= and [agfi:claude-resume-compact].
##
function h-agent-handoff-source {
    #: Usage: <source-agent> <transcript|id>; exact stopped source in REPLY.
    local agent="${1}" session="${2}"
    assert-args agent session @RET
    local agent_session_agents="${agent}"
    local agent_session_live_list_cache='' claude_code_session_live_list_cache=''
    local transcript owner row
    transcript="$(h-agent-session-resolve "${session}")" @RET
    owner="$(h-agent-session-agent-of "${transcript}")" @RET
    if [[ "${owner}" != "${agent}" ]] ; then
        ecerr "$0: expected a ${agent} transcript, got ${owner}"
        return 1
    fi
    local live
    live="$(h-agent-session-live-list)" @RET
    local -a fields
    for row in "${(@f)live}" ; do
        fields=( "${(@ps:\t:)row}" )
        (( ${#fields} >= 5 )) || continue
        if [[ "${fields[5]}" == "${transcript}" ]] ; then
            ecerr "$0: source is live (pid ${fields[1]}); quit it before transferring"
            return 1
        fi
    done
    REPLY="${transcript}"
}

function h-agent-handoff-env-clear {
    #: Called only inside a subshell. The destination must register its own
    #: identity and must not inherit the source's managed-pane resume state.
    unset CLAUDECODE CLAUDE_CODE_SESSION_ID CLAUDE_PID CODEX_THREAD_ID \
        CODEX_SESSION_ID CODEX_SANDBOX ANTIGRAVITY_CONVERSATION_ID AI_AGENT \
        AGENT_SESSION_STATE AGENT_SESSION_ID AGENT_SESSION_REUSE_PANE \
        AGENT_SESSION_CWD AGENT_SESSION_INITIAL_COMMAND \
        AGENT_SESSION_RESUME_COMMAND AGENT_SESSION_HOOK_ARGS_FILE
}

function h-agent-handoff-codex-args {
    #: Validate launcher options before any import; return preparation options
    #: in reply. Only model and config also apply to app-server preparation.
    local arg
    local -a prepared=()
    while (( $# )) ; do
        arg="${1}"
        shift
        case "${arg}" in
            --model|-m|--config|-c)
                (( $# )) || { ecerr "$0: missing value for ${arg}"; return 1; }
                [[ "${arg}" == (-m|--model) ]] && prepared+=(--model "${1}") || prepared+=(--config "${1}")
                shift ;;
            --model=*|--config=*) prepared+=("${arg}") ;;
            --profile|-p|--profile=*)
                ecerr "$0: --profile is not supported by handoff preparation; use --config overrides"
                return 1 ;;
            --sandbox|-s|--ask-for-approval|-a|--add-dir|--image|-i|--local-provider|--enable|--disable)
                (( $# )) || { ecerr "$0: missing value for ${arg}"; return 1; }
                shift ;;
            --sandbox=*|--ask-for-approval=*|--add-dir=*|--image=*|--local-provider=*|--enable=*|--disable=*) ;;
            --search|--approve-for-me|--no-alt-screen|--no-daemon|--strict-config|--oss|--dangerously-bypass-approvals-and-sandbox|--dangerously-bypass-hook-trust) ;;
            *)
                ecerr "$0: unsupported handoff launcher option: ${arg}"
                return 1 ;;
        esac
    done
    reply=( "${prepared[@]}" )
}

function h-claude-to-codex {
    #: Usage: <native|compact> <transcript|id> [codex options...]
    local mode="${1}" session="${2}"
    shift 2 @RET
    local timeout="${agent_handoff_timeout:-10m}"
    local REPLY transcript dir id
    local -a reply prepared
    h-agent-handoff-codex-args "$@" @RET
    prepared=( "${reply[@]}" )
    h-agent-session-dep @RET
    ensure-cmd codex @RET
    h-agent-handoff-source claude "${session}" @RET
    transcript="${REPLY}"
    dir="$(h-agent-session-dir "${transcript}" claude)" @RET
    if ! test -d "${dir}" ; then
        ecerr "$0: source working directory is unavailable: ${dir}"
        return 1
    fi
    id="$(
        h-agent-handoff-env-clear
        agent_session claude handoff -mode "${mode}" -cwd "${dir}" \
            -timeout "${timeout}" "${transcript}" -- "${prepared[@]}"
    )" @RET
    local uuid_re='^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$'
    if [[ ! "${id}" =~ ${uuid_re} ]] ; then
        ecerr "$0: backend did not return an exact Codex thread ID"
        return 1
    fi
    local -a context_opts=()
    [[ "${mode}" == compact ]] && context_opts+=(--config model_context_window=1050000)
    (
        h-tmux-env-repair || true
        h-agent-handoff-env-clear
        builtin cd -q -- "${dir}" @RET
        codex-m resume "${id}" "$@" "${context_opts[@]}"
    )
}

function claude-to-codex-native {
    : "usage: claude-to-codex-native <transcript|id> [codex options...]"
    h-claude-to-codex native "$@"
}

function claude-to-codex-compact {
    : "usage: claude-to-codex-compact <transcript|id> [codex options...]"
    h-claude-to-codex compact "$@"
}

function h-claude-to-codex-fz {
    local mode="${1}"
    shift @RET
    local agent_session_agents=claude
    local agent_session_fz_scope="${agent_handoff_scope:-project}"
    local transcript
    transcript="$(h-agent-session-select-fz)" @RET
    h-claude-to-codex "${mode}" "${transcript}" "$@"
}
aliasfn claude-to-codex-native-fz h-claude-to-codex-fz native
aliasfn claude-to-codex-native-all-fz agent_handoff_scope=all h-claude-to-codex-fz native
aliasfn claude-to-codex-compact-fz h-claude-to-codex-fz compact
aliasfn claude-to-codex-compact-all-fz agent_handoff_scope=all h-claude-to-codex-fz compact
