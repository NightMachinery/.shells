# No personal shell dependencies. Provider discovery and profile selection
# belong to callers; this layer receives the exact identity and launcher.
typeset -g agent_session_runtime="${${(%):-%x}:A:h}/pane.py"
typeset -g agent_tmux_identity_option="${agent_tmux_identity_option:-@agent_session}"

function h-agent-session-resume-argv {
    # Usage: <provider> <id> <launcher> [args...]; returns argv in reply.
    local provider="${1}" id="${2}" launcher="${3}"
    shift 3 || return 64
    local uuid_re='^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$'
    if [[ ! ${id} =~ ${uuid_re} || -z ${launcher} ]] ; then
        print -ru2 -- 'resume requires an exact conversation ID and launcher'
        return 64
    fi
    case ${provider} in
        claude) reply=("${launcher}" --resume "${id}" "$@") ;;
        codex) reply=("${launcher}" resume "${id}" "$@") ;;
        agy) reply=("${launcher}" --conversation "${id}" "$@") ;;
        *) print -ru2 -- "unsupported resume provider: ${provider}"; return 64 ;;
    esac
}

function agent-session-resume-exact {
    # Usage: <provider> <id> <cwd> <launcher> [args...]
    local provider="${1}" id="${2}" dir="${3}" launcher="${4}"
    shift 4 || return 64
    local -a reply hooks
    h-agent-session-resume-argv "${provider}" "${id}" "${launcher}" "$@" || return $?
    if [[ -n ${AGENT_SESSION_HOOK_ARGS_FILE:-} ]] ; then
        local encoded
        encoded="$(command python3 "${agent_session_runtime}" hooks "${AGENT_SESSION_HOOK_ARGS_FILE}")" || return $?
        hooks=("${(@0)encoded}")
        [[ -n ${encoded} ]] && reply+=("${hooks[@]}")
    fi
    # A subshell preserves the caller's cwd and any dynamically scoped profile.
    ( builtin cd -q -- "${dir}" && "${reply[@]}" )
}

function h-agent-tmux-identity-set {
    # Usage: <pane> <provider> <id> [transcript]
    local pane="${1}" agent="${2}" id="${3}" transcript="${4}"
    local state
    state="$(command tmux show-option -pqv -t "${pane}" @agent_session_state 2>/dev/null)"
    if [[ -n ${state} ]] ; then
        command python3 "${agent_session_runtime}" identity "${state}" "${agent}" "${id}" "${transcript}" || return $?
    fi
    command tmux set-option -t "${pane}" "${agent_tmux_identity_option}" \
        "${agent}"$'\t'"${id}"$'\t'"${transcript}" 2>/dev/null
}

function agent-tmux-identity-get {
    local pane="${1:-${TMUX_PANE}}" value
    [[ -n ${pane} ]] || { print -ru2 -- 'not inside tmux'; return 1; }
    value="$(command tmux show-option -qv -t "${pane}" "${agent_tmux_identity_option}" 2>/dev/null)" || return $?
    [[ -n ${value} ]] || { print -ru2 -- 'no conversation recorded yet'; return 1; }
    print -r -- "${value}"
}

function agent-session-register-current {
    # Call inside the child before its first task action, including on resume.
    local provider="${1}" id
    case ${provider} in
        claude) id="${CLAUDE_CODE_SESSION_ID:-}" ;;
        codex) id="${CODEX_THREAD_ID:-${CODEX_SESSION_ID:-}}" ;;
        agy) id="${ANTIGRAVITY_CONVERSATION_ID:-}" ;;
        *) return 64 ;;
    esac
    [[ -n ${id} && -n ${TMUX_PANE:-} ]] || {
        print -ru2 -- 'conversation ID or TMUX_PANE is unavailable'; return 1
    }
    h-agent-tmux-identity-set "${TMUX_PANE}" "${provider}" "${id}" ""
}

function agent-session-pane-run {
    # Executed only by pane.py after taking the per-pane process lock.
    local state="${1}" mode="${2}" setup
    setup="$(command python3 "${agent_session_runtime}" shell "${state}" "${mode}")" || return $?
    eval "${setup}" || return $?
    builtin cd -q -- "${AGENT_SESSION_CWD}" || return $?
    if [[ ${mode} == initial ]] ; then
        eval "${AGENT_SESSION_INITIAL_COMMAND}"
    else
        eval "${AGENT_SESSION_RESUME_COMMAND}"
    fi
}
