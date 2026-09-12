##
#: Codex CLI's side of the agent session helpers: the adapter verbs
#: =agent-session.zsh= dispatches to ([agfi:h-agent-session-call]), and the
#: `codex-*' names for the common cases. Rollouts live under
#: `$CODEX_HOME/sessions/YYYY/MM/DD/rollout-<local time>-<uuid>.jsonl'; the Go
#: side, =golang/agent_session/internal/codex=, reads them. The tmux naming
#: hook is in =agent-tmux.zsh=. See =docs/agent-sessions.md=.
##
function h-codex-session-home {
    #: Codex's state directory.
    ##
    ec "${CODEX_HOME:-${HOME}/.codex}"
}

function h-codex-session-roots {
    #: The sessions directory, when it exists; nothing otherwise, which is how
    #: a host without Codex contributes no rows.
    ##
    local d
    d="$(h-codex-session-home)/sessions"
    test -d "${d}" || return 1
    ec "${d}"
}

function h-codex-session-owns-p {
    #: Whether a transcript is a Codex rollout: it sits under the sessions
    #: directory.
    ##
    local transcript="${1:a}"

    local d
    for d in ${(f)"$(h-codex-session-roots 2>/dev/null)"} ; do
        [[ "${transcript}" == "${d:a}"/* ]] && return 0
    done
    return 1
}

function h-codex-session-id-of {
    #: `rollout-2026-09-08T12-34-56-<uuid>.jsonl' -> the uuid: the last 36
    #: characters of the stem.
    ##
    local stem="${1:t:r}"
    ec "${stem[-36,-1]}"
}

function h-codex-session-resolve {
    #: The rollouts whose uuid starts with the given prefix, one per line.
    #: [agfi:h-agent-session-resolve] decides what several hits mean.
    ##
    setopt localoptions bareglobqual

    local input="${1}"
    assert-args input @RET

    local -a hits
    local d
    for d in ${(f)"$(h-codex-session-roots)"} ; do
        hits+=( "${d}"/*/*/*/rollout-*-"${input}"*.jsonl(N) )
    done
    (( ${#hits} )) || return 1

    print -rl -- "${hits[@]}"
}

function h-codex-session-current-id {
    #: The thread id Codex exports into the shells it runs: CODEX_THREAD_ID,
    #: with CODEX_SESSION_ID as the older spelling. Only the shell tool's
    #: children get them, not hooks; a plain shell has neither.
    ##
    local id="${CODEX_THREAD_ID:-${CODEX_SESSION_ID}}"
    if test -z "${id}" ; then
        ecerr "$0: not inside a Codex shell (CODEX_THREAD_ID is unset)"
        return 1
    fi
    ec "${id}"
}

function h-codex-session-live-list {
    #: Every running Codex thread, one per line, tab separated: pid, thread id,
    #: name, cwd, transcript, tmux session (or `-'), status. The Go side pairs
    #: threads to processes through the lock files they hold; see
    #: =golang/agent_session/internal/codex/live.go=.
    ##
    local -a roots
    roots=( ${(f)"$(h-codex-session-roots)"} ) || return 1

    h-agent-session-dep @RET
    agent_session codex live "${roots[@]}"
}

function h-codex-session-resume {
    #: `codex resume <uuid>', through the [agfi:codex] launcher.
    #: Usage: h-codex-session-resume <transcript> [codex args...]
    ##
    local transcript="${1}"
    shift
    assert-args transcript @RET

    local id
    id="$(h-codex-session-id-of "${transcript}")" @RET

    #: In the session's own directory ([agfi:h-agent-session-resume-run]).
    local -a reply
    h-agent-session-resume-argv codex "${id}" codex "$@" @RET
    h-agent-session-resume-run "${transcript}" "${reply[@]}"
}

function h-codex-session-account {
    #: The signed-in ChatGPT account, as `someone@example.com · pro'. Codex
    #: stores an id token in `auth.json'; two of its claims say who is signed
    #: in and on what plan. The token itself is never printed, and nothing here
    #: goes to the network -- a document header must not wait on an API.
    ##
    local f
    f="$(h-codex-session-home)/auth.json"
    test -e "${f}" || return 1
    isdefined-cmd jq || return 1

    local claims
    claims="$(jq -r '.tokens.id_token // empty' "${f}" 2>/dev/null | h-jwt-payload)" || return 1
    test -n "${claims}" || return 1

    local email plan
    email="$(ec "${claims}" | jq -r '.email // empty' 2>/dev/null)" || email=''
    plan="$(ec "${claims}" | jq -r '.["https://api.openai.com/auth"].chatgpt_plan_type // empty' 2>/dev/null)" || plan=''

    local -a parts
    test -n "${email}" && parts+=( "${email}" )
    test -n "${plan}" && parts+=( "${plan:l}" )
    (( ${#parts} )) || return 1

    print -r -- "${(j: · :)parts}"
}

function h-codex-session-hook-transcript {
    #: The transcript a Codex hook payload is about, or nothing. Codex gives
    #: `transcript_path', but may leave it null; then the thread is found from
    #: `session_id'. A payload carrying `agent_id' is a subagent's and is
    #: ignored: its `session_id' is the root thread, which registered itself.
    ##
    local input
    input="$(h-agent-hook-payload "${1}")"
    test -n "$input" || return 1

    #: `(ps:\t:)' rather than `read': tab is IFS whitespace, so `read' would
    #: collapse the empty first field of a payload whose `transcript_path' is
    #: null and slide the session id into it.
    local -a f
    f=( "${(@ps:\t:)$(ec "$input" | jq -r '[(.transcript_path // ""), (.session_id // ""), (.agent_id // "")] | @tsv' 2>/dev/null)}" )
    local transcript="${f[1]}" sid="${f[2]}" agent="${f[3]}"
    test -z "${agent}" || return 0

    if test -n "${transcript}" && test -e "${transcript}" ; then
        ec "${transcript}"
        return 0
    fi
    test -n "${sid}" || return 0

    h-agent-session-resolve codex "${sid}" 2>/dev/null || true
}
##
#: The everyday names. `agent_session_agents=codex' narrows the shared
#: pickers and resolvers to Codex.
##
aliasfn codex-resume agent_session_agents=codex agent-session-resume
aliasfn codex-resume-fz agent_session_agents=codex agent-session-resume-fz
aliasfn codex-resume-all-fz agent_session_agents=codex agent_session_resume_scope=all agent-session-resume-fz
aliasfn codex-view-session-fz agent_session_agents=codex agent-view-session-fz
aliasfn codex-view-session-all-fz agent_session_agents=codex agent_session_fz_scope=all agent-view-session-fz
aliasfn codex-session-live-fz agent_session_agents=codex agent-session-live-fz
##
