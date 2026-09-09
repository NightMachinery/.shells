##
#: The coding agents this repository knows how to drive -- Claude Code, Codex,
#: Antigravity (`agy') -- and the dispatch that lets agent-neutral code
#: (=agent-session.zsh=) call into each one's adapter without naming it.
#:
#: An adapter is a set of `h-<agent>-session-<verb>' functions; see
#: =docs/agent-sessions.md= for the verbs. The agent tokens are the ones
#: [agfi:ai-agent-name] and =agent-tmux.zsh= already use.
##
function h-agents-table {
    #: One row per agent, tab separated: agent, label, glyph, the binary
    #: names it runs under (space separated, for matching a foreground
    #: process), and its launcher function. Redefine in `personal/' to add or
    #: hide one.
    ##
    print -r -- $'claude\tClaude Code\t🍼\tclaude claude.exe\tclaude'
}

function h-agents {
    #: The agents the session helpers consult, one per line. All of them by
    #: default; `agent_session_agents' (whitespace separated) narrows it, which
    #: is how the Claude-only compat names in =claude-session.zsh= work.
    ##
    if test -n "${agent_session_agents}" ; then
        print -rl -- ${=agent_session_agents}
        return 0
    fi

    h-agents-table | command cut -f1
}

function h-agent-field {
    #: One column of [agfi:h-agents-table] for an agent.
    #: Usage: h-agent-field <agent> <label|glyph|binaries|launcher>
    ##
    local agent="${1}" col="${2}"
    assert-args agent col @RET

    local n
    case "${col}" in
        agent) n=1 ;;
        label) n=2 ;;
        glyph) n=3 ;;
        binaries) n=4 ;;
        launcher) n=5 ;;
        *)
            ecerr "$0: unknown column: ${col}"
            return 1
            ;;
    esac

    local row
    row="$(h-agents-table | gawk -F'\t' -v a="${agent}" -v n="${n}" '$1 == a { print $n ; exit }')" @RET
    if test -z "${row}" ; then
        ecerr "$0: unknown agent: ${agent}"
        return 1
    fi
    ec "${row}"
}

function h-agent-session-call {
    #: Calls an adapter verb: `h-<agent>-session-<verb> args...'. Returns 2 when
    #: the agent has no such verb, so "unsupported" can be told from "failed".
    #: Usage: h-agent-session-call <agent> <verb> [args...]
    ##
    local agent="${1}" verb="${2}"
    shift 2
    assert-args agent verb @RET

    local fn="h-${agent}-session-${verb}"
    if (( ! ${+functions[${fn}]} )) ; then
        return 2
    fi
    "${fn}" "$@"
}

function h-agent-session-agent-of {
    #: Which agent a transcript belongs to, from its path alone, so the agent
    #: never has to travel separately: not across the tmux job boundary, not
    #: in fzf rows, not in the registry. Each adapter's `owns-p' is a glob test.
    ##
    local transcript="${1}"
    assert-args transcript @RET

    local agent
    for agent in ${(f)"$(h-agents)"} ; do
        if h-agent-session-call "${agent}" owns-p "${transcript}" ; then
            ec "${agent}"
            return 0
        fi
    done

    ecerr "$0: no agent owns: ${transcript}"
    return 1
}
