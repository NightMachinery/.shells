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
    print -r -- $'codex\tCodex\t⚡\tcodex codex.js\tcodex'
    print -r -- $'agy\tAntigravity\t🪐\tagy\tantigravity'
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

    #: Split in the shell rather than through `cut': this is called on every
    #: session lookup, and a subprocess for three lines is not worth 3ms.
    local row
    for row in ${(f)"$(h-agents-table)"} ; do
        print -r -- "${row%%$'\t'*}"
    done
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

    #: Shell field splitting rather than `gawk': a lookup per agent per picker
    #: adds up, and the table is three lines.
    local row
    local -a f
    for row in ${(f)"$(h-agents-table)"} ; do
        f=( "${(@ps:\t:)row}" )
        if [[ "${f[1]}" == "${agent}" ]] ; then
            ec "${f[$n]}"
            return 0
        fi
    done

    ecerr "$0: unknown agent: ${agent}"
    return 1
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

    #: A transcript outside every store -- one copied to ~/tmp, or a file a
    #: colleague sent -- is still worth reading, so the shape of its first
    #: record decides. Last, and only then: a path under a store is the
    #: authority, and this cannot tell a fork from its original.
    if agent="$(h-agent-session-sniff "${transcript}")" ; then
        ec "${agent}"
        return 0
    fi

    ecerr "$0: no agent owns: ${transcript}"
    return 1
}

function h-agent-session-sniff {
    #: Which agent wrote a transcript, from the first record's own keys. Each
    #: format names itself: a Codex rollout opens with a `session_meta' record
    #: carrying a payload, an Antigravity step has a `step_index', and a Claude
    #: Code record has a `sessionId' or one of its record types.
    ##
    local transcript="${1}"
    assert-args transcript @RET
    test -r "${transcript}" || return 1
    isdefined-cmd jq || return 1

    local agent
    agent="$(command head -n 1 -- "${transcript}" 2>/dev/null | jq -r '
        if type != "object" then empty
        elif has("payload") and (.type // "" | test("^session_meta$|^response_item$|^turn_context$")) then "codex"
        elif has("step_index") then "agy"
        elif has("sessionId") or has("isMeta") or ((.type // "") | test("^(user|assistant|summary)$")) then "claude"
        else empty end' 2>/dev/null)" || return 1

    test -n "${agent}" || return 1
    ec "${agent}"
}
##
function h-jwt-payload {
    #: The payload of a JWT on stdin, as JSON on stdout. The signature is not
    #: checked and cannot be: this is for reading the claims of a token we
    #: already stored ourselves -- which account is signed in -- not for
    #: trusting one that arrived from somewhere.
    ##
    local tok body
    tok="$(command cat)"
    tok="${tok//[[:space:]]/}"
    test -n "${tok}" || return 1

    body="${${tok#*.}%%.*}"
    test -n "${body}" && [[ "${body}" != "${tok}" ]] || return 1

    #: base64url to base64, then pad to a multiple of four.
    body="${body//-/+}"
    body="${body//_//}"
    while (( ${#body} % 4 )) ; do
        body+='='
    done

    print -r -- "${body}" | command base64 -d 2>/dev/null
}

function h-agent-session-account {
    #: Which account and profile a transcript belongs to, as one short line, or
    #: failure when the agent cannot say. Each adapter's `account' verb reads
    #: the agent's own config; none of them prints a token.
    #: Usage: h-agent-session-account <transcript>
    ##
    local transcript="${1}"
    assert-args transcript @RET

    local agent
    agent="$(h-agent-session-agent-of "${transcript}")" @RET

    h-agent-session-call "${agent}" account "${transcript}" 2>/dev/null
}

#: Whether the launcher runs agents behind [agfi:h-decset-rewrite-dep]'s pty
#: proxy, which downgrades the DECSET mouse modes below. On for now; set it to
#: `n' once Termux ships termux-app PR 5281, which teaches the emulator 1003
#: itself and makes the whole detour unnecessary. A per-agent
#: `<agent>_decset_rewrite_p' overrides it; see [agfi:h-agent-launch] and
#: =docs/termux-mouse-decset-1003.md=.
typeset -g agent_launch_decset_rewrite_p="${agent_launch_decset_rewrite_p:-y}"

#: The rewrites themselves, `FROM=TO' each, passed on as `-map' arguments. Only
#: private modes are touched: `ESC [ ? <from> h' and its `l'. The default
#: downgrades any-event mouse tracking (1003) to button-event tracking (1002),
#: which is what Termux understands and what the PR above aliases it to anyway.
typeset -ga agent_launch_decset_map
(( ${#agent_launch_decset_map} )) || agent_launch_decset_map=(1003=1002)

#: A path here turns tracing on: the proxy appends one line per private-mode
#: sequence the agent emits -- mode numbers only, never content -- which is how
#: to find out what an agent actually asks for. Empty, the default, is off.
typeset -g agent_launch_decset_trace="${agent_launch_decset_trace:-}"

typeset -g agent_launch_decset_module="${agent_launch_decset_module:-github.com/NightMachinery/decset-rewrite}"

function h-decset-rewrite-dep {
    #: Ensures the DECSET proxy is on PATH, installing it on first use.
    #: Same shape, and the same `whence -p' reasoning, as
    #: [agfi:h-agent-session-dep]: this sits on every agent launch, and reading
    #: `$commands' would hash all of PATH for it.
    #:
    #: The tool is *not* in this repository. It is useful to anyone whose
    #: terminal ignores DECSET 1003, so it has its own public repository and
    #: that is the only copy; keeping a second one here would be a fork waiting
    #: to happen. [agfi:go-install] takes the latest tag, so the first install
    #: needs the network -- which an agent launch does anyway -- and every
    #: later launch is one `whence -p' and no more.
    ##
    if whence -p decset-rewrite > /dev/null 2>&1 ; then
        return 0
    fi

    ensure-cmd go @RET
    ensure-dep1 decset-rewrite go-install "${agent_launch_decset_module}" @RET
}

function h-agent-launch {
    #: The preamble every agent launcher shares, then the agent: `nvim' as the
    #: editor, the instruction files synced ([agfi:h-agents-md-sync-ask];
    #: `agent_launch_sync_p=n' skips it), the terminal titled `<glyph><cwd>' so
    #: a tab can be told apart at a glance (`agent_launch_glyph' overrides the
    #: table's glyph; [agfi:claude-work] uses that), and the proxy environment.
    #: Anything agent-specific -- Claude's watchdog variables, say -- is set by
    #: the caller before this runs; `local -x' reaches the child from there.
    #:
    #: It also runs the agent on a pty behind [agfi:h-decset-rewrite-dep]'s
    #: proxy, which rewrites `ESC [ ? 1003 h/l' to 1002 on the way out. Termux
    #: drops 1003 and gates touch forwarding on some mouse mode being active,
    #: and mosh keeps only the last mode an app asked for, so an agent whose
    #: last request is 1003 has no mouse at all on the phone;
    #: =docs/termux-mouse-decset-1003.md= has the whole chain. Gated on
    #: `agent_launch_decset_rewrite_p', which `<agent>_decset_rewrite_p'
    #: overrides per agent (`claude_decset_rewrite_p', `codex_decset_rewrite_p',
    #: `agy_decset_rewrite_p'), on `agent_launch_decset_map' for the rewrites
    #: and `agent_launch_decset_trace' for the log; a missing proxy is reported
    #: and the agent launched unwrapped, never blocked. `agent_launch_echo_p=y'
    #: prints the final command line before running it.
    #: Usage: h-agent-launch <agent> <binary> [args...]
    ##
    local agent="${1}"
    shift
    assert-args agent @RET
    (( $# )) || return 1

    local -x EDITOR=nvim
    local -x VISUAL="${EDITOR}"

    if bool "${agent_launch_sync_p:-y}" ; then
        h-agents-md-sync-ask @RET
        #: The shared skills (=configFiles/agent-skills/=) are installed on the
        #: same schedule and for the same reason as the instruction files: an
        #: agent reads them at startup, so the moment to make sure they are
        #: there is just before one starts. A stat per skill per agent.
        agent-skills-link || true
    fi

    local glyph="${agent_launch_glyph}"
    if test -z "${glyph}" ; then
        glyph="$(h-agent-field "${agent}" glyph)" @RET
    fi
    tty-title "${glyph}${PWD:t}"

    #: The per-agent flag falls back to the global one, so
    #: `claude_decset_rewrite_p=n claude' works without touching the launchers.
    local rewrite_p="${(P)${:-${agent}_decset_rewrite_p}:-${agent_launch_decset_rewrite_p}}"

    local -a cmd
    cmd=( "$@" )
    if bool "${rewrite_p}" && isTty ; then
        if h-decset-rewrite-dep ; then
            cmd=( decset-rewrite )

            local m
            for m in "${agent_launch_decset_map[@]}" ; do
                cmd+=( -map "${m}" )
            done
            if test -n "${agent_launch_decset_trace}" ; then
                cmd+=( -trace "${agent_launch_decset_trace}" )
            fi

            cmd+=( -- "$@" )
        else
            #: A launch is never worth blocking on a mouse-mode workaround.
            ecerr "$0: decset-rewrite unavailable, launching ${agent} unwrapped"
        fi
    fi

    #: `command' here rather than in every caller: it has to sit inside the
    #: wrapper's argv, not in front of it.
    if bool "${agent_launch_echo_p}" ; then
        $proxyenv reval-ec command "${cmd[@]}"
    else
        $proxyenv command "${cmd[@]}"
    fi
}
