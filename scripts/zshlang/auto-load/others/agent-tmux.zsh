##
#: Naming the tmux session after the agent session inside it, for Claude
#: Code, Codex and Antigravity (`agy') alike. Each agent fires a hook with
#: its session id and transcript; the hook bodies here hand those to one
#: core that records the identity on the tmux session and renames it to
#: `+Claude/work <name>', `+Codex <name>' or `+Agy <name>'. The by-hand
#: commands live in =tmux.zsh= ([agfi:tmux-session-rename-current-auto]).
#: See =docs/tmux-session-rename.md=.
##
#: The tmux user option that lets the hooks rename a session. Read with
#: `show-option -A', so a session-level value overrides the global default
#: set in =~/.tmux.conf=.
typeset -g agent_tmux_autoname_option='@agent_autoname'
#: Where the hook leaves who lives in a tmux session: agent, id, transcript,
#: tab-separated. It is what lets [agfi:tmux-session-rename-current-auto]
#: work in a Codex or agy shell, which export no id of their own.
typeset -g agent_tmux_identity_option='@agent_session'
#: The marker on a session name the hooks own, as opposed to one a person
#: chose with [agfi:tmux-session-rename-current]. Not `@': a leading `@' is
#: tmux window-id syntax, so `@Claude/work x' was unusable as a `-t' target
#: and `fft' on such a session failed with `can't find window'. `+' is an
#: ordinary character to tmux, and still sorts agent sessions to the top of
#: `tmux ls'. See =docs/tmux-session-rename.md=.
typeset -g agent_tmux_name_marker='+'

function h-agent-hook-payload {
    #: The JSON an agent's hook was handed: `$1' when non-empty, else stdin.
    #: Bounded: an inherited pipe that never closes must not wedge the hook.
    ##
    local input="${1}"

    if test -z "$input" && ! test -t 0 ; then
        input="$(gtimeout 2 cat)" || input=''
    fi

    ec "${input}"
}
aliasfn h-claude-code-hook-payload h-agent-hook-payload

function h-tmux-session-name-sanitize {
    : "makes a string acceptable and tidy as a tmux session name"
    #: tmux refuses '.' and ':' (they are target syntax). Whitespace is
    #: allowed but runs of it and stray edges read badly; long titles are cut.
    ##
    local name="${1}"
    local max="${tmux_session_name_max:-60}"

    name="${name//[.:]/-}"
    name="${(j: :)${=name}}"
    ec "${name[1,${max}]}"
}

#: Identity recording is shared with the portable agent-session plugin.

function h-agent-session-tmux-name {
    : "<agent> <id> <transcript>: the tmux session name for that agent session"
    local agent="${1}" id="${2}" transcript="${3}"

    case "${agent}" in
        claude) h-claude-code-session-tmux-name "${transcript}" ;;
        codex) h-codex-session-tmux-name "${id}" ;;
        agy) h-agy-session-tmux-name "${id}" ;;
        *)
            ecerr "$0: unknown agent: ${agent}"
            return 1
            ;;
    esac
}

function h-agent-tmux-autoname {
    #: The hook core: <agent> <pane> <id> <transcript>. Records the identity,
    #: then renames the tmux session when allowed. Every early return is an
    #: ordinary outcome, not an error: no tmux, the option off, a name that is
    #: already right. Silent throughout; the hook lines discard output anyway.
    #:
    #: Sessions named `ag--*' are never touched, whatever the option says.
    #: They belong to the tmux-subagents skill, which keeps readable task and
    #: model labels in the name and would lose that identity to a rename.
    ##
    local agent="${1}" pane="${2}" id="${3}" transcript="${4}"
    test -n "${pane}" || return 0
    test -n "${id}${transcript}" || return 0

    h-agent-tmux-identity-set "${pane}" "${agent}" "${id}" "${transcript}" || return 0

    local current
    current="$(command tmux display-message -p -t "${pane}" '#S' 2>/dev/null)" || return 0
    [[ "${current}" == ag--* ]] && return 0

    local opt
    opt="$(command tmux show-option -qvA -t "${pane}" "${agent_tmux_autoname_option}" 2>/dev/null)"
    [[ "${opt}" == on ]] || return 0

    local target
    target="$(h-agent-session-tmux-name "${agent}" "${id}" "${transcript}")" || return 0
    target="$(h-tmux-session-name-sanitize "${target}")"
    test -n "${target}" || return 0
    [[ "${target}" == "${current}" ]] && return 0

    command tmux rename-session -t "${pane}" "${target}" 2>/dev/null || return 0
}
##
#: Claude Code. The name function is in =claude-session.zsh=, next to the
#: transcript machinery it uses.
function claude-code-session-tmux-autoname {
    : "hook body for Claude Code's SessionStart and UserPromptSubmit: <tmux-pane> [payload]"
    #: `$1' is passed by the hook line as "$TMUX_PANE": this runs in the
    #: garden, whose environment knows nothing of the pane the agent sits in.
    ##
    local pane="${1}"
    local input
    input="$(h-agent-hook-payload "${2}")"
    test -n "${input}" || return 0

    local id transcript
    id="$(ec "${input}" | jq -r '.session_id // empty' 2>/dev/null)"
    transcript="$(ec "${input}" | jq -r '.transcript_path // empty' 2>/dev/null)"

    h-agent-tmux-autoname claude "${pane}" "${id}" "${transcript}"
}
##
#: Codex. Threads are named in =$CODEX_HOME/session_index.jsonl=, one line
#: per update, so the last line for an id carries its current name.
function codex-thread-name {
    : "prints the current name of a Codex thread, or nothing if it has none"
    local id="${1}"
    test -n "$id" || return 1
    local config_home="${CODEX_HOME:-${HOME}/.codex}"
    local index="${config_home}/session_index.jsonl"

    test -r "${index}" || return 0
    #: Reduce records before printing: tail would lose multiline names, and
    #: skipping an empty/null/missing name would resurrect an earlier title.
    command jq --null-input --raw-output --arg id "${id}" '
        reduce inputs as $entry ("";
            if ($entry | type) == "object" and $entry.id == $id then
                ($entry.thread_name | if type == "string" then . else "" end)
            else . end)
    ' "${index}" 2>/dev/null
}

function h-codex-session-tmux-name {
    : "prints the tmux session name for a Codex thread: '+Codex <name>'"
    local id="${1}"
    assert-args id @RET

    local name
    name="$(codex-thread-name "${id}")"
    ec "${agent_tmux_name_marker}Codex ${name:-${id[1,8]}}"
}

function codex-session-tmux-autoname {
    : "hook body for Codex's SessionStart and UserPromptSubmit: <tmux-pane> [payload]"
    local pane="${1}"
    local input
    input="$(h-agent-hook-payload "${2}")"
    test -n "${input}" || return 0

    local id transcript
    id="$(ec "${input}" | jq -r '.session_id // empty' 2>/dev/null)"
    transcript="$(ec "${input}" | jq -r '.transcript_path // empty' 2>/dev/null)"

    h-agent-tmux-autoname codex "${pane}" "${id}" "${transcript}"
}
##
#: Antigravity. Conversations are summarised in a sqlite database: `title'
#: is the name the user gave with `/rename' (or F2 in `/resume'), empty until
#: then, and `preview' is the title the model generated. The user's wins.
#:
#: Hooks live in =~/.gemini/config/hooks.json= (=configFiles/antigravity/=),
#: run via `sh -c' with agy's environment plus ANTIGRAVITY_CONVERSATION_ID,
#: synchronously, so the hook line backgrounds the garden call and answers
#: `{}' to keep agy's log clean. `SessionStart' fires at conversation start
#: and `Stop' once per turn; `PostInvocation' would fire per model call.
#: Non-tool events take the handler object directly, not a matcher group.
typeset -g agy_summaries_db="${HOME}/.gemini/antigravity-cli/conversation_summaries.db"

function agy-conversation-name {
    : "prints the title, else the preview, of an Antigravity conversation; nothing if neither"
    local id="${1}"
    assert-args id @RET
    local db="${agy_summaries_db}"

    #: The id is interpolated into SQL; only a UUID gets through.
    if [[ "${id}" != [0-9a-fA-F-]## ]] ; then
        ecerr "$0: not a conversation id: ${id}"
        return 1
    fi
    ensure-cmd sqlite3 @RET
    test -r "${db}" || return 0

    command sqlite3 -readonly "${db}" \
        "select coalesce(nullif(title, ''), nullif(preview, '')) from conversation_summaries where conversation_id = '${id}' limit 1;" 2>/dev/null
}

function h-agy-session-tmux-name {
    : "prints the tmux session name for an Antigravity conversation: '+Agy <name>'"
    local id="${1}"
    assert-args id @RET

    local name
    name="$(agy-conversation-name "${id}")"
    ec "${agent_tmux_name_marker}Agy ${name:-${id[1,8]}}"
}

function agy-session-tmux-autoname {
    : "hook body for Antigravity's hooks: <tmux-pane> [payload]"
    local pane="${1}"
    local input
    input="$(h-agent-hook-payload "${2}")"
    test -n "${input}" || return 0

    local id transcript
    id="$(ec "${input}" | jq -r '.conversationId // empty' 2>/dev/null)"
    transcript="$(ec "${input}" | jq -r '.transcriptPath // empty' 2>/dev/null)"

    h-agent-tmux-autoname agy "${pane}" "${id}" "${transcript}"
}
