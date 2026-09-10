##
#: Antigravity's side of the agent session helpers: the adapter verbs
#: =agent-session.zsh= dispatches to ([agfi:h-agent-session-call]), and the
#: `agy-*' names for the common cases. A conversation lives at
#: `~/.gemini/antigravity-cli/brain/<conversationId>/', and its readable record
#: is the JSONL transcript under `.system_generated/logs/'; the Go side,
#: =golang/agent_session/internal/agy=, reads it. The tmux naming hook and
#: [agfi:agy-conversation-name] are in =agent-tmux.zsh=. See
#: =docs/agent-sessions.md=.
##
function h-agy-session-home {
    #: Antigravity's state directory.
    ##
    ec "${agy_home:-${HOME}/.gemini/antigravity-cli}"
}

function h-agy-session-roots {
    #: The conversations directory, when it exists; nothing otherwise, which is
    #: how a host without Antigravity contributes no rows.
    ##
    local d
    d="$(h-agy-session-home)/brain"
    test -d "${d}" || return 1
    ec "${d}"
}

function h-agy-session-owns-p {
    #: Whether a transcript is an Antigravity one: it sits under the brain
    #: directory.
    ##
    local transcript="${1:a}"

    local d
    for d in ${(f)"$(h-agy-session-roots 2>/dev/null)"} ; do
        [[ "${transcript}" == "${d:a}"/* ]] && return 0
    done
    return 1
}

function h-agy-session-id-of {
    #: `<brain>/<id>/.system_generated/logs/transcript_full.jsonl' -> the id.
    ##
    ec "${1:h:h:h:t}"
}

function h-agy-session-transcript-of {
    #: The transcript of a conversation id: the full record, else the truncated
    #: one, else failure -- a conversation that has written neither has nothing
    #: to show.
    #: Usage: h-agy-session-transcript-of <conversation-id>
    ##
    setopt localoptions bareglobqual

    local id="${1}"
    assert-args id @RET

    local root logs f
    for root in ${(f)"$(h-agy-session-roots)"} ; do
        logs="${root}/${id}/.system_generated/logs"
        for f in "${logs}/transcript_full.jsonl"(N) "${logs}/transcript.jsonl"(N) ; do
            ec "${f}"
            return 0
        done
    done
    return 1
}

function h-agy-session-resolve {
    #: The transcripts of the conversations whose id starts with the given
    #: prefix, one per line. [agfi:h-agent-session-resolve] decides what several
    #: hits mean.
    ##
    setopt localoptions bareglobqual

    local input="${1}"
    assert-args input @RET

    local -a hits
    local root d t
    for root in ${(f)"$(h-agy-session-roots)"} ; do
        for d in "${root}/${input}"*(N/) ; do
            t="$(h-agy-session-transcript-of "${d:t}")" || continue
            hits+=( "${t}" )
        done
    done
    (( ${#hits} )) || return 1

    print -rl -- "${hits[@]}"
}

function h-agy-session-current-id {
    #: The conversation id Antigravity exports into the shells it runs.
    #: ANTIGRAVITY_TRAJECTORY_ID is a different thing and does not identify a
    #: conversation; hooks get the id in their payload instead.
    ##
    local id="${ANTIGRAVITY_CONVERSATION_ID}"
    if test -z "${id}" ; then
        ecerr "$0: not inside an Antigravity shell (ANTIGRAVITY_CONVERSATION_ID is unset)"
        return 1
    fi
    ec "${id}"
}

function h-agy-session-live-list {
    #: Every running Antigravity conversation, one per line, tab separated:
    #: pid, conversation id, name, cwd, transcript, tmux session (or `-'),
    #: status. The Go side pairs a process to a conversation through the
    #: workspace map Antigravity keeps for "resume here"; see
    #: =golang/agent_session/internal/agy/live.go=.
    ##
    local -a roots
    roots=( ${(f)"$(h-agy-session-roots)"} ) || return 1

    h-agent-session-dep @RET
    agent_session agy live "${roots[@]}"
}

function h-agy-session-name {
    #: The conversation's name. [agfi:agy-conversation-name] asks the SQLite
    #: summaries, which are the authority and what the tmux name uses; the Go
    #: side reads the JSON mirror beside them and stands in when sqlite3 is
    #: missing. Keeping the two in one place is what stops the picker's column
    #: and the tmux session name disagreeing.
    ##
    local transcript="${1}"
    assert-args transcript @RET

    local id name
    id="$(h-agy-session-id-of "${transcript}")" @RET

    if isdefined-cmd sqlite3 ; then
        name="$(agy-conversation-name "${id}" 2>/dev/null)" || name=''
    fi
    if test -n "${name}" ; then
        ec "${name}"
        return 0
    fi

    h-agent-session-dep @RET
    agent_session agy name "${transcript}"
}

function h-agy-session-account {
    #: The signed-in Google account. Antigravity is a Gemini CLI derivative and
    #: shares its account file, `~/.gemini/google_accounts.json', whose
    #: `active' is the address in use; the old ones beside it are not.
    ##
    local f
    f="$(h-agy-session-home)/../google_accounts.json"
    test -e "${f}" || return 1
    isdefined-cmd jq || return 1

    local email
    email="$(jq -r '.active // empty' "${f}" 2>/dev/null)" || return 1
    test -n "${email}" || return 1

    print -r -- "${email}"
}

function h-agy-session-resume {
    #: `agy --conversation <id>', through the [agfi:antigravity] launcher.
    #: Usage: h-agy-session-resume <transcript> [agy args...]
    ##
    local transcript="${1}"
    shift
    assert-args transcript @RET

    local id
    id="$(h-agy-session-id-of "${transcript}")" @RET

    #: In the session's own directory ([agfi:h-agent-session-resume-run]).
    h-agent-session-resume-run "${transcript}" antigravity --conversation "${id}" "$@"
}

function h-agy-session-hook-transcript {
    #: The transcript an Antigravity hook payload is about, or nothing. Its
    #: keys are camelCase: `transcriptPath' when it has one, else the
    #: conversation is found from `conversationId'.
    ##
    local input
    input="$(h-agent-hook-payload "${1}")"
    test -n "$input" || return 1

    #: `(ps:\t:)' rather than `read': tab is IFS whitespace, so `read' would
    #: collapse an empty first field and slide the id into it.
    local -a f
    f=( "${(@ps:\t:)$(ec "$input" | jq -r '[(.transcriptPath // ""), (.conversationId // "")] | @tsv' 2>/dev/null)}" )
    local transcript="${f[1]}" id="${f[2]}"

    if test -n "${transcript}" && test -e "${transcript}" ; then
        ec "${transcript}"
        return 0
    fi
    test -n "${id}" || return 0

    #: The transcript is written as the conversation runs, so early in one there
    #: is nothing yet; that is not an error, just no registration this time.
    h-agy-session-transcript-of "${id}" 2>/dev/null || true
}
##
#: The everyday names. `agent_session_agents=agy' narrows the shared pickers
#: and resolvers to Antigravity.
##
aliasfn agy-resume agent_session_agents=agy agent-session-resume
aliasfn agy-resume-fz agent_session_agents=agy agent-session-resume-fz
aliasfn agy-resume-all-fz agent_session_agents=agy agent_session_resume_scope=all agent-session-resume-fz
aliasfn agy-view-session-fz agent_session_agents=agy agent-view-session-fz
aliasfn agy-view-session-all-fz agent_session_agents=agy agent_session_fz_scope=all agent-view-session-fz
aliasfn agy-session-live-fz agent_session_agents=agy agent-session-live-fz
##
