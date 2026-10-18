##
#: One report for every coding agent: what is left of each one's quota, and
#: when it comes back.
#:
#: There is no shared quota API to call -- each agent's status command is its
#: own program with its own transport ([agfi:claude-code-usage-all] talks HTTP,
#: [agfi:codex-status] starts `codex', [agfi:agy-status] starts `agy') -- so
#: this fans out to each selected provider's JSON command, then renders
#: compact aligned account blocks with a shared human-only renderer.
#:
#: The fan-out goes through the repository's GNU parallel wrapper
#: ([agfi:parallelm]) rather than shell job control. Job control
#: would mean either interleaved output or a temporary file per agent; `parallelm'
#: with =--keep-order= already buffers each job and replays them in input
#: order, which keeps provider sections stable and lets the local renderer
#: stream each completed section without waiting for later providers. Jobs
#: run through =brishz_para.dash=, i.e. inside
#: BrishGarden shells, so zshlang functions are callable there -- and so the
#: garden must be restarted (=brishz-restart=) after any edit to the functions
#: below, or it will keep running the versions it loaded at start-up.
#:
#: See =docs/agent-usage-armed.md=.
##
#: Colour of the combined report: =auto= means "colour iff our own stdout is a
#: terminal". An enum rather than a switch, because "decide for me" is a third
#: state that a boolean cannot carry, and it is the default.
typeset -g agent_status_color="${agent_status_color:-auto}"
#: The agents to report on, overriding the default list in
#: [agfi:agent-status]. Also the order the sections come out in.
typeset -ga agent_status_agents
##
function h-color-mode-p {
    #: Whether an =auto|always|never= colour knob means "colour", with =auto=
    #: decided from the *caller's* own stdout.
    #:
    #: A predicate rather than something that prints =always= or =never=,
    #: because printing would force every caller into a command substitution --
    #: and inside one, fd 1 is a pipe, so =auto= would answer "no colour" even
    #: for a report going straight to the terminal. That is not hypothetical:
    #: it is the bug this shape exists to make unwritable.
    #:
    #: An unrecognized value is said on stderr and answered as "no colour":
    #: plain text is the safe way to be wrong about a colour knob.
    #: Usage: h-color-mode-p [auto|always|never]
    ##
    local mode="${1:-auto}"

    case "${mode}" in
        always)
            return 0
            ;;
        never)
            return 1
            ;;
        auto)
            isOutTty
            ;;
        *)
            ecerr "$0: unknown colour mode: ${mode} (auto, always, never)"
            return 1
            ;;
    esac
}

function h-agent-status-collect {
    #: One compact JSON envelope per provider, for the local human renderer.
    #: Provider JSON contracts remain owned by their original commands.
    local agent="${1}" report='' retcode=0 cache_files='{}' id
    local -a reply
    case "${agent}" in
        claude) report="$(claude-code-usage-all --json 2>/dev/null)" || retcode=$? ;;
        codex) report="$(codex_status_strip_ansi_p=y codex-status --json 2>/dev/null)" || retcode=$? ;;
        agy)
            report="$(agy-status --json 2>/dev/null)" || retcode=$?
            for id in "${(@f)$(ec "${report}" | command jq --raw-output '.[]?.account' 2>/dev/null)}" ; do
                if h-agy-account-info "${id}" 2>/dev/null ; then
                    cache_files="$(ec "${cache_files}" | command jq --arg id "${id}" --arg file "${reply[5]}" '. + {($id): $file}')"
                fi
            done ;;
        *) retcode=2 ;;
    esac
    command jq --null-input --compact-output --arg agent "${agent}" \
        --arg report "${report}" --argjson retcode "${retcode}" --argjson files "${cache_files}" \
        '{agent:$agent, exit:$retcode, report:(try ($report | fromjson) catch null), cache_files:$files}'
}

function h-agent-status-one {
    #: Compatibility entry point for one provider's formatted section.
    local agent="${1}" color="${2:-never}" columns="${3:-80}"
    h-agent-status-collect "${agent}" | command python3 \
        "${NIGHTDIR:-${HOME}/scripts}/python/agent_status_render.py" \
        --columns "${columns}" --color "${color}"
}

function agent-status {
    #: Collect concurrently in the garden; format beside the user's terminal.
    bella_zsh_disable1
    local color=never columns="${agent_status_columns:-${COLUMNS:-80}}"
    [[ "${columns}" == <1-> ]] || columns=80
    if [[ -t 1 && ! -v NO_COLOR ]] && h-color-mode-p "${agent_status_color:-auto}" ; then
        color=always
    fi
    ensure-cmd python3 jq @RET
    ensure-array agent_status_agents
    local agents=("${agent_status_agents[@]}")
    if (( ${#agents} == 0 )) ; then
        agents=( claude codex )
    fi
    parallel_halt=never parallelm --keep-order h-agent-status-collect '{}' ::: "${agents[@]}" |
        command python3 "${NIGHTDIR:-${HOME}/scripts}/python/agent_status_render.py" \
            --columns "${columns}" --color "${color}" --absolute "${agent_status_absolute_p:-auto}" |
        pager-if-overflow-streaming
}
aliasfn agst agent-status
aliasfn as agent-status #: @nameconfilct (macOS)
##
