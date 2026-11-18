##
#: Quota-aware continuation for local Paseo agents. See =docs/paseo-auto-continue.md=.
##
function h-paseo-auto-continue-agent-quotas-p {
    #: Whether this call can go to =agent-quotas continue=: its daemon answers,
    #: the action is one it serves for Paseo (=on=, =off=, =status=, =list=,
    #: =stopped=), the flags are ones it knows, and =on|off|status= name a
    #: Paseo agent by its full UUID (argument or =PASEO_AGENT_ID=). Without
    #: one, =agent-quotas continue= would act on the native session instead,
    #: so the script keeps such a call and refuses it as before. =watch=,
    #: =check --state= and =--state-dir= are the script's own.
    #: Usage: h-paseo-auto-continue-agent-quotas-p <action> [arg...]
    ##
    #: Claude Code's shell runs without EXTENDED_GLOB; the UUID test needs it.
    setopt localoptions extendedglob

    local env_agent="${PASEO_AGENT_ID:-}"

    local action="${1:-}"
    (( $# )) && shift

    local spec agent='' arg
    case "${action}" in
        on) spec='--home= --poll=' ;;
        off|status) spec='--home=' ;;
        list) spec='' ;;
        stopped) spec='--home= --usable' ;;
        *) return 1 ;;
    esac

    #: The agent is the one positional argument, wherever it sits.
    local -a flags=()
    while (( $# )) ; do
        arg="${1}"
        shift
        if [[ "${arg}" != -* ]] ; then
            test -z "${agent}" || return 1
            agent="${arg}"
        elif [[ "${arg}" == --home || "${arg}" == --poll ]] ; then
            flags+=( "${arg}" )
            (( $# )) || return 1
            flags+=( "${1}" )
            shift
        else
            flags+=( "${arg}" )
        fi
    done
    h-agent-quotas-args-ok "${spec}" "${flags[@]}" || return 1

    case "${action}" in
        on|off|status)
            agent="${agent:-${env_agent}}"
            [[ "${agent}" == [[:xdigit:]](#c8)-[[:xdigit:]](#c4)-[[:xdigit:]](#c4)-[[:xdigit:]](#c4)-[[:xdigit:]](#c12) ]] || return 1
            ;;
        *)
            test -z "${agent}" || return 1
            ;;
    esac

    h-agent-quotas-p
}

function h-paseo-auto-continue {
    #: The agent-quotas daemon runs the Paseo engine once it is installed, so
    #: registering through it starts no per-agent tmux watcher; the
    #: registration files and claims are the script's, byte for byte. The
    #: script stays the fallback, and still serves =watch= and =check=.
    #: See =docs/paseo-auto-continue.md=.
    ##
    if h-paseo-auto-continue-agent-quotas-p "$@" ; then
        if [[ "${1:-}" == list ]] ; then
            command agent-quotas continue list --paseo
        else
            command agent-quotas continue "$@"
        fi
        return $?
    fi

    command python3 "${NIGHTDIR}/python/paseo_auto_continue.py" "$@"
}

function paseo-auto-continue-stopped {
    : "usage: paseo-auto-continue-stopped [--home PATH] [--usable]
The local Paseo agents whose last turn stopped at a usage limit, subagents
included, one UUID per line; registered for auto-continue or not. --usable
keeps only those whose seat has quota left for their model now (one usage
check per seat and model family)."
    h-paseo-auto-continue stopped "$@"
}

function paseo-auto-continue-on {
    : "usage: paseo-auto-continue-on [agent-uuid] [--home PATH] [--poll SECONDS]"
    h-paseo-auto-continue on "$@"
}

function paseo-auto-continue-off {
    : "usage: paseo-auto-continue-off [agent-uuid] [--home PATH]"
    h-paseo-auto-continue off "$@"
}

function paseo-auto-continue-status {
    : "usage: paseo-auto-continue-status [agent-uuid] [--home PATH]"
    h-paseo-auto-continue status "$@"
}
