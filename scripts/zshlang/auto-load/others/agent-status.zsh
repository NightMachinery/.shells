##
#: One report for every coding agent: what is left of each one's quota, and
#: when it comes back.
#:
#: There is no shared quota API to call -- each agent's status command is its
#: own program with its own transport ([agfi:claude-code-usage-all] talks HTTP,
#: [agfi:codex-status] starts `codex', [agfi:agy-status] starts `agy') -- so
#: this is a fan-out, not an abstraction: run all three, label each block, and
#: hand the lot to a pager.
#:
#: The fan-out goes through the repository's GNU parallel wrapper
#: ([agfi:parallelm], alias =para=) rather than shell job control. Job control
#: would mean either interleaved output or a temporary file per agent; `para'
#: with =--keep-order= already buffers each job and replays them in input
#: order, which is exactly the "one section per agent, in the configured
#: order" this wants. Its jobs run through =brishz_para.dash=, i.e. inside
#: BrishGarden shells, so zshlang functions are callable there -- and so the
#: garden must be restarted (=brishz-restart=) after any edit to the functions
#: below, or it will keep running the versions it loaded at start-up.
#:
#: See =docs/agent-usage-notif.md=.
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

function h-agent-status-one {
    #: One agent's section: a header naming it, a rule, then that agent's own
    #: status report. Always succeeds: a failing report is said inside the
    #: section and the section still appears, so that one agent being down
    #: cannot cost you the other two.
    #: Usage: h-agent-status-one <agent> [always|never]
    ##
    local agent="${1}" color="${2:-never}"
    assert-args agent @RET

    #: Raw SGR strings from zsh's own `colors', not [agfi:colorfg]: those gate
    #: themselves on [agfi:isColor] and [agfi:true-color-p], which inspect the
    #: environment -- and the environment here is a garden worker's, not the
    #: terminal's. The decision has already been made by our caller.
    local c_head='' c_rule='' c_err='' c_off=''
    if [[ "${color}" == always ]] ; then
        c_head="${fg_bold[blue]}"
        c_rule="${fg[blue]}"
        c_err="${fg_bold[red]}"
        c_off="${reset_color}"
    fi

    #: A name that is not in [agfi:h-agents-table] still gets a section saying
    #: so, rather than a job that dies before printing anything: every agent
    #: that was asked for has to be accounted for in the report, and a typo in
    #: =agent_status_agents= is exactly the case where that matters.
    local glyph label
    glyph="$(h-agent-field "${agent}" glyph 2>/dev/null)" || glyph='❔'
    label="$(h-agent-field "${agent}" label 2>/dev/null)" || label="${agent}"

    local header="${glyph} ${label}"
    #: One column wider than the character count, because the glyph is a
    #: double-width emoji and `${#...}' counts code points.
    local rule="${(l:$(( ${#header} + 1 ))::─:)}"

    ec "${c_head}${header}${c_off}"
    ec "${c_rule}${rule}${c_off}"

    local retcode=0
    case "${agent}" in
        claude)
            #: The wrapper puts its own flags before ours and argparse is
            #: last-wins, so this decides the colour whatever
            #: =claude_code_usage_strip_ansi_p= resolved to.
            claude-code-usage-all --color "${color}" || retcode=$?
            ;;
        codex)
            #: [agfi:codex-status] passes our arguments *before* its own, so
            #: its =--color never= would win over ours; the knob is scoped off
            #: here so that our own enum is the only thing deciding.
            codex_status_strip_ansi_p=n codex-status --color "${color}" || retcode=$?
            ;;
        agy)
            agy_status_color="${color}" agy-status || retcode=$?
            ;;
        *)
            ec "${c_err}$0: unknown agent: ${agent}${c_off}"
            ec ''
            return 0
            ;;
    esac

    if (( retcode != 0 )) ; then
        #: On stdout rather than through [agfi:ecerr]: `para' collects each
        #: job's stderr separately from its stdout, so a message written there
        #: would surface outside the section it belongs to -- and, with
        #: =--keep-order=, possibly before any section at all.
        ec "${c_err}$0: ${agent}: status report failed (${retcode})${c_off}"
    fi

    #: The section separator. Trailing rather than leading so that the report
    #: does not open with a blank line; [agfi:pager-if-overflow] drops it again
    #: on a terminal.
    ec ''

    #: Deliberately zero: `para' should report the *run* as successful even
    #: when an agent could not be reached, since the combined report did come
    #: out. The failure is visible in the section.
    return 0
}

function agent-status {
    #: Every agent's quota at once. See =docs/agent-usage-notif.md=.
    ##
    local color_mode="${agent_status_color:-auto}"

    #: Resolved before the pipeline below, so that =auto= tests *our* stdout
    #: rather than the pipe into the pager. The children are then told the
    #: answer outright: they run in the garden, whose stdout is a pipe, so an
    #: =auto= of theirs could only ever say "no colour".
    local color=never
    h-color-mode-p "${color_mode}" && color=always

    ensure-array agent_status_agents
    local agents=("${agent_status_agents[@]}")
    if (( ${#agents} == 0 )) ; then
        #: One per line so that an agent can be dropped from the report by
        #: commenting its line out.
        agents=(
            claude
            codex
            agy
        )
    fi

    #: =parallel_halt=never=: one agent's report failing must never stop the
    #: others, which is the opposite of [agfi:parallelm]'s default. Belt and
    #: braces with [agfi:h-agent-status-one]'s own `return 0', because a job
    #: can also die in ways the function never sees.
    parallel_halt=never para --keep-order h-agent-status-one '{}' "${color}" ::: "${agents[@]}" |
        pager-if-overflow
}
aliasfn agst agent-status
##
