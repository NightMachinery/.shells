##
#: agent-quotas: one daemon that polls every coding-agent account's quota and
#: runs Paseo auto-continue (https://github.com/NightMachinery/agent-quotas).
#:
#: The quota readers ([agfi:claude-code-usage], [agfi:codex-status],
#: [agfi:agent-status]) and the Paseo auto-continue entry points
#: ([agfi:paseo-auto-continue-on] and its siblings) ask it when it answers,
#: and run their old Python/zsh path otherwise. A host without the daemon (or
#: with it stopped) therefore behaves exactly as before. [agfi:agy-status] has
#: no old path left: without the daemon it runs agent-quotas in its own
#: process (=--direct=).
#:
#: The decision is made on every call, never cached in a global: the daemon
#: can be installed, stopped or restarted while a garden shell lives on, and a
#: stale "yes" would turn every report into an error.
##
#: =n= forces the old path (for [agfi:agy-status], an in-process read), which
#: is how the tests pin it and how to compare the two by hand:
#: =agent_quotas_p=n ccu=. Anything else means "use the daemon
#: when it answers".
typeset -g agent_quotas_p="${agent_quotas_p:-auto}"
##
function h-agent-quotas-p {
    : "usage: h-agent-quotas-p
Succeeds when agent-quotas is installed, not switched off, and its daemon answers."
    #: One socket round trip (=health=), about the cost of starting the binary.
    #: =whence -p= rather than =$+commands=: see =PE/Zsh.org= on why reading
    #: =$commands= is slow in a garden shell.
    ##
    local enabled="${agent_quotas_p:-auto}"

    [[ "${enabled}" == n ]] && return 1
    whence -p agent-quotas >/dev/null || return 1
    command agent-quotas health >/dev/null 2>&1
}

function h-agent-quotas-args-ok {
    : "usage: h-agent-quotas-args-ok <spec> [arg...]
Succeeds when every arg is a flag that <spec> names, so the call can go to agent-quotas.
<spec> is space separated; a flag ending in = takes a value (--color= accepts
both --color X and --color=X)."
    #: The compat subcommands accept a subset of the old scripts' flags. A call
    #: using any other flag (or a positional argument) goes to the old script,
    #: which still honours it, rather than to agent-quotas, which would refuse
    #: it or, worse, ignore it.
    ##
    local spec_str="${1}"
    shift

    local -a spec=( ${(s: :)spec_str} )
    local arg name
    while (( $# )) ; do
        arg="${1}"
        shift
        [[ "${arg}" == --* ]] || return 1

        name="${arg%%=*}"
        if (( ${spec[(Ie)${name}]} )) ; then
            #: A bare flag must not be given a value.
            [[ "${arg}" == *=* ]] && return 1
        elif (( ${spec[(Ie)${name}=]} )) ; then
            if [[ "${arg}" != *=* ]] ; then
                (( $# )) || return 1
                shift
            fi
        else
            return 1
        fi
    done
    return 0
}
##
