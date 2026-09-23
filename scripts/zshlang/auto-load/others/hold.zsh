##
#: Self-expiring advisory holds over an arbitrary resource -- a repository, a
#: GPU, a service -- so one agent can do something atomic without a parallel
#: session walking into it, and so several can hold off one auto-reloader at
#: once. [agfi:hold-acquire] and friends; see =docs/holds.md=.
#:
#: These are wrappers. The work is in =golang/night_hold=, for a reason worth
#: recording: this was shell first, and three of the bugs found writing it were
#: shell footguns rather than logic errors -- a tab IFS collapsing empty fields,
#: `nobareglobqual' in the agent shell breaking `*(N)', and a held path matched
#: as a bare substring so that a hold on `path:~/tmp' denied `ls ~/tmpfoo'. The
#: port also found a fourth that shell cannot express at all: the read-check-
#: write of an acquire had no critical section, so two processes could both see
#: "free" and an exclusive hold could quietly have two holders.
#:
#: What stays here is the part Go cannot do: [agfi:path-unabbrev] resolves
#: `~[nt]' through [agfi:aliasdir], which is zsh-only. Everything else is
#: passed straight through, including the `hold_holder' and `hold_dir'
#: environment overrides, which the binary reads itself.
#:
#: Deliberately not `zsystem flock', which [agfi:lock-acquire] argues well for
#: over redis elsewhere: a flock is released when the process holding the
#: descriptor dies, and every agent tool call is a *new* process. A hold has to
#: outlive the shell that took it, so it is a self-expiring record instead.
#: (The binary does use flock -- for the microseconds of the acquire itself,
#: which is what flock is actually good at.)
##

function h-hold-dep {
    #: Builds the binary on first use and again after its source changes,
    #: like [agfi:h-agent-session-dep]. [agfi:go-local-dep] keeps the check
    #: fork-free: `$commands' is never read, since its first read fills the
    #: whole command hash, and in a garden shell -- which forks per call, so
    #: the hash is never warm -- that is tens of milliseconds every time.
    ##
    go-local-dep night_hold "${NIGHTDIR}/golang/night_hold"
}

function h-hold-resource {
    : "<resource> -> the same resource with any dynamic named directory resolved"

    #: Only for a path kind, and only when it actually starts with a `~': the
    #: binary handles `~/' and `$HOME' itself, so this exists for `~[nt]/x' and
    #: `~cod/x' alone, and paying for a subprocess otherwise would be silly.
    local resource="${1}"
    assert-args resource @RET

    case "${resource}" in
        (repo:\~*|path:\~*|dir:\~*|file:\~*)
            local kind="${resource%%:*}" rest="${resource#*:}"
            case "${rest}" in
                (\~/*|\~) ec "${resource}" ;;
                (*) ec "${kind}:$(path-unabbrev "${rest}")" ;;
            esac
            ;;
        (*) ec "${resource}" ;;
    esac
}

function hold-acquire {
    : "hold <resource> for a while; fails when someone else holds it

Usage: hold-acquire <resource> [--ttl <dur>] [--reason <text>] [--match <literal>]...
                               [--shared] [--wait <dur>] [--holder <id>]

The resource is any string. \`repo:', \`path:', \`dir:' and \`file:' prefixes are
resolved to an absolute path first, so \`repo:~/scripts' and
\`repo:/Users/evar/scripts' are one resource and not two.

Re-acquiring your own live hold renews it rather than failing, so a long job can
just call this again instead of tracking whether it already holds one.

--ttl is usually wrong to pass. By default a hold lasts until you release it or
until the agent holding it dies, which is a better answer than any duration
guessed up front; give one only when you want a hard deadline. Taken without an
agent pid to check -- from a plain shell, or a script -- liveness cannot work
and 30m is imposed instead; [agfi:hold-status] says so when it happens.

--shared lets others hold it at the same time. That is not a weaker lock, it is
a different thing: a suppression registry, where every holder wants the same
outcome and a second costs nothing. [agfi:hs-reload-hold] is the one user.

--wait keeps retrying for that long instead of failing, for when your next step
*is* the held resource and you have nothing better to do.

--match adds a literal that the PreToolUse guard should treat as naming this
resource, tested as a plain substring. For a vcsh repository it is not optional:
\`vcsh night.sh commit' spells the path nowhere. A path resource already matches
its own path, absolute and \`~'-abbreviated, but only where a path boundary
follows -- so a hold on \`path:~/tmp' does not block \`ls ~/tmpfoo'."

    local resource="${1}"
    assert-args resource @RET
    shift

    h-hold-dep @RET
    night_hold acquire "$(h-hold-resource "${resource}")" "$@"
}

function hold-release {
    : "drop your hold on <resource>"

    local resource="${1}"
    assert-args resource @RET
    shift

    h-hold-dep @RET
    night_hold release "$(h-hold-resource "${resource}")" "$@"
}

function hold-renew {
    : "push your deadline on <resource> out again

Rarely needed by an agent: the PreToolUse guard already refreshes a hold while
its holder is making tool calls, so one that is being used does not lapse."

    local resource="${1}"
    assert-args resource @RET
    shift

    h-hold-dep @RET
    night_hold renew "$(h-hold-resource "${resource}")" "$@"
}

function hold-check {
    : "0 when acquiring <resource> would succeed, 1 when someone else holds it

Silent; for use in a condition. [agfi:hold-status] is the one that prints."

    local resource="${1}"
    assert-args resource @RET
    shift

    h-hold-dep @RET
    night_hold check "$(h-hold-resource "${resource}")" "$@"
}

function hold-status {
    : "who holds what, why, and for how much longer; one resource when named"

    h-hold-dep @RET

    if test -z "${1}" ; then
        night_hold status
        return $?
    fi

    local resource="${1}"
    shift
    night_hold status "$(h-hold-resource "${resource}")" "$@"
}

function hold-holders {
    : "the live holders of <resource>, one per line"

    local resource="${1}"
    assert-args resource @RET
    shift

    h-hold-dep @RET
    night_hold holders "$(h-hold-resource "${resource}")" "$@"
}
