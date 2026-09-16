##
#: Self-expiring advisory holds over an arbitrary resource -- a repository, a
#: GPU, a service -- so one agent can do something atomic without a parallel
#: session walking into it. [agfi:hold-acquire] and friends; see
#: =docs/holds.md=.
#:
#: Generalized from the Hammerspoon auto-reload holds ([agfi:hs-reload-hold]),
#: whose load-bearing property this keeps: a hold always expires on its own, so
#: a crashed -- or merely compacted -- agent cannot leave a resource locked
#: forever. Two deliberate differences from that one:
#:
#: - These are *exclusive*. The Hammerspoon holds are shared, because every
#:   holder wants the same thing (reloading suppressed) and a second one costs
#:   nothing. The point here is mutual exclusion, so a second holder is a
#:   failure rather than a co-signer.
#: - The deadline lives in the file *contents*, not in the mtime. Over there a
#:   Lua reader on Hammerspoon's main thread had to answer from a single stat
#:   with no parsing. Every reader here wants the holder and the reason as
#:   well, so it opens the file regardless, and a deadline in the mtime would
#:   only be a second source of truth to disagree with. It also spares the
#:   PreToolUse guard, which is `sh' and not zsh, a portable-stat problem:
#:   BSD and GNU `stat' spell mtime differently and both are on the PATH here.
#:
#: Deliberately not `zsystem flock', which [agfi:lock-acquire] in =system.zsh=
#: argues well for over redis: a flock is released when the process holding the
#: descriptor dies, and every agent tool call is a *new* process. A hold has to
#: outlive the shell that took it, so it is a self-expiring record instead.
#:
#: Advisory. [agfi:hold-check] only answers the question; the enforcement is
#: =configFiles/claude-code/hooks/hold-guard.sh=, a Claude Code PreToolUse
#: hook, and that stops accidents rather than a determined process.
##
#: Under $HOME, like ${hs_no_reload_dir}, and not under $TMPDIR as first
#: sketched. Every reader has to agree on the path, and the readers here are a
#: login shell, a BrishGarden shell and an `sh' hook -- $TMPDIR is per-context
#: on macOS and simply unset in some of them, which would have split the state
#: silently. $HOME is the one thing they all spell the same way. It is inside
#: the vcsh work tree, which is harmless: that repo is always read with
#: `status -uno'.
typeset -g hold_dir="${hold_dir:-${HOME}/.night-holds}"
#: Same default as the agent banner and the Hammerspoon holds, for the same
#: reason: long enough to be useful, short enough that forgetting it is not a
#: lasting problem.
typeset -g hold_ttl_default="${hold_ttl_default:-30m}"

function h-hold-holder {
    : "a name for whoever is asking: stable across calls, distinct between concurrent sessions"

    #: Same ladder as [agfi:h-hs-reload-holder], and the same caveat: the
    #: Claude Code id is per *session*, so a compaction or a resume changes it
    #: mid-task and the hold becomes unreleasable by name. The deadline is the
    #: real backstop; `hold_holder=<name>' overrides for the rare caller that
    #: has to release someone else's.
    local id="${hold_holder:-}"
    test -n "$id" || id="${CLAUDE_CODE_SESSION_ID:-}"
    test -n "$id" || id="${CODEX_THREAD_ID:-}"
    test -n "$id" || id="${ANTIGRAVITY_CONVERSATION_ID:-}"
    test -n "$id" || id="${TERM_SESSION_ID:-}"
    test -n "$id" || id="$$@${HOST}"

    ec "${id//[^A-Za-z0-9_@.-]/-}"
}

function h-hold-canonical {
    : "<resource> -> the spelling every caller must agree on"

    #: `repo:~/scripts' and `repo:/Users/evar/scripts' are the same resource,
    #: and a guard that did not know that would silently protect nothing. Path
    #: kinds are expanded and resolved; every other kind is left alone, so
    #: `gpu:0' and `service:garden' pass through untouched.
    local resource="${1}"
    assert-args resource @RET

    local kind rest
    case "${resource}" in
        (repo:*|path:*|dir:*|file:*)
            kind="${resource%%:*}"
            rest="${resource#*:}"
            #: Handles `~', `~[nt]' and plain absolute paths alike.
            rest="$(path-unabbrev "${rest}")" @RET
            #: `:A' absolutizes and resolves symlinks, and works on a path that
            #: does not exist yet.
            rest="${rest:A}"
            ec "${kind}:${rest%/}"
            ;;
        (*) ec "${resource}" ;;
    esac
}

function h-hold-slug {
    : "<canonical resource> -> a filename that cannot escape \${hold_dir}"

    #: Lossy, so two resources could in principle collide here.
    #: [agfi:hold-acquire] catches that by comparing the `resource:' line it
    #: finds against its own, rather than trusting the name.
    ec "${1//[^A-Za-z0-9_.@-]/-}"
}

function h-hold-file {
    : "<resource> -> the state file for it, canonicalizing on the way"

    local resource="${1}"
    assert-args resource @RET

    local canonical
    canonical="$(h-hold-canonical "${resource}")" @RET

    ec "${hold_dir}/$(h-hold-slug "${canonical}")"
}

function h-hold-field {
    : "<file> <key> -> the first <key>: line's value, or nothing"

    local f="${1}" key="${2}"
    assert-args f key @RET

    command sed -n -E "s/^${key}:[[:space:]]*//p" "${f}" 2>/dev/null | head -n 1
}

function h-hold-live-p {
    : "<file>: true while the hold in it has not expired; reaps it when it has"

    local f="${1}"
    test -e "$f" || return 1

    local until
    until="$(h-hold-field "$f" until)"
    #: A file with no readable deadline is a corpse from a half-written
    #: acquire; treat it as expired rather than as an eternal hold.
    if [[ "$until" == <-> ]] && (( until > EPOCHSECONDS )) ; then
        return 0
    fi

    command rm -f "$f"
    return 1
}

function hold-acquire {
    : "hold <resource> exclusively for a while; fails when someone else holds it

Usage: hold-acquire <resource> [--ttl <dur>] [--reason <text>] [--match <literal>]...

The resource is any string. \`repo:', \`path:', \`dir:' and \`file:' prefixes
are resolved to an absolute path first, so \`repo:~/scripts' and
\`repo:/Users/evar/scripts' are one resource and not two.

Re-acquiring your own live hold renews it rather than failing, so a long job
can just call this again instead of tracking whether it already holds one.

--match adds a literal string that the PreToolUse guard should treat as
touching this resource, beyond the path itself, and is tested as a plain
substring -- you asked for that exact text. For a vcsh repository it is worth
doing: a command like \`vcsh night.sh commit' names the repo nowhere.

A path resource also gets its own path matched, absolute and \`~'-abbreviated,
but only where a path boundary follows it, so a hold on \`path:~/tmp' does not
block \`ls ~/tmpfoo'."

    local resource="${1}"
    assert-args resource @RET
    shift

    local -a o_ttl o_reason o_match
    zparseopts -D -E -- -ttl:=o_ttl -reason:=o_reason -match+:=o_match || return 1

    local dur="${o_ttl[2]:-${hold_ttl_default}}"
    local reason="${o_reason[2]:-unspecified}"

    local secs
    secs="$(dur2sec "$dur")" @RET

    local canonical holder f
    canonical="$(h-hold-canonical "${resource}")" @RET
    holder="$(h-hold-holder)" @RET
    f="${hold_dir}/$(h-hold-slug "${canonical}")"

    if h-hold-live-p "$f" ; then
        local owner theirs
        owner="$(h-hold-field "$f" holder)"
        theirs="$(h-hold-field "$f" resource)"

        #: Two different resources that slugged to the same name. Refusing is
        #: the conservative answer and it is also vanishingly rare; say so
        #: plainly rather than letting the caller think it holds something.
        if [[ "$theirs" != "$canonical" ]] ; then
            ecerr "$0: name collision: ${canonical} and ${theirs} share a state file"
            return 1
        fi

        if [[ "$owner" != "$holder" ]] ; then
            local left=$(( $(h-hold-field "$f" until) - EPOCHSECONDS ))
            ecerr "$0: ${canonical} is held by ${owner} for another $(seconds-fmt-short "$left")"
            ecerr "  reason: $(h-hold-field "$f" reason)"
            return 1
        fi
        #: Ours. Fall through and rewrite it, which is the renew.
    fi

    local -a matches=()
    local i
    for (( i = 2 ; i <= ${#o_match} ; i += 2 )) ; do
        matches+=("${o_match[i]}")
    done

    #: Written as `path-match:', which the guard tests differently: an
    #: occurrence only counts when a path boundary follows it. A plain
    #: substring test here was wrong -- a hold on `path:~/tmp' denied
    #: `ls ~/tmpfoo', because the held path is a prefix of an unrelated one.
    #: The abbreviated form is derived too, because an agent writes `~/x' at
    #: least as often as the real path.
    local -a path_matches=()
    case "${canonical}" in
        (repo:*|path:*|dir:*|file:*)
            local p="${canonical#*:}"
            path_matches+=("$p")
            path_matches+=("${p/#${HOME}/~}")
            ;;
    esac

    mkdir -p "$hold_dir" @TRET

    {
        ec "resource: ${canonical}"
        ec "holder:   ${holder}"
        ec "until:    $(( EPOCHSECONDS + secs ))"
        ec "acquired: ${EPOCHSECONDS}"
        ec "pid:      $$"
        ec "host:     ${HOST}"
        ec "reason:   ${reason}"
        for i in "${(@u)matches}" ; do
            ec "match:    ${i}"
        done
        for i in "${(@u)path_matches}" ; do
            ec "path-match: ${i}"
        done
    } > "$f" @TRET

    ecgray "$0: holding ${canonical} for $(seconds-fmt-short "$secs") (${reason}); release with hold-release"
}

function hold-release {
    : "drop your hold on <resource>"

    local resource="${1}"
    assert-args resource @RET

    local canonical holder f
    canonical="$(h-hold-canonical "${resource}")" @RET
    holder="$(h-hold-holder)" @RET
    f="${hold_dir}/$(h-hold-slug "${canonical}")"

    if ! h-hold-live-p "$f" ; then
        ecgray "$0: ${canonical} was not held"
        return 0
    fi

    local owner
    owner="$(h-hold-field "$f" holder)"
    if [[ "$owner" != "$holder" ]] ; then
        #: Releasing someone else's is not ours to guess at -- with concurrent
        #: agents it would open the resource under whoever is still working --
        #: so the caller has to say so explicitly:
        #:   hold_holder=<name> hold-release <resource>
        ecerr "$0: ${canonical} is held by ${owner}, not by ${holder}"
        ecerr "  to take it anyway: hold_holder=${owner} hold-release ${resource}"
        return 1
    fi

    command rm -f "$f" @TRET
    ecgray "$0: released ${canonical}"
}

function hold-renew {
    : "push your deadline on <resource> out again; same as re-acquiring"

    local resource="${1}"
    assert-args resource @RET
    shift

    local -a o_ttl
    zparseopts -D -E -- -ttl:=o_ttl || return 1

    local canonical f reason
    canonical="$(h-hold-canonical "${resource}")" @RET
    f="${hold_dir}/$(h-hold-slug "${canonical}")"

    if ! h-hold-live-p "$f" ; then
        ecerr "$0: ${canonical} is not held; use hold-acquire"
        return 1
    fi

    #: Carry the reason and the explicit `--match' literals forward, so renewing
    #: does not quietly narrow what the guard protects. The `path-match:' lines
    #: are not carried: [agfi:hold-acquire] re-derives them from the resource,
    #: and `^match:' does not match `path-match:' anyway.
    reason="$(h-hold-field "$f" reason)"
    local -a matches=("${(@f)$(command sed -n -E 's/^match:[[:space:]]*//p' "$f" 2>/dev/null)}")

    local -a match_opts=()
    local i
    for i in "${matches[@]}" ; do
        test -n "$i" && match_opts+=(--match "$i")
    done

    local -a ttl_opts=()
    test -n "${o_ttl[2]}" && ttl_opts=(--ttl "${o_ttl[2]}")

    hold-acquire "${resource}" "${ttl_opts[@]}" --reason "${reason}" "${match_opts[@]}"
}

function hold-check {
    : "0 when <resource> is free or already yours, 1 when someone else holds it

Silent; for use in a condition. [agfi:hold-status] is the one that prints."

    local resource="${1}"
    assert-args resource @RET

    local canonical holder f
    canonical="$(h-hold-canonical "${resource}")" @RET
    holder="$(h-hold-holder)" @RET
    f="${hold_dir}/$(h-hold-slug "${canonical}")"

    h-hold-live-p "$f" || return 0
    [[ "$(h-hold-field "$f" holder)" == "$holder" ]]
}

function hold-status {
    : "who holds what, why, and for how much longer; one resource when named"

    #: `null_glob' rather than a `(N)' qualifier: the shell snapshot Claude Code
    #: hands its agents runs with `nobareglobqual', where `*(N)' is not a
    #: qualifier at all and the glob fails with `no matches found'. That is a
    #: live bug in [agfi:h-hs-reload-holds-live], which is where this pattern
    #: came from.
    setopt localoptions null_glob

    local -a files=()
    if test -n "${1}" ; then
        local f
        f="$(h-hold-file "${1}")" @RET
        files=("$f")
    else
        test -d "$hold_dir" || {
            ec "holds: none"
            return 0
        }
        files=("${hold_dir}"/*)
    fi

    local f any='' left
    for f in "${files[@]}" ; do
        h-hold-live-p "$f" || continue
        any=y
        left=$(( $(h-hold-field "$f" until) - EPOCHSECONDS ))
        ec "$(h-hold-field "$f" resource) held by $(h-hold-field "$f" holder), $(seconds-fmt-short "$left") left, $(h-hold-field "$f" reason)"
    done

    test -n "$any" || ec "holds: none"
}
