###
typeset -g redis_auth_file="${HOME}/.redis-auth"

function h-redis-auth-ensure {
    #: On multi-user hosts redis must require a password: binding to 127.0.0.1
    #: excludes other *hosts* but not the other *users* of the same machine,
    #: who could otherwise read our history and brishgarden state.
    #:
    #: `redis-cli` picks the password up from $REDISCLI_AUTH, which the shell
    #: normally exports at startup. Re-read it here so that shells started
    #: *before* the password existed - and any process that inherited a stale
    #: environment - heal themselves instead of failing every redis call with
    #: NOAUTH.
    #:
    #: When no secret exists at all, mint one. Note what this does and does
    #: NOT buy you: it only makes our *clients* send a password. Nothing here
    #: makes the *server* demand one - that is [agfi:redis-harden]. On a host
    #: bootstrapped by setup/bootstrap, the file existing is enough,
    #: because [agfi:night-startup-redis] passes it as --requirepass on the
    #: next start; anywhere redis is started by brew/systemd/the distro it is
    #: not, and the server stays open until something applies the secret.
    ##
    if test -z "${REDISCLI_AUTH}" && test -r "${redis_auth_file}" ; then
        #: `read' rather than "$(<...)": a command substitution forks, and
        #: this runs on every redis call. `|| true' because read reports
        #: failure on a file with no trailing newline, having read the value.
        IFS= read -r REDISCLI_AUTH < "${redis_auth_file}" || true
        export REDISCLI_AUTH
    fi

    if test -z "${REDISCLI_AUTH}" ; then
        #: Guarded so that a permanent failure (read-only home, no /dev/urandom)
        #: costs one attempt per shell instead of a fork on every redis call.
        test -n "${h_redis_auth_attempted}" && return 0
        typeset -g h_redis_auth_attempted=y

        bool "${redis_auth_generate_disable}" && return 0

        h-redis-auth-generate
    fi

    return 0
}

function h-redis-auth-generate {
    #: Mints ~/.redis-auth (mode 600) and exports it, idempotently.
    #:
    #: @duplicateCode/1eb4b0a0e4b4b0a3f0b7f56bd4d40e0a (stage 45 of
    #: setup/bootstrap generates the same file at bootstrap time.)
    ##
    local auth_file="${redis_auth_file}"

    if test -s "${auth_file}" ; then
        IFS= read -r REDISCLI_AUTH < "${auth_file}" || true
        export REDISCLI_AUTH
        return 0
    fi

    local secret
    #: 256 bits as hex. Hex rather than base64 because base64 on some hosts
    #: emits CRLF, and a stray CR that `tr -d '\n'` misses would silently
    #: become part of the password.
    #:
    #: Both branches below draw from the same kernel CSPRNG - `openssl rand`
    #: seeds from /dev/urandom - so neither is "more random" than the other.
    #: openssl is preferred only because it emits the digits directly, with no
    #: whitespace-stripping step to get subtly wrong. The `od' fallback is
    #: POSIX and needs no openssl, which is not guaranteed on a stripped host.
    if (( ${+commands[openssl]} )) ; then
        secret="$(command openssl rand -hex 32 | command tr -d ' \r\n')"
    else
        secret="$(command od -An -tx1 -N32 /dev/urandom | command tr -d ' \r\n')"
    fi || {
        ecerr "$0: could not generate a secret"
        return 1
    }
    if (( ${#secret} != 64 )) ; then
        ecerr "$0: refusing to write a ${#secret}-char secret (expected 64)"
        return 1
    fi

    #: Write-then-hardlink rather than a plain redirect: `ln' fails if the
    #: target exists, atomically and over NFS, so two shells racing here
    #: cannot end up with different secrets. The loser adopts the winner's.
    local tmp="${auth_file}.$$.tmp"
    {
        ( umask 077 ; print -rn -- "${secret}" > "${tmp}" ) &&
            command chmod 600 "${tmp}"
    } || {
        ecerr "$0: could not write ${tmp}"
        command rm -f "${tmp}"
        return 1
    }

    if command ln "${tmp}" "${auth_file}" 2>/dev/null ; then
        ecgray "$0: generated ${auth_file} (mode 600). Redis itself is NOT yet protected; see \`redis-harden\`."
    fi
    command rm -f "${tmp}"

    #: Re-read unconditionally: whether we won the race or not, the file is
    #: now the single source of truth.
    IFS= read -r REDISCLI_AUTH < "${auth_file}" || true
    export REDISCLI_AUTH

    test -n "${REDISCLI_AUTH}"
}

function h-redis-enforcement-assert {
    #: Does the running server actually require our secret?
    #:
    #: Two probes, because either alone is ambiguous. With the secret we must
    #: get PONG, or we have locked ourselves out. Without any secret we must
    #: NOT get PONG, or the server is still serving anonymous clients.
    ##
    local with without

    with="$(command redis-cli --no-auth-warning --raw PING 2>/dev/null)"
    if [[ "${with:l}" != pong ]] ; then
        ecerr "$0: authenticated PING failed after setting requirepass. Redis said: ${with:-<nothing>}"
        return 1
    fi

    #: `env -u' rather than unsetting our own: redis-cli reads REDISCLI_AUTH
    #: from its environment, so it has to be absent in the *child*. env also
    #: resolves redis-cli from PATH, so no alias or function can intercept it.
    without="$(command env -u REDISCLI_AUTH redis-cli --no-auth-warning --raw PING 2>&1)"
    if [[ "${without:l}" == pong ]] ; then
        ecerr "$0: the server still answers unauthenticated clients; requirepass did not take effect."
        return 1
    fi

    return 0
}

function h-redis-conf-protect {
    #: CONFIG REWRITE writes the password *in plaintext* into the config file
    #: and leaves its mode alone, so on a host that ships it world-readable -
    #: Homebrew's redis.conf is 644 - the rewrite hands the secret to every
    #: local user. Take that away.
    #:
    #: `o-rwx', not `600'. The threat here is the other *users* of this
    #: machine, which is exactly the `o' bits; owner and group access must
    #: survive, because the daemon reads this file as itself. Debian ships it
    #: redis:redis 640 and runs the daemon as redis, so a blanket 600 would
    #: strip the group and the daemon would lose its own config on the next
    #: restart - trading a disclosure bug for an outage.
    ##
    local conf="$1"

    command chmod o-rwx "${conf}" 2>/dev/null && return 0

    #: Escalate only after the unprivileged attempt has failed, and only where
    #: root is actually reachable: a password-prompting sudo on a headless host
    #: is a hang, not a question. [agfi:h-sudo-cmd] picks the safe argv.
    if sudo-usable-p ; then
        local sudo_cmd=( ${(@f)"$(h-sudo-cmd)"} )

        if silent "${sudo_cmd[@]}" chmod o-rwx "${conf}" ; then
            ecgray "$0: tightened ${conf} (needed root)"
            return 0
        fi
    fi

    ecerr "$0: WARNING: could not tighten ${conf}. The running server IS protected, but that file now holds the password in plaintext and its mode is unverified. Fix it by hand: chmod o-rwx ${conf}"
    return 1
}

function redis-harden {
    #: Makes the *running* server require our secret, which is the half that
    #: [agfi:h-redis-auth-ensure] cannot do on its own.
    #:
    #: CONFIG SET takes effect immediately and does not drop existing
    #: connections - already-authenticated clients keep working - but every
    #: *new* connection from a client that does not know the secret will fail
    #: with NOAUTH. See the caveats in ./docs/redis-hardening.org before
    #: wiring this into startup.
    ##
    h-redis-auth-ensure
    if test -z "${REDISCLI_AUTH}" ; then
        ecerr "$0: no secret available; nothing to apply"
        return 1
    fi

    #: `command redis-cli', not [agfi:redism]: redism calls
    #: h-redis-auth-ensure, and we are inside it.
    #:
    #: Probe *with* our secret, not without. An unauthenticated probe cannot
    #: tell "no password is set" from "a password is set and we did not send
    #: it" - both come back empty. Sending it is safe against a passwordless
    #: server: redis rejects the AUTH, redis-cli says so on stderr, and the
    #: connection keeps working. Hence 2>/dev/null.
    local out
    out="$(command redis-cli --no-auth-warning --raw CONFIG GET requirepass 2>/dev/null)" || {
        ecerr "$0: cannot reach redis"
        return 1
    }

    #: A successful read is exactly "requirepass\n<value>". Anything else -
    #: NOAUTH, a renamed/disabled CONFIG command - means we did not read it,
    #: and must not be mistaken for "the password is empty".
    local -a lines=( "${(@f)out}" )
    if [[ "${lines[1]}" != requirepass ]] ; then
        ecerr "$0: could not read the current requirepass. Redis said: ${out:-<nothing>}"
        return 1
    fi
    local current="${lines[2]}"

    if test -n "${current}" ; then
        if [[ "${current}" == "${REDISCLI_AUTH}" ]] ; then
            ecgray "$0: already requires our secret"
            return 0
        else
            ecerr "$0: redis already requires a *different* password. Refusing to change it; reconcile ${redis_auth_file} by hand."
            return 1
        fi
    fi

    silent command redis-cli --no-auth-warning CONFIG SET requirepass "${REDISCLI_AUTH}" || {
        ecerr "$0: CONFIG SET requirepass failed"
        return 1
    }

    #: Prove the server now *demands* it, rather than trusting that CONFIG SET
    #: did what it said. A renamed or ACL-restricted CONFIG command can accept
    #: the call and change nothing, and we would then report success over a
    #: still-open server - which is the precise failure ./docs/redis-hardening.md
    #: exists to warn about.
    h-redis-enforcement-assert || return 1

    local rc=0

    #: Persists it across restarts, but only where redis was started from a
    #: config file. Our own [agfi:night-startup-redis] passes everything on
    #: the command line and has none, so REWRITE fails there - harmlessly,
    #: because that path re-reads ${redis_auth_file} on every start anyway.
    #:
    #: Unprivileged on purpose, and sudo could not help: CONFIG REWRITE is
    #: performed by the redis *daemon*, so it turns on the daemon's rights over
    #: its own config file, never on ours. Under a distro package the daemon
    #: usually owns that file, and the rewrite succeeds even on a host where we
    #: cannot so much as stat it.
    if silent command redis-cli --no-auth-warning CONFIG REWRITE ; then
        local conf
        conf="$(command redis-cli --no-auth-warning --raw INFO server 2>/dev/null | command grep -m1 '^config_file:')"
        conf="${${conf#config_file:}%$'\r'}"

        #: Deliberately no `test -f': where the config lives in a directory we
        #: may not traverse - /etc/redis is 750 root:redis on Debian - the test
        #: is false for a file that is certainly there, and we would skip
        #: protecting it without saying so.
        if test -n "${conf}" ; then
            h-redis-conf-protect "${conf}" || rc=1
        fi

        ecgray "$0: requirepass set and written to ${conf:-the config file}"
    else
        ecgray "$0: requirepass set for the running server, but NOT persisted (redis has no config file). It must be restarted with --requirepass."
    fi

    return "${rc}"
}

function ensure-redis {
    (( ${+commands[redis-cli]} )) || {
        ecerr "redis-cli not found. Have you installed redis?"
        return 2
    }

    h-redis-auth-ensure

    [[ "${$(redism ping):l}" == pong ]] || {
        ecerr '`redis-cli ping` failed. Please make sure redis is up.'
        return 1
    }
}
function redis-assert {
    ensure-redis "$@"
}

function redism {
    h-redis-auth-ensure
    revaldbg redis-cli --raw "$@"
    local r=$?
    if (( r == 141 )) ; then
        local cmd="$(gq "$0" "$@")"
        local msg="$0: redis returned $r (is stdout a bad pipe?). Cmd: $cmd"
        ##
        # ecerr $msg
        # ectty $msg
        ##
        ectrace_ret=$r ectrace "$msg"
        ##
        # eval "$cmd"
        return $r
    fi
    return $r
}

function redism-bool {
    local o
    o="$(redism "$@")" @TRET

    if (( o == 1 )) ; then #: success
        return 0
    else
        return 13
    fi
}
##
function redis-defvar {
    local name="${1}"

    if test -z "${name}" ; then
        ecerr "$0: name not supplied"
        return 1
    fi

    fndef "${name}_get" redism get "$name"
    aliasfnq "${name}_set" silent redism set "$name"
    aliasfnq "${name}_setnx" silent redism setnx "$name"
    fndef "${name}_del" silent redism del "$name"
}
##
#: Export the secret at load time, not merely on the first [agfi:redism] call.
#:
#: Plenty of redis access never goes through redism - [agfi:memoi]'s write path
#: calls `redis-cli' directly, iterm_focus.py shells out to it through brish -
#: and those inherit whatever environment their shell had. Doing this once at
#: startup means every child process gets REDISCLI_AUTH, so bare `redis-cli'
#: keeps working rather than failing with NOAUTH.
#:
#: It costs one `read' of a 64-byte file per shell (no fork); the generator
#: runs at most once per machine.
h-redis-auth-ensure
