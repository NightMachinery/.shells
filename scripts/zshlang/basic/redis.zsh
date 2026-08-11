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
    #: bootstrapped by setup/bootstrap-sudoless, the file existing is enough,
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
    #: setup/bootstrap-sudoless generates the same file at bootstrap time.)
    ##
    local auth_file="${redis_auth_file}"

    if test -s "${auth_file}" ; then
        IFS= read -r REDISCLI_AUTH < "${auth_file}" || true
        export REDISCLI_AUTH
        return 0
    fi

    local secret
    #: hex, not base64: base64 on some hosts emits CRLF, and a stray CR that
    #: `tr -d '\n'` misses would silently become part of the password. `od' is
    #: POSIX, so this works where xxd (a vim dependency) is missing.
    secret="$(od -An -tx1 -N32 /dev/urandom | tr -d ' \r\n')" || {
        ecerr "$0: could not read /dev/urandom"
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
            chmod 600 "${tmp}"
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

    #: Persists it across restarts, but only where redis was started from a
    #: config file. Our own [agfi:night-startup-redis] passes everything on
    #: the command line and has none, so REWRITE fails there - harmlessly,
    #: because that path re-reads ${redis_auth_file} on every start anyway.
    if silent command redis-cli --no-auth-warning CONFIG REWRITE ; then
        #: CONFIG REWRITE writes the password *in plaintext* into the config
        #: file, and leaves its mode alone. Homebrew ships redis.conf as 644,
        #: so on the very hosts this is meant to protect the rewrite would
        #: hand the secret to every local user. Lock it down.
        local conf
        conf="$(command redis-cli --no-auth-warning --raw INFO server 2>/dev/null | command grep -m1 '^config_file:')"
        conf="${${conf#config_file:}%$'\r'}"
        if test -n "${conf}" && test -f "${conf}" ; then
            chmod 600 "${conf}" 2>/dev/null ||
                ecerr "$0: WARNING: could not chmod 600 ${conf}; it now contains the password in plaintext"
        fi

        ecgray "$0: requirepass set, written to ${conf:-the config file} (mode 600)"
    else
        ecgray "$0: requirepass set for the running server, but NOT persisted (redis has no config file). It must be restarted with --requirepass."
    fi
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
