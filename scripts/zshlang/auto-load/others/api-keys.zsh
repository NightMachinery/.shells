##
#: Our localhost HTTP services (BrishGarden, blackbutler, JupyterGarden) require
#: an API key. Binding to 127.0.0.1 excludes other *hosts* but not the other
#: *users* of this machine, nor a browser tricked into POSTing to localhost, and
#: these services run arbitrary zsh/Python.
#:
#: Each key lives in `~/.keys/<service>` and holds a complete header line,
#: `X-API-Key: <key>`, so that clients can send it with `curl --header @<file>`.
#: That keeps the key out of argv, which `ps` exposes to every local user - the
#: very users we are excluding.
##
function api-key-file-get {
    local name="${1:?}"

    print -r -- "$HOME/.keys/${name}"
}

function h-api-key-write {
    : "usage: h-api-key-write <key file>
Writes a new random key to <key file>, as an X-API-Key header line. An old
file is replaced in one rename, so a reader sees the old key or the new one,
never a partial file. Prints nothing."
    local key_file="${1:?}"

    command mkdir -p -m 700 -- "${key_file:h}" || return $?
    #: `mkdir -m` does not touch an already existing directory.
    chmod 700 "${key_file:h}" || return $?

    #: @duplicateCode/1eb4b0a0e4b4b0a3f0b7f56bd4d40e0a (`~/.redis-auth` in the bootstrap stages)
    #: Some `base64` builds (linuxbrew's, on our VPS) emit CRLF, and a stray
    #: CR makes the key an invalid HTTP header value - Caddy rejects it with
    #: `invalid header field value` and every proxied request 502s. Delete
    #: CR along with LF and the padding, and map to URL-safe base64 so the
    #: key matches what `secrets.token_urlsafe` produces on the Python side.
    #: The key travels through pipes and a builtin `printf`, never an argv.
    local key
    key="$(head -c 32 /dev/urandom | base64 | tr -d '\r\n=' | tr '+/' '-_')"
    #: 32 bytes are 43 characters of unpadded base64; anything else means a
    #: tool in the pipe failed, and an empty key would let anyone in.
    if (( ${#key} != 43 )) ; then
        ecerr "$0: could not generate a key"
        return 1
    fi

    local tmp="${key_file}.new.$$"
    #: The redirection must stay *inside* the subshell, or the file is
    #: created by the caller under the caller's umask.
    if ! (
            umask 077
            printf -- 'X-API-Key: %s\n' "$key" >| "$tmp"
        ) ; then
        command rm -f -- "$tmp"
        return 1
    fi
    #: macOS `chmod` has no `--`; `$tmp` is always absolute, so it needs none.
    chmod 600 "$tmp" &&
        command mv -f -- "$tmp" "$key_file" || {
            command rm -f -- "$tmp"
            return 1
        }
}

function api-key-get {
    #: Prints the API key of a localhost service, creating it if absent.
    #: The servers generate the same file themselves at boot (see
    #: `pynight/common_apikey.py`); this exists for the launchers, which may
    #: need the key before the server that owns it has finished starting.
    ##
    local name="${1:?api-key-get: service name required}"
    local key_file
    key_file="$(api-key-file-get "$name")" || return $?

    if ! test -s "$key_file" ; then
        h-api-key-write "$key_file" || return $?
    fi

    local line
    #: `read <` instead of `$(<...)` to avoid a fork, and `|| true` to tolerate a
    #: missing trailing newline.
    IFS= read -r line < "$key_file" || true

    #: `read` splits on LF only, so strip a CR from a file written elsewhere.
    line="${line%$'\r'}"

    print -r -- "${line#X-API-Key: }"
}

typeset -gA api_key_holders
#: The sessions that read a service's key once, when they start, and keep the
#: old one until they restart: tmux session names, or the names of jobs that
#: [agfi:tmux2kitty] moved into kitty. For the garden, the garden itself and,
#: on the VPS, Caddy (session `serve-dl`, see =launchers/various.zsh=), which
#: vouches for remote callers with the garden's key.
api_key_holders[brishgarden]='BrishGarden serve-dl'

function api-key-rotate {
    : "usage: api-key-rotate <service>
Gives a localhost service a new random key, then restarts the sessions of
api_key_holders[<service>] that run on this host. Never prints a key."
    #: Clients read the key file per call, so they switch at once. A holder
    #: that has not restarted yet refuses them (the garden) or sends the old
    #: key upstream (Caddy) until it does.
    ##
    local name="${1:?api-key-rotate: service name required}"
    local -a holders=( ${(s: :)api_key_holders[$name]} )

    #: Restarting a holder from inside it kills this function half way:
    #: after the new key is written, before the holder has read it.
    if test -n "${brish_server_index}" ; then
        ecerr "$0: refusing to run inside a Brish worker, which a restart of the garden would kill; run it from a terminal"
        return 1
    fi
    if test -n "${TMUX}" ; then
        local here
        here="$(command tmux display-message -p '#{session_name}' 2>/dev/null)"
        if (( ${holders[(Ie)$here]} )) ; then
            ecerr "$0: refusing to run inside tmux session ${(qq)here}, which holds the key and would be restarted under us"
            return 1
        fi
    fi

    local key_file
    key_file="$(api-key-file-get "$name")" || return $?
    if ! h-api-key-write "$key_file" ; then
        ecerr "$0: could not write a new key to ${key_file}; the old key stays"
        return 1
    fi
    ecgray "$0: ${name} has a new key in ${key_file}"

    local s ret=0 found_p=''
    for s in "${holders[@]}" ; do
        if tmux-session-id "$s" &>/dev/null ; then
            found_p=y
            tmux-session-restart "$s" || ret=1
        elif (( ${+functions[tmux2kitty-restart]} )) && h-tmux2kitty-moved-p "$s" ; then
            found_p=y
            tmux2kitty-restart "$s" || ret=1
        fi
    done

    if (( ret )) ; then
        ecerr "$0: a holder of ${name}'s old key did not restart; restart it by hand, as it keeps the old key until then"
    elif test -z "$found_p" ; then
        ecgray "$0: none of ${holders[*]:-its holders} runs here"
    fi
    return "$ret"
}

function caddy-run-garden {
    : "usage: caddy-run-garden <caddy argument>...
Runs caddy with GARDEN_KEY set to this host's garden key, which the Caddyfile
injects upstream, read here so that it appears in no argv."
    #: `ps` shows every process's argv to every local user, and a launcher
    #: that handed tmux `GARDEN_KEY=<key> caddy ...` put the key in the argv
    #: of the pane's shell. A process's environment is readable only by its
    #: owner and root. [agfi:api-key-rotate] restarts this session to pass a
    #: new key on.
    ##
    local garden_key
    garden_key="$(api-key-get brishgarden)" || ecerr "$0: could not read the garden's API key; remote access will 401"

    GARDEN_KEY="$garden_key" exec caddy "$@"
}
##
