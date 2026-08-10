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
        #: @duplicateCode/1eb4b0a0e4b4b0a3f0b7f56bd4d40e0a (`~/.redis-auth` in the bootstrap stages)
        command mkdir -p -m 700 -- "${key_file:h}" || return $?
        #: `mkdir -m` does not touch an already existing directory.
        chmod 700 "${key_file:h}" || return $?
        #: The redirection must stay *inside* the subshell, or the file is
        #: created by the caller under the caller's umask.
        #: Some `base64` builds (linuxbrew's, on our VPS) emit CRLF, and a stray
        #: CR makes the key an invalid HTTP header value - Caddy rejects it with
        #: `invalid header field value` and every proxied request 502s. Delete
        #: CR along with LF and the padding, and map to URL-safe base64 so the
        #: key matches what `secrets.token_urlsafe` produces on the Python side.
        (
            umask 077
            printf -- 'X-API-Key: %s\n' "$(head -c 32 /dev/urandom | base64 | tr -d '\r\n=' | tr '+/' '-_')" > "$key_file"
        ) || return $?
        #: macOS `chmod` has no `--`; `$key_file` is always absolute, so it needs none.
        chmod 600 "$key_file" || return $?
    fi

    local line
    #: `read <` instead of `$(<...)` to avoid a fork, and `|| true` to tolerate a
    #: missing trailing newline.
    IFS= read -r line < "$key_file" || true

    #: `read` splits on LF only, so strip a CR from a file written elsewhere.
    line="${line%$'\r'}"

    print -r -- "${line#X-API-Key: }"
}
##
