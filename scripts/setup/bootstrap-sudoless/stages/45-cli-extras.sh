#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: Stage 45: the CLI programs the dotfiles call that have no static release
#: binary, via conda-forge.
#:
#: These are the ones that actually broke a real command line on beta:
#:   fribidi  -- `l` (the ls wrapper) pipes through `fribidi --nobreak`
#:   redis    -- brishzq/history machinery connects to 127.0.0.1:6379
#:   socat    -- [agfi:pbcopy-remote] forwards the clipboard over the ssh
#:               tunnel with `socat - tcp:127.0.0.1:6030'. Without it every
#:               copy from the server dies with CNOTFOUND.
#: The rest are the long tail from setup/brewables and setup/installables that
#: conda-forge carries as prebuilt binaries.

# shellcheck disable=SC1091
. "${HOME}/.night-bootstrap.env"

have micromamba || die "stage 40 (micromamba) must run first"

: "${NIGHT_TOOLS_ENV:=tools}"

#: conda-forge names. Deliberately NOT the whole of brewables: things already
#: covered by mise (rg/fd/fzf/bat/delta/eza/gh/uv/zoxide/starship) stay with
#: mise so there is one owner per binary.
night_conda_tools='
fribidi
redis-server
socat
pandoc
aria2
ncdu
ugrep
tealdeer
sox
imagemagick
poppler
p7zip
unrar-free
htop
w3m
lynx
jq
moreutils
expect
'

#: `micromamba install -n X` errors outright when X does not exist yet, so the
#: first call must be `create`. Getting this wrong made every package in the
#: fallback loop fail too, for the same reason.
if micromamba env list 2>/dev/null | grep -qE "(^| )${NIGHT_TOOLS_ENV}[ /]" ; then
    ok "env ${NIGHT_TOOLS_ENV} exists; updating"
    mamba_verb=install
else
    log "creating env ${NIGHT_TOOLS_ENV}"
    mamba_verb=create
fi

#: One solve for the whole set. If it fails, fall back to one at a time so a
#: single unsatisfiable package cannot block the other fifteen.
# shellcheck disable=SC2086
if run micromamba "${mamba_verb}" --yes -n "${NIGHT_TOOLS_ENV}" -c conda-forge ${night_conda_tools} ; then
    ok "installed the tools env in one solve"
else
    warn "combined solve failed; falling back to one-by-one"
    #: Make sure the env exists before the loop, or every iteration repeats
    #: the same "env does not exist" failure.
    micromamba env list 2>/dev/null | grep -qE "(^| )${NIGHT_TOOLS_ENV}[ /]" \
        || run_soft micromamba create --yes -n "${NIGHT_TOOLS_ENV}" -c conda-forge
    for p in ${night_conda_tools} ; do
        run_soft micromamba install --yes -n "${NIGHT_TOOLS_ENV}" -c conda-forge "${p}"
    done
fi

##
tools_bin="${MAMBA_ROOT_PREFIX}/envs/${NIGHT_TOOLS_ENV}/bin"
if [ ! -d "${tools_bin}" ] ; then
    die "tools env bin not found at ${tools_bin}"
fi

#: Put the tools env LAST on PATH, so mise's newer static binaries win any
#: name collision (both ship jq, for instance) and this only fills gaps.
if ! grep -q 'NIGHT_TOOLS_BIN' "${HOME}/.night-bootstrap.env" 2>/dev/null ; then
    cat >> "${HOME}/.night-bootstrap.env" <<EOF

#: conda-forge tools env (stage 45). Appended, not prepended: mise owns any
#: binary both provide.
export NIGHT_TOOLS_BIN="${tools_bin}"
case ":\${PATH}:" in
    *":\${NIGHT_TOOLS_BIN}:"*) : ;;
    *) export PATH="\${PATH}:\${NIGHT_TOOLS_BIN}" ;;
esac
EOF
    ok "added the tools env to PATH (appended)"
fi

PATH="${PATH}:${tools_bin}" ; export PATH

##
#: --- redis credential ---
#: redis listens on 127.0.0.1, which excludes other *hosts* but not the other
#: *users* of the same one -- so where anyone else can log in, it must require
#: a password. The secret is generated once here; stage 70 starts the server
#: with it, and the env contract exports REDISCLI_AUTH so existing redis-cli
#: callers are unchanged. Starting the service itself belongs to stage 70.
#:
#: Skipped only when the profile *declares* the host single-user; the default
#: profile says y, so an unconfigured host gets the password.
auth_file="${HOME}/.redis-auth"
if [ "${NIGHT_MULTIUSER:-y}" != y ] ; then
    ok "single-user host: redis needs no password"
elif [ -s "${auth_file}" ] ; then
    ok "redis credential already present"
elif have redis-server ; then
    ( umask 077 ; head -c 32 /dev/urandom | base64 | tr -d '\n=' > "${auth_file}" )
    chmod 600 "${auth_file}"
    ok "generated ${auth_file} (chmod 600)"
fi

##
log "checking the ones that broke real commands"
for c in fribidi redis-server pandoc ncdu ugrep ; do
    if command -v "${c}" >/dev/null 2>&1 ; then
        dim "$(printf '%-14s %s' "${c}" "$(command -v "${c}")")"
    else
        warn "$(printf '%-14s MISSING' "${c}")"
    fi
done
