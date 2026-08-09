#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: Stage 45: the CLI programs the dotfiles call that have no static release
#: binary, via conda-forge.
#:
#: These are the ones that actually broke a real command line on beta:
#:   fribidi  -- `l` (the ls wrapper) pipes through `fribidi --nobreak`
#:   redis    -- brishzq/history machinery connects to 127.0.0.1:6379
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
#: --- redis ---
#: brishzq and the history machinery talk to 127.0.0.1:6379; without it every
#: new shell prints "Could not connect to Redis at 127.0.0.1:6379".
#: One instance per host, listening only on loopback. The data directory MUST
#: be local disk: this NFS mount is local_lock=none, so every lock round-trips
#: to the server, and redis on NFS is a known corruption risk.
if have redis-server ; then
    if redis-cli ping >/dev/null 2>&1 ; then
        ok "redis already running"
    else
        ensure_dir "${NIGHT_LOCAL_CACHE}/redis"
        run_soft redis-server \
            --daemonize yes \
            --bind 127.0.0.1 --port 6379 \
            --dir "${NIGHT_LOCAL_CACHE}/redis" \
            --save '' --appendonly no \
            --logfile "${NIGHT_LOCAL_CACHE}/redis/redis.log"
        if redis-cli ping >/dev/null 2>&1 ; then
            ok "redis started (loopback only, data on local disk)"
        else
            warn "redis did not come up; see ${NIGHT_LOCAL_CACHE}/redis/redis.log"
        fi
    fi
    dim "redis is per-host and does NOT survive a reboot; rerun this stage"
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
