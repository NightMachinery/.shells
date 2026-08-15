#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: Stage 40: micromamba + conda-forge, rooted on the big store.
#:
#: We install our OWN micromamba rather than using a system conda, because on
#: the CIS cluster /opt/miniconda3 exists ONLY on beta (verified), is conda
#: 23.7.2 with the classic solver, and is pinned to the `defaults` channel.
#: Depending on it would break silently on rho*/zeta*.

# shellcheck disable=SC1091
. "${HOME}/.night-bootstrap.env"

##
if have micromamba ; then
    ok "micromamba already present: $(micromamba --version 2>&1 | head -1)"
else
    log "installing micromamba into ${NIGHT_BIN}"
    tmpd="${NIGHT_LOCAL_CACHE}/micromamba-dl"
    ensure_dir "${tmpd}"

    #: The official tarball puts the binary at bin/micromamba.
    if have curl ; then
        curl -fsSL "https://micro.mamba.pm/api/micromamba/linux-64/latest" \
            | tar -xj -C "${tmpd}" bin/micromamba
    else
        wget -q -O- "https://micro.mamba.pm/api/micromamba/linux-64/latest" \
            | tar -xj -C "${tmpd}" bin/micromamba
    fi
    [ -x "${tmpd}/bin/micromamba" ] || die "micromamba download failed"

    mv "${tmpd}/bin/micromamba" "${NIGHT_BIN}/micromamba"
    chmod +x "${NIGHT_BIN}/micromamba"
    rm -rf "${tmpd}"
    path_prepend "${NIGHT_BIN}"
    ok "micromamba $(micromamba --version 2>&1 | head -1)"
fi

##
ensure_dir "${MAMBA_ROOT_PREFIX}"

#: conda-forge only. Never `defaults`: it carries Anaconda Inc. licensing
#: terms that have been billing universities.
run_soft micromamba config append channels conda-forge
run_soft micromamba config set channel_priority strict
#: `defaults` is appended by some tooling; make sure it is not in play.
run_soft micromamba config remove channels defaults

ok "mamba root: ${MAMBA_ROOT_PREFIX}"
dim "channels: $(micromamba config list 2>/dev/null | tr '\n' ' ' | head -c 200)"

##
#: A general-purpose python env. Kept separate from any tool env so a broken
#: research dependency cannot take the shell down with it.
: "${NIGHT_PY_ENV:=py312}"
: "${NIGHT_PY_VERSION:=3.12}"

if micromamba env list 2>/dev/null | grep -q "[/ ]${NIGHT_PY_ENV}\$\|/${NIGHT_PY_ENV} " ; then
    ok "env ${NIGHT_PY_ENV} already exists"
else
    log "creating env ${NIGHT_PY_ENV} (python ${NIGHT_PY_VERSION})"
    run micromamba create --yes -n "${NIGHT_PY_ENV}" \
        -c conda-forge \
        "python=${NIGHT_PY_VERSION}" pip \
        || die "could not create ${NIGHT_PY_ENV}"
fi

ok "python: $(micromamba run -n "${NIGHT_PY_ENV}" python --version 2>&1)"
