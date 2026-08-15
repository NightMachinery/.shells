#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: Stage 10: vcsh + the dotfiles.
#:
#: This is the real unblock and it needs nothing compiled: vcsh is a single
#: POSIX sh script whose only hard dependency is git, and these hosts ship
#: zsh 5.9 already.

have git || die "git is required and was not found"

##
#: --- vcsh ---
#: vcsh 2.x is autotools-based, so there is no runnable script at the repo
#: root (only vcsh.in, full of @GIT@/@SED@ placeholders). Upstream ships a
#: pre-substituted `vcsh-standalone.sh` as a release asset; that is the one
#: file we want.
: "${VCSH_RELEASE:=v2.0.10}"
: "${VCSH_SHA256:=f31624638c8ccec799f31c29d917fc5a3cdb9356aed3eeb785aa6aac23cd54ed}"

if have vcsh ; then
    ok "vcsh already present: $(command -v vcsh)"
else
    log "installing vcsh ${VCSH_RELEASE} into ${NIGHT_BIN}"
    vcsh_url="https://github.com/RichiH/vcsh/releases/download/${VCSH_RELEASE}/vcsh-standalone.sh"
    fetch "${vcsh_url}" "${NIGHT_BIN}/vcsh.tmp" || die "could not download vcsh"

    if have sha256sum ; then
        got="$(sha256sum "${NIGHT_BIN}/vcsh.tmp" | cut -d' ' -f1)"
        if [ "${got}" != "${VCSH_SHA256}" ] ; then
            rm -f "${NIGHT_BIN}/vcsh.tmp"
            die "vcsh checksum mismatch: got ${got}, expected ${VCSH_SHA256}"
        fi
        dim "checksum ok"
    else
        warn "sha256sum unavailable; skipping checksum verification"
    fi

    mv "${NIGHT_BIN}/vcsh.tmp" "${NIGHT_BIN}/vcsh"
    chmod +x "${NIGHT_BIN}/vcsh"
    path_prepend "${NIGHT_BIN}"
    have vcsh || die "vcsh installed but not on PATH"
    ok "vcsh $(vcsh --version 2>&1 | head -1)"
fi

##
#: --- back up colliding dotfiles ---
#: vcsh refuses to clone over existing files. Rather than guess, ask the
#: remote what it tracks at top level and back up exactly those.
if vcsh list 2>/dev/null | grep -qx "${NIGHT_DOTFILES_VCSH_NAME}" ; then
    ok "vcsh repo '${NIGHT_DOTFILES_VCSH_NAME}' already exists"
else
    log "determining which files the dotfiles repo would overwrite"

    probe_dir="${NIGHT_LOCAL_CACHE}/dotfiles-probe.git"
    rm -rf "${probe_dir}"
    if run git clone --quiet --bare --depth 1 "${NIGHT_DOTFILES_URL}" "${probe_dir}" ; then
        tracked_top="$(git --git-dir="${probe_dir}" ls-tree --name-only HEAD 2>/dev/null)"
    else
        warn "bare probe clone failed; falling back to a known collision list"
        tracked_top='.bashrc .inputrc .profile .zshrc .zshenv .zprofile .bash_profile .curlrc .tmux.conf'
    fi

    for f in ${tracked_top} ; do
        #: Only back up real pre-existing files, and never touch a directory
        #: we did not create (e.g. .config, .ssh).
        target="${HOME}/${f}"
        if [ -f "${target}" ] && [ ! -L "${target}" ] ; then
            backup_file "${target}"
        elif [ -d "${target}" ] ; then
            dim "leaving directory alone: ${target}"
        fi
    done

    #: ~/.emacs is NOT tracked by the repo, but it shadows Doom's init
    #: (emacs prefers ~/.emacs.el, then ~/.emacs, then ~/.emacs.d/init.el),
    #: so it must go before stage 50 or Doom silently never loads.
    for f in "${HOME}/.emacs" "${HOME}/.emacs.el" ; do
        [ -f "${f}" ] && backup_file "${f}"
    done

    rm -rf "${probe_dir}"

    ##
    log "cloning dotfiles"
    run vcsh clone "${NIGHT_DOTFILES_URL}" "${NIGHT_DOTFILES_VCSH_NAME}" \
        || die "vcsh clone failed"
fi

##
#: --- things the dotfiles assume exist ---
#: brishzq.zsh assumes ~/.privateShell exists.
if [ ! -e "${HOME}/.privateShell" ] ; then
    : > "${HOME}/.privateShell"
    ok "created ~/.privateShell"
fi

#: Wire the storage contract into the interactive shell exactly once.
if ! grep -q 'night-bootstrap.env' "${HOME}/.privateShell" 2>/dev/null ; then
    cat >> "${HOME}/.privateShell" <<'EOF'
##
#: Added by setup/bootstrap. Storage contract for this host.
if [ -r "${HOME}/.night-bootstrap.env" ] ; then
    . "${HOME}/.night-bootstrap.env"
fi
##
EOF
    ok "wired ~/.night-bootstrap.env into ~/.privateShell"
fi

##
[ -d "${NIGHTDIR}" ] || die "expected ${NIGHTDIR} after clone, but it is missing"
ok "dotfiles at ${NIGHTDIR}"
