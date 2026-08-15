#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: Stage 30: make zsh actually usable -- zinit, terminfo, and the guards that
#: a shared NFS home across ~12 hosts requires.

# shellcheck disable=SC1091
. "${HOME}/.night-bootstrap.env"

have zsh || die "zsh not found (these hosts ship 5.9 at /usr/bin/zsh)"
ok "zsh $(zsh --version)"

##
#: --- never let a shell startup block on a credential prompt ---
#: Plugin clones that hit a private or renamed repo will otherwise sit forever
#: waiting for a username on a tty nobody is watching.
if ! grep -q 'GIT_TERMINAL_PROMPT' "${HOME}/.night-bootstrap.env" 2>/dev/null ; then
    cat >> "${HOME}/.night-bootstrap.env" <<'EOF'

#: Headless host: a git prompt here means a hung shell, not a question.
export GIT_TERMINAL_PROMPT=0
EOF
    ok "disabled interactive git prompts"
fi
export GIT_TERMINAL_PROMPT=0

##
#: --- zinit ---
#: The dotfiles self-install zinit, but doing it here means the first
#: interactive shell is not also the first network-bound one.
zinit_dir="${HOME}/.zinit/bin"
if [ -f "${zinit_dir}/zinit.zsh" ] ; then
    ok "zinit already installed"
else
    log "installing zinit"
    ensure_dir "${HOME}/.zinit"
    chmod g-rwX "${HOME}/.zinit" 2>/dev/null || true
    run git clone --quiet https://github.com/zdharma-continuum/zinit "${zinit_dir}" \
        || warn "zinit clone failed; the shell will retry on first start"
fi

##
#: --- terminfo ---
#: /usr/bin/tic explicitly: a conda env's `tic` is known to be broken.
if [ -e "${NIGHTDIR}/setup/terminfo-24bit.src" ] && [ -x /usr/bin/tic ] ; then
    run_soft /usr/bin/tic -x -o "${HOME}/.terminfo" "${NIGHTDIR}/setup/terminfo-24bit.src"
    if [ -n "${TERMINFO:-}" ] && [ -d "${TERMINFO}" ] ; then
        run_soft /usr/bin/tic -x -o "${TERMINFO}" "${NIGHTDIR}/setup/terminfo-24bit.src"
    fi
    ok "terminfo compiled"
else
    warn "terminfo source or /usr/bin/tic missing; skipping"
fi

##
#: --- first load ---
#: Bounded, and with a process cap: a broken config can recurse through
#: command_not_found_handler and fork until the host notices. (That is a real
#: failure mode we hit here; see the crossplatform.zsh fix.)
log "verifying a clean interactive load (bounded)"
if ( ulimit -u 400 2>/dev/null || true
     timeout -k 5 180 zsh -ic 'exit 0' >/dev/null 2>&1 ) ; then
    ok "interactive zsh loads cleanly"
else
    warn "interactive zsh did not exit cleanly within 180s"
    warn "debug with: PS4='+%N:%i> ' zsh -x -ic 'exit' 2>&1 | tail -50"
fi
