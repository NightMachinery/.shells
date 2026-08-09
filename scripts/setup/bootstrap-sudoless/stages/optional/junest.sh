#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: Optional: junest -- Arch Linux (pacman + AUR) with no root.
#:
#: This is a LAST-RESORT escape hatch, not a default. Prefer, in order:
#:   1. mise (stage 20)          -- static binaries, no isolation weirdness
#:   2. conda-forge (stage 40)   -- huge catalog, plays well with the rest
#:   3. apt-get download + dpkg-deb -x ~/.local
#:   4. AppImage (FUSE is available on these hosts)
#:   5. junest                   -- only when nothing above has the package
#:
#: Specifically NOT for emacs: junest's emacs has caused problems before.
#:
#: Enable with: BOOTSTRAP_WITH_JUNEST=y sh bootstrap.sh
#:
#: Viability on the CIS hosts was verified: bwrap is installed and
#: kernel.apparmor_restrict_unprivileged_userns = 0, which is unusual for
#: Ubuntu 24.04 and is what makes the `ns` (namespace) backend work.

# shellcheck disable=SC1091
. "${HOME}/.night-bootstrap.env"

flag_on BOOTSTRAP_WITH_JUNEST || {
    dim "junest not requested (BOOTSTRAP_WITH_JUNEST=y to enable)"
    return 0 2>/dev/null || exit 0
}

have bwrap || warn "bwrap not found; junest will fall back to proot (slower)"

junest_dir="${HOME}/.local/share/junest"
if [ -d "${junest_dir}" ] ; then
    ok "junest already installed"
else
    log "cloning junest"
    run git clone --depth 1 https://github.com/fsquillace/junest.git "${junest_dir}" \
        || die "junest clone failed"
fi

PATH="${junest_dir}/bin:${PATH}" ; export PATH
JUNEST_HOME="${HOME}/.junest" ; export JUNEST_HOME

if [ -d "${JUNEST_HOME}/usr" ] ; then
    ok "junest image already set up"
else
    log "junest setup (downloads an Arch bootstrap image)"
    run_soft junest setup
fi

dim "use it with: junest ns -- pacman -Syy <pkg>"
dim "or the fake sudo: \${JUNEST_HOME}/usr/bin_wrappers/sudo pacman -S <pkg>"
