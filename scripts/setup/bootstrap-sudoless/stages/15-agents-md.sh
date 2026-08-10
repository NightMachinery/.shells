#!/usr/bin/env sh
# -*- mode: sh; sh-shell: sh; -*-
#: Stage 15: machine-local agent instruction files.
#: Runs after the dotfiles clone, since the assembled files are built from
#: sources that arrive with it.
#:
#: Only the untracked, machine-local files are created, and only empty. The
#: per-host files under PE/Agents/hosts/ are git-tracked and deliberately not
#: created here: an empty one committed by accident is worse than none.

for f in \
    "${HOME}/.agents.local.md" \
    "${HOME}/.claude.local.md" \
    "${HOME}/.codex.local.md" \
    "${HOME}/.antigravity.local.md" \
    ; do
    if [ -e "${f}" ] ; then
        continue
    fi

    #: Empty rather than templated: every byte here is prepended to every
    #: prompt on this machine, for every agent.
    if : > "${f}" ; then
        dim "created ${f}"
    else
        warn "could not create ${f}"
    fi
done

ok "machine-local agent instruction files present"

#: The assembled files themselves are written by [agfi:agents-md-sync], which
#: the agent launchers call. It needs zsh, so it is not run from here.
