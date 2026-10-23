#!/usr/bin/env zsh
# Sourced by various-darwin.zsh after the host's private startup file.
# See docs/macos-workstation.md for the opt-in profile and dependencies.

if ! command redis-cli ping >/dev/null 2>&1 ; then
    print -ru2 -- 'workstation startup: Redis is unavailable; check org.night.redis in launchctl.'
    return 1
fi

brishgarden-boot "${NIGHT_BRISH_WORKERS:-8}" || return $?
omni-history-boot || return $?

# Desktop manages its bundled daemon by default. A host may explicitly choose
# the independently supervised CLI instead, with Desktop management disabled.
if [[ "${NIGHT_PASEO_STANDALONE:-}" == y ]] && isdefined-cmd paseo ; then
    command paseo daemon start || return $?
fi

# GUI launch is optional so SSH-only sessions need no desktop permissions.
if [[ "${NIGHT_WORKSTATION_OPEN_APPS:-}" == y ]] ; then
    command open -g -a Hammerspoon
    command open -g -a Emacs
    if [[ "${NIGHT_PASEO_STANDALONE:-}" != y ]] ; then
        command open -g -a Paseo
    fi
fi
