#!/usr/bin/env zsh
# Sourced by various-darwin.zsh after the host's private startup file.
# See docs/macos-workstation.md for the opt-in profile and dependencies.

if ! command redis-cli ping >/dev/null 2>&1 ; then
    print -ru2 -- 'workstation startup: Redis is unavailable; check org.night.redis in launchctl.'
    return 1
fi

#: The services below run in tmux, and a tmux server keeps the macOS privacy
#: grants (TCC) of whatever started it; every session made in it later shares
#: them. Started from this LaunchAgent it has none, so omni-history cannot read
#: Brave's history. Started by Kitty's ivy tab it inherits Kitty's Full Disk
#: Access. So let Kitty create the server first, and only then fall back.
if (( ${NIGHT_WORKSTATION_IVY_WAIT:-120} > 0 )) ; then
    integer ivy_deadline=$(( SECONDS + ${NIGHT_WORKSTATION_IVY_WAIT:-120} ))
    until command tmux has-session -t '=ivy' 2>/dev/null ; do
        if (( SECONDS >= ivy_deadline )) ; then
            print -ru2 -- "workstation startup: no ivy tmux session after ${NIGHT_WORKSTATION_IVY_WAIT:-120}s; starting tmux here, without Kitty's privacy grants."
            break
        fi
        sleep 2
    done
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
