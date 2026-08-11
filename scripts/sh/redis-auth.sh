#!/bin/sh
#: Exports REDISCLI_AUTH for POSIX-sh consumers. Source it, do not run it:
#:
#:     : "${NIGHTDIR:=$HOME/scripts}"
#:     . "${NIGHTDIR}/sh/redis-auth.sh"
#:
#: Redis requires a password (see docs/redis-hardening.md), and `redis-cli'
#: reads it from $REDISCLI_AUTH on its own. Scripts run from launchd, iTerm
#: triggers, Hammerspoon and the like do not inherit an environment that has
#: been through zshlang, so without this they get NOAUTH.
#:
#: The zsh equivalent, with the extra job of minting the secret when none
#: exists, is [agfi:h-redis-auth-ensure]. This deliberately only *reads*: one
#: generator is enough, and a POSIX-sh copy of it would be a second thing to
#: keep in step.
#:
#: @duplicateCode/1eb4b0a0e4b4b0a3f0b7f56bd4d40e0a
##

if [ -z "${REDISCLI_AUTH}" ] && [ -r "${HOME}/.redis-auth" ] ; then
    #: `read' rather than $(cat ...): no fork, which matters because
    #: bicon_zsh.dash is on the interactive shell's startup path.
    #: `|| true' because read reports failure on a file with no trailing
    #: newline, having nonetheless read the value.
    IFS= read -r REDISCLI_AUTH < "${HOME}/.redis-auth" || true
    export REDISCLI_AUTH
fi
