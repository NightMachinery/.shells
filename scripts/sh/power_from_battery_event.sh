#!/usr/bin/env bash
#: [jalali:1401/09/07] These could be used with an app like ControlPlane. But it didn't work for me.
##

#: Power events hand us launchd's environment, not a shell's, so REDISCLI_AUTH
#: has to be loaded explicitly or redis answers NOAUTH.
: "${NIGHTDIR:=$HOME/scripts}"
. "${NIGHTDIR}/sh/redis-auth.sh"

redis-cli set battery_p y
