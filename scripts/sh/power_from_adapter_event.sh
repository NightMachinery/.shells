#!/usr/bin/env bash

#: Power events hand us launchd's environment, not a shell's, so REDISCLI_AUTH
#: has to be loaded explicitly or redis answers NOAUTH.
: "${NIGHTDIR:=$HOME/scripts}"
. "${NIGHTDIR}/sh/redis-auth.sh"

redis-cli set battery_p n
