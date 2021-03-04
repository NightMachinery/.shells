#!/usr/bin/env zsh
# DEPRECATED use ensurerun instead
myInvocation="$(printf %q "${(%):-%x}")$((($#)) && printf ' %q' "$@")"
echo "$myInvocation"

eval "gtimeout $1 $(gquote "${@:2}")"
res=$?
# echo $res
if [[ $res -eq 124 ]]; then
    >&2 echo
    >&2 echo "Command '${@:2}' timed out in $1. Rerunning ..."
    exec eval "$myInvocation"
fi
