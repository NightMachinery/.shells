#!/usr/bin/env zsh
setopt err_exit nounset
root=${0:A:h:h:h}
source "${root}/zshlang/auto-load/others/agent-auto-continue.zsh"
source "${root}/zshlang/auto-load/others/paseo-auto-continue.zsh"
function h-paseo-auto-continue { print -r -- "$*"; }
export PASEO_AGENT_ID=00000000-0000-4000-8000-000000000001
[[ "$(agent-auto-continue-on --poll 60)" == 'on --poll 60' ]]
[[ "$(agent-auto-continue-off)" == off ]]
[[ "$(agent-auto-continue-status --home /tmp/daemon)" == 'status --home /tmp/daemon' ]]
[[ "$(paseo-auto-continue-on explicit-id --poll 120)" == 'on explicit-id --poll 120' ]]

#: The agent-quotas route: taken only when its daemon answers and the call
#: names a Paseo agent; everything else reaches the script (a stub here).
unfunction h-paseo-auto-continue
source "${root}/zshlang/auto-load/others/agent-quotas.zsh"
source "${root}/zshlang/auto-load/others/paseo-auto-continue.zsh"
tmp="$(command mktemp -d "${TMPDIR:-/tmp}/paseo-auto-continue-tests.XXXXXX")"
trap 'command rm -rf -- "${tmp}"' EXIT
command mkdir -p "${tmp}/bin" "${tmp}/python"
print -r -- 'import sys; print("PY " + " ".join(sys.argv[1:]))' > "${tmp}/python/paseo_auto_continue.py"
command cat > "${tmp}/bin/agent-quotas" <<'SH'
#!/bin/sh
[ "$1" = health ] && exit 0
printf 'AQ %s\n' "$*"
SH
command chmod +x "${tmp}/bin/agent-quotas"
export NIGHTDIR="${tmp}" PATH="${tmp}/bin:${PATH}"
rehash
agent_quotas_p=auto
[[ "$(agent-auto-continue-on --poll 60)" == 'AQ continue on --poll 60' ]]
[[ "$(paseo-auto-continue-status 00000000-0000-4000-8000-000000000002 --home /tmp/daemon)" == 'AQ continue status 00000000-0000-4000-8000-000000000002 --home /tmp/daemon' ]]
[[ "$(h-paseo-auto-continue list)" == 'AQ continue list --paseo' ]]
[[ "$(paseo-auto-continue-stopped --usable)" == 'AQ continue stopped --usable' ]]
#: A non-UUID agent would make agent-quotas act on the native session.
[[ "$(paseo-auto-continue-on explicit-id --poll 120)" == 'PY on explicit-id --poll 120' ]]
[[ "$(PASEO_AGENT_ID='' paseo-auto-continue-off)" == 'PY off' ]]
[[ "$(h-paseo-auto-continue check --state /tmp/x)" == 'PY check --state /tmp/x' ]]
[[ "$(agent_quotas_p=n paseo-auto-continue-off)" == 'PY off' ]]
print -r -- 'Paseo auto-continue dispatch checks passed'
