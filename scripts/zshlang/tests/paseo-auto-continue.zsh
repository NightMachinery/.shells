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
print -r -- 'Paseo auto-continue dispatch checks passed'
