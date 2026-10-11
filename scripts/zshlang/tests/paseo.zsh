#!/usr/bin/env zsh
source "${0:A:h}/fixtures/agent-handoff.zsh"
source "${handoff_test_root}/zshlang/auto-load/others/paseo.zsh"
typeset -g NIGHTDIR="${handoff_test_root}" paseo_state_dir="${handoff_test_tmp}/state"
typeset -gx PASEO_TEST_LOG="${handoff_test_log}"
command mkdir -- "${handoff_test_tmp}/bin"
cat > "${handoff_test_tmp}/bin/python3" <<'SH'
#!/bin/sh
printf 'prepare %s\n' "$*" >> "$PASEO_TEST_LOG"
while [ "$#" -gt 0 ]; do
    if [ "$1" = --output ]; then shift; printf '{}\n' > "$1"; break; fi
    shift
done
SH
cat > "${handoff_test_tmp}/bin/tmux" <<'SH'
#!/bin/sh
printf 'tmux %s\n' "$*" >> "$PASEO_TEST_LOG"
SH
cat > "${handoff_test_tmp}/bin/paseo" <<'SH'
#!/bin/sh
printf 'unexpected direct Paseo call\n' >&2
exit 91
SH
command chmod 700 "${handoff_test_tmp}/bin/"*
path=("${handoff_test_tmp}/bin" "${path[@]}")
function npm-install { print -r -- "install ${(j: :)${(qq)@}}" >> "${handoff_test_log}"; }
function h-npm-install-report { print -r -- "report ${1}" >> "${handoff_test_log}"; }
function tmux-job-start { print -r -- "job ${(j: :)${(qq)@}}" >> "${handoff_test_log}"; }
function tmux-job-running-p { return 1; }
function agent-session-current-agent { print -r -- "${handoff_test_owner}"; }
function agent-session-current-file { print -r -- "${handoff_test_source}"; }
function h-agent-session-call { print -r -- "${handoff_test_id}"; }
function h-codex-session-home { print -r -- "${handoff_test_tmp}/codex-home"; }
unset PASEO_AGENT_ID PASEO_HOME PASEO_HOST
typeset -gx TMUX=fixture
: > "${handoff_test_log}"
paseo-install
expect-log "install '@getpaseo/cli'"
expect-log 'report paseo'
paseo-install 0.10.2
expect-log "install '@getpaseo/cli@0.10.2'"
if paseo-install one two ; then exit 1; fi
handoff_test_source="${handoff_test_tmp}/work-profile/projects/project/${handoff_test_id}.jsonl"
command mkdir -p -- "${handoff_test_source:h}"
print '{}' > "${handoff_test_source}"
handoff_test_live=$'42\t'"${handoff_test_id}"$'\tsource\tcwd\t'"${handoff_test_source}"$'\ttmux\tworking\tinteractive'
: > "${handoff_test_log}"
2paseo
expect-log "--agent claude --id ${handoff_test_id}"
expect-log "--provider-home ${handoff_test_tmp}/work-profile"
expect-log "--source-pid 42 --caller-pid "
expect-log "job 'paseo-${handoff_test_id}' 'command' 'python3'"
expect-log "tmux switch-client -t =paseo-${handoff_test_id}"
: > "${handoff_test_log}"
handoff_test_owner=codex
2paseo --wait-exit --no-switch
expect-log '--wait-exit'
expect-log "--provider-home ${handoff_test_tmp}/codex-home"
[[ "$(<"${handoff_test_log}")" != *switch-client* ]]
: > "${handoff_test_log}"
PASEO_AGENT_ID="${handoff_test_id}" 2paseo --no-switch
expect-log "'attach' '${handoff_test_id}'"
[[ "$(<"${handoff_test_log}")" != *prepare* ]]
: > "${handoff_test_log}"
function tmux-job-running-p { return 0; }
PASEO_AGENT_ID="${handoff_test_id}" 2paseo
expect-log "tmux switch-client -t =paseo-${handoff_test_id}"
[[ "$(<"${handoff_test_log}")" != *job* ]]
[[ "$(<"${handoff_test_log}")" != *prepare* ]]
: > "${handoff_test_log}"
if PASEO_AGENT_ID=bad 2paseo ; then exit 1; fi
if 2paseo --bad ; then exit 1; fi
handoff_test_live=''
if 2paseo ; then exit 1; fi
[[ ! -s "${handoff_test_log}" ]]
handoff_test_live=$'42\tid\tname\tcwd\t'"${handoff_test_source}"$'\t-\tworking\n43\tid\tname\tcwd\t'"${handoff_test_source}"
if 2paseo ; then exit 1; fi
[[ ! -s "${handoff_test_log}" ]]
#: --to: parsing and ~/ expansion here; paseo_handoff.py checks the directory itself.
handoff_test_live=$'42\t'"${handoff_test_id}"$'\tsource\tcwd\t'"${handoff_test_source}"$'\ttmux\tworking\tinteractive'
handoff_test_owner=claude
command mkdir -p -- "${handoff_test_tmp}/home/.claude/projects"
typeset -g handoff_test_personal="${handoff_test_tmp:A}/home/.claude"
: > "${handoff_test_log}"
2paseo --to "${handoff_test_tmp}/home/./.claude"
expect-log "--provider-home ${handoff_test_tmp}/work-profile"
expect-log "--target-home ${handoff_test_personal}"
expect-log "tmux switch-client -t =paseo-${handoff_test_id}"
: > "${handoff_test_log}"
HOME="${handoff_test_tmp}/home" 2paseo --wait-exit '--to=~/.claude' --no-switch
expect-log '--wait-exit'
expect-log "--target-home ${handoff_test_personal}"
[[ "$(<"${handoff_test_log}")" != *switch-client* ]]
: > "${handoff_test_log}"
HOME="${handoff_test_tmp}/home" 2paseo --no-switch --to '~/.claude'
expect-log "--target-home ${handoff_test_personal}"
: > "${handoff_test_log}"
if 2paseo --to ; then exit 1; fi
if 2paseo --to '' ; then exit 1; fi
if 2paseo --to relative/.claude ; then exit 1; fi
if 2paseo --to '~other/.claude' ; then exit 1; fi
handoff_test_owner=codex
if 2paseo --to "${handoff_test_personal}" ; then exit 1; fi
handoff_test_owner=claude
if PASEO_AGENT_ID="${handoff_test_id}" 2paseo --to "${handoff_test_personal}" ; then exit 1; fi
[[ ! -s "${handoff_test_log}" ]]
print 'ok: Paseo handoff orchestration'
