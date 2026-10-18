#!/usr/bin/env zsh
# Run with zsh -f; no account credentials, network or owner's state is used.
setopt err_exit pipe_fail extended_glob
typeset -g test_root="${0:A:h:h:h}"
typeset -g test_tmp="$(command mktemp -d /private/tmp/agy-accounts-tests.XXXXXX)"
trap 'command rm -rf -- "${test_tmp}"' EXIT
source "${test_root}/zshlang/basic/core.zsh"
function ecerr { print -ru2 -- "$@" }
function isdbg { return 1 }
source "${test_root}/zshlang/basic/return-aliases.zsh"
source "${test_root}/zshlang/basic/compatibility.zsh"
source "${test_root}/zshlang/basic/enhancers.zsh"
source "${test_root}/zshlang/basic/macros.zsh"
source "${test_root}/zshlang/basic/assert.zsh"
source "${test_root}/zshlang/basic/eval.zsh"
function assert-args { return 0 }
function isDeus { return 1 }
function h-color-mode-p { [[ "${1}" == always ]] }
function date-unix-to-3339 { command gdate --date="@${1}" --iso-8601=seconds }
function silent { "$@" >/dev/null 2>&1 }
typeset -gx NIGHTDIR="${test_root}"
source "${test_root}/zshlang/auto-load/others/agent-status.zsh"
function fail { print -ru2 -- "$*"; exit 1 }
function bella_zsh_disable1 { return 0 }
function ensure-array { return 0 }
function claude-code-usage-all {
    [[ "$*" == --json ]] || fail 'Claude JSON transport changed'
    print -ru2 -- 'provider diagnostic must remain inside its section'
    print -r -- '[{"profile":"default","plan":"max","windows":[{"key":"session","utilization_percent":52,"resets_at":2000000000}]},{"profile":"work","error":"Login required"}]'
    return 1
}
function codex-status {
    [[ "$*" == --json ]] || fail 'Codex JSON transport changed'
    print -r -- 'malformed response'
    return 2
}
function agy-status {
    [[ "$*" == --json ]] || fail 'agy JSON transport changed'
    print -r -- '[{"account":"m","label":"m","signed_in":null,"buckets":[{"bucket_id":"gemini-weekly","source":"cache","remaining_percent":75,"resets_at":2000000000}]}]'
}
function h-agy-account-info { reply=( m m /fixture /fixture "${test_tmp}/cache.json" ) }
print -r -- '{"buckets":{"gemini-weekly":{"captured_at":2000000000}}}' > "${test_tmp}/cache.json"
function parallelm {
    [[ "$1" == --keep-order ]] || fail 'provider ordering lost'
    shift
    local worker="$1"; shift
    [[ "$1" == '{}' && "$2" == ::: ]] || fail 'unexpected fanout arguments'
    shift 2
    local provider
    for provider in "$@" ; do
        "${worker}" "${provider}"
    done
}
function pager-if-overflow-streaming { command cat }
typeset -ga agent_status_agents=( claude codex agy unknown )
agent_status_color=always agent_status_columns=80 agent-status > "${test_tmp}/output" 2> "${test_tmp}/errors"
[[ ! -s "${test_tmp}/errors" ]] || fail 'diagnostics escaped provider section'
command python3 - "${test_tmp}/output" <<'PYTEST'
from pathlib import Path
import sys
sys.path.insert(0, str(Path.cwd() / 'python'))
from agy_status_render import width
text = Path(sys.argv[1]).read_text()
assert text.index('Claude Code') < text.index('Codex') < text.index('Antigravity') < text.index('unknown')
assert 'Login required' in text and 'Status lookup failed (exit 2)' in text and 'Unknown provider' in text
assert 'Gemini' in text and 'cache <1m' in text
assert all(width(line) <= 80 for line in text.splitlines())
assert '\x1b' not in text
PYTEST
h-agent-status-collect agy | command jq --exit-status --arg file "${test_tmp}/cache.json" '.agent == "agy" and .cache_files.m == $file and .report[0].buckets[0].remaining_percent == 75' >/dev/null
print -r -- 'ok: provider JSON fanout, stable order, partial failures, cache context and local compact rendering'
