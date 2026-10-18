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
typeset -gx agy_status_cache_file="${test_tmp}/quota.json"
source "${test_root}/zshlang/auto-load/others/agy-status.zsh"
function fail { print -ru2 -- "$*"; exit 1 }
function h-agy-status-run {
    print -r -- $'Gemini Models\tWeekly Limit Remaining\t91%\t2030-01-01T00:00:00Z'
    print -r -- $'Gemini 4 Argon\tGemini 4 Argon Weekly Limit\t17%\t2030-01-01T00:00:00Z'
    print -r -- $'Gemini 4 Argon\tGemini 4 Argon 5 Hour Limit\t31%\t2030-01-01T00:00:00Z'
    print -r -- $'Future Model\tWeekly Limit\t42%\t2030-01-01T00:00:00Z'
}
typeset -g rows="$(h-agy-status-rows)"
print -r -- "${rows}" | h-agy-status-json > "${test_tmp}/buckets.json"
command jq --exit-status '
 length == 4 and ([.[].bucket_id] | unique | length) == 4
 and any(.[]; .bucket_id == "gemini-weekly" and .remaining_percent == 91)
 and any(.[]; .bucket_id == "gemini-4-argon-weekly" and .remaining_percent == 17)
 and any(.[]; .bucket_id == "gemini-4-argon-5h" and .remaining_percent == 31)
 and any(.[]; .bucket_id == "future-model-weekly")
' "${test_tmp}/buckets.json" >/dev/null
h-agy-status-cache-merge slow "${rows}"
typeset -g raw="$(h-agy-status-statusline-raw)"
[[ "${raw}" == *$'gemini-4-argon-weekly\t17'* ]] || fail 'model-specific cache key lost'
[[ "${raw}" == *$'Gemini 4 Argon\tGemini 4 Argon Weekly Limit'* ]] || fail 'cached model label lost'
# Existing native Argon aliases must canonicalize and deduplicate by capture
# time, including before either writer has refreshed the cache.
command jq '.buckets["gemini-argon-wtu-weekly"] = (.buckets["gemini-4-argon-weekly"] + {captured_at:1,remaining_percent:99})' "${agy_status_cache_file}" > "${test_tmp}/alias.json"
command mv "${test_tmp}/alias.json" "${agy_status_cache_file}"
raw="$(h-agy-status-statusline-raw)"
[[ "$(print -r -- "${raw}" | command rg --count '^gemini-4-argon-weekly')" == 1 ]] || fail 'native alias duplicated pool'
[[ "${raw}" != *gemini-argon-wtu-weekly* ]] || fail 'native alias escaped JSON reader'
# The statusline writer must preserve arbitrary future native keys, without
# overwriting existing slow-path labels or unrelated pools.
print -r -- '{"quota":{"gemini-weekly":{"remaining_fraction":0.8,"reset_in_seconds":60},"gemini-argon-5h":{"remaining_fraction":0.3,"reset_in_seconds":120},"future-model-5h":{"remaining_fraction":0.5,"reset_in_seconds":180}}}' | command dash "${test_root}/zshlang/wrappers/agy_statusline.dash" >/dev/null
command jq --exit-status '
 (.buckets | length) == 5
 and (.buckets | has("gemini-argon-wtu-weekly") | not)
 and (.buckets | has("gemini-argon-5h") | not)
 and .buckets["gemini-weekly"].remaining_percent == 80
 and .buckets["gemini-4-argon-weekly"].remaining_percent == 17
 and .buckets["gemini-4-argon-5h"].remaining_percent == 30
 and .buckets["future-model-5h"].remaining_percent == 50
' "${agy_status_cache_file}" >/dev/null
print -r -- 'ok: distinct general/model/window IDs, cache roundtrip and arbitrary statusline pools'
