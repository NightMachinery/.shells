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
typeset -gx AGY_ACCOUNTS='acc1 acc2 acc3'
typeset -gx AGY_ACCOUNT_DIR_ACC1="${test_tmp}/shared"
typeset -gx AGY_SHARED_GEMINI_DIR="${AGY_ACCOUNT_DIR_ACC1}"
typeset -gx AGY_ACCOUNTS_DIR="${test_tmp}/accounts"
typeset -gx AGY_ACCOUNT_NAME_ACC2=cave
typeset -gx agy_status_cache_file="${test_tmp}/first-cache.json"
typeset -gx AGY_TEST_CALLS="${test_tmp}/calls.json"
command mkdir -p "${test_tmp}/bin" "${AGY_SHARED_GEMINI_DIR}/config" "${AGY_SHARED_GEMINI_DIR}/antigravity-cli"
print -r -- 'shared rules' > "${AGY_SHARED_GEMINI_DIR}/GEMINI.md"
print -r -- '{}' > "${AGY_SHARED_GEMINI_DIR}/config/hooks.json"
print -r -- '{"statusLine":{"command":"fixture-statusline"},"privateSetting":"not shared"}' > "${AGY_SHARED_GEMINI_DIR}/antigravity-cli/settings.json"
command cat > "${test_tmp}/bin/agy" <<'PY'
#!/usr/bin/env python3
import json, os, sys
json.dump({"argv":sys.argv[1:], "root":os.environ.get("ANTIGRAVITY_GEMINI_DIR"),
           "ssh":os.environ.get("SSH_CONNECTION"), "home":os.environ["HOME"],
           "account":os.environ.get("AGY_ACCOUNT_ID"),
           "cache":os.environ.get("agy_status_cache_file")},open(os.environ["AGY_TEST_CALLS"],"w"))
if sys.argv[-1] == "models":
    print("gemini-fixture\tGemini fixture\nclaude-fixture\tClaude fixture")
if "--print" in sys.argv:
    print("Gemini Models\tWeekly Limit Remaining\t93%\t2030-01-01T00:00:00Z")
PY
command chmod +x "${test_tmp}/bin/agy"
typeset -gx PATH="${test_tmp}/bin:${PATH}"
source "${test_root}/zshlang/auto-load/others/antigravity.zsh"
source "${test_root}/zshlang/auto-load/others/agy-status.zsh"
function h-agent-launch { shift; command "$@" }

function fail { print -ru2 -- "$*"; exit 1 }
(( ${+functions[agy-cave]} && ${+functions[agy-cave-t]} && ${+functions[agy-acc3]} )) || fail 'generated names'
typeset -g original_home="${HOME}" original_ssh="${SSH_CONNECTION-}"
agy-cave models
command jq --exit-status --arg root "${AGY_ACCOUNTS_DIR}/acc2" --arg home "${HOME}" '
 .root == $root and .home == $home and .account == "acc2" and (.ssh | length > 0)
 and (.argv | index("--gemini_dir="+$root) != null)
' "${AGY_TEST_CALLS}" >/dev/null
[[ "${HOME}" == "${original_home}" && "${SSH_CONNECTION-}" == "${original_ssh}" ]] || fail 'process env leaked'
[[ -L "${AGY_ACCOUNTS_DIR}/acc2/GEMINI.md" ]] || fail 'shared rules missing'
command jq --exit-status 'has("statusLine") and (has("privateSetting") | not)' \
 "${AGY_ACCOUNTS_DIR}/acc2/antigravity-cli/settings.json" >/dev/null
agy-account-prepare 2
if agy-cave --gemini_dir=/private/tmp/unintended 2>/dev/null; then fail 'state override accepted'; fi
if AGY_ACCOUNT_DIR_ACC2="${AGY_ACCOUNT_DIR_ACC1}" h-agy-accounts 2>/dev/null; then fail 'shared auth root accepted'; fi
if AGY_ACCOUNT_NAME_ACC2=status h-agy-accounts 2>/dev/null; then fail 'reserved launcher accepted'; fi
if AGY_ACCOUNT_NAME_ACC2=acc3 h-agy-accounts 2>/dev/null; then fail 'account alias hijack accepted'; fi
typeset -ga reply
AGY_ACCOUNT_ID=acc2 agy_status_cache_file="${test_tmp}/wrong-cache.json" h-agy-account-info acc1
[[ "${reply[5]}" != "${test_tmp}/wrong-cache.json" ]] || fail 'main inherited another cache'

print -r -- 'inert fixture' > "${AGY_SHARED_GEMINI_DIR}/antigravity-cli/antigravity-oauth-token"
print -r -- 'inert fixture' > "${AGY_ACCOUNTS_DIR}/acc2/antigravity-cli/antigravity-oauth-token"
typeset -g now="$(command date +%s)"
command jq --null-input --argjson now "${now}" '{account:{tier:"fixture"}, buckets:{"gemini-weekly":{remaining_percent:45,resets_at:2000000000,captured_at:$now,source:"fixture"},"3p-weekly":{remaining_percent:50,resets_at:2000000000,captured_at:$now,source:"fixture"}}}' > "${agy_status_cache_file}"
command jq --null-input --argjson now "${now}" '{buckets:{"gemini-weekly":{remaining_percent:88,resets_at:2000000000,captured_at:$now,source:"fixture"},"gemini-5h":{remaining_percent:72,resets_at:2000000000,captured_at:$now,source:"fixture"},"3p-weekly":{remaining_percent:6,resets_at:2000000000,captured_at:$now,source:"fixture"}}}' > "${AGY_ACCOUNTS_DIR}/acc2/antigravity-cli/quota-cache.json"
typeset -g json='' rc=0
json="$(agy-status --json)" || rc=$?
(( rc == 0 )) || fail 'signed-out account is a status, not measurement failure'
print -r -- "${json}" | command jq --exit-status '
 length == 3 and .[0].buckets[0].used_percent == 55
 and all(.[]; keys == ["account","auth_status","buckets","email","error","label","message","signed_in","tier"])
 and all(.[].buckets[]; keys == ["bucket_id","group","label","remaining_percent","resets_at","resets_at_iso","source","used_percent"])
 and .[1].label == "cave" and .[1].buckets[0].used_percent != 55
 and .[2].signed_in == false and .[2].error == null and .[2].message != null
' >/dev/null
json="$(AGY_ACCOUNT_ID=acc1 agy_status_json_p=y agy-status)"
print -r -- "${json}" | command jq --exit-status '.[0].remaining_percent == 45 and .[0].source == "cache"' >/dev/null

# The human report consumes the same data without floating source diagnostics.
agy_status_columns=80 agy-status > "${test_tmp}/human.txt" 2> "${test_tmp}/human.err"
[[ ! -s "${test_tmp}/human.err" ]] || fail 'human diagnostics escaped account blocks'
command rg --quiet 'resets in|reset .* ago' "${test_tmp}/human.txt" || fail 'relative resets missing'
print -r -- "${json}" | command jq --exit-status 'all(.[]; has("_display_cache_file") | not)' >/dev/null

# Best account considers the minimum known window; only Gemini prefers acc2.
agy-best --model 'Gemini fixture' models
command jq --exit-status '.account == "acc2"' "${AGY_TEST_CALLS}" >/dev/null
agy-best --model 'Claude fixture' models
command jq --exit-status '.account == "acc1"' "${AGY_TEST_CALLS}" >/dev/null

# A live fallback must remain valid JSON and use the selected account/env.
command rm "${AGY_ACCOUNTS_DIR}/acc2/antigravity-cli/quota-cache.json"
json="$(agy-status --account cave --json)"
print -r -- "${json}" | command jq --exit-status '.[0].signed_in == true and .[0].buckets[0].used_percent == 7' >/dev/null
command jq --exit-status '.account == "acc2" and (.argv | index("--mode") != null)' "${AGY_TEST_CALLS}" >/dev/null
command rm "${AGY_ACCOUNTS_DIR}/acc2/antigravity-cli/quota-cache.json" "${AGY_TEST_CALLS}"
if agy_status_cache_only_p=y agy-status --account cave --json > "${test_tmp}/cache-only.json"; then fail 'missing cache must fail'; fi
[[ ! -e "${AGY_TEST_CALLS}" ]] || fail 'cache-only invoked agy'
command jq --exit-status '.[0].error != null' "${test_tmp}/cache-only.json" >/dev/null

source "${test_root}/zshlang/plugins/agent-session/session.zsh"
source "${test_root}/zshlang/auto-load/others/agy-session.zsh"
source "${test_root}/zshlang/auto-load/others/agent-tmux.zsh"
typeset -g conversation='12345678-1234-1234-1234-123456789abc'
typeset -g transcript="${AGY_SHARED_GEMINI_DIR}/antigravity-cli/brain/${conversation}/.system_generated/logs/transcript_full.jsonl"
command mkdir -p "${transcript:h}" "${AGY_ACCOUNTS_DIR}/history-owners"
print -r -- '{}' > "${transcript}"
command ln -s "${AGY_SHARED_GEMINI_DIR}/antigravity-cli/brain" "${AGY_ACCOUNTS_DIR}/acc2/antigravity-cli/brain"
print -r -- acc2 > "${AGY_ACCOUNTS_DIR}/history-owners/${conversation}"
[[ "$(h-agy-account-of-transcript "${transcript}")" == acc2 ]] || fail 'shared history lost owner'
typeset -ga agy_test_resume=()
function h-agent-session-resume-run { shift; agy_test_resume=( "$@" ) }
h-agy-session-resume "${transcript}"
[[ "${agy_test_resume[1]}" == agy-acc2 && "${agy_test_resume[2]}" == --conversation &&
   "${agy_test_resume[3]}" == "${conversation}" ]] || fail 'owning account resume argv'
function agy-conversation-name { print -r -- fixture }
[[ "$(AGY_ACCOUNT_ID=acc2 AGY_ACCOUNT_LABEL=cave h-agy-session-tmux-name "${conversation}" "${transcript}")" == '+Agy/cave fixture' ]] || fail 'live account identity lost'

# Exercise the hook itself: a retained duplicate silently skipped the runner.
function h-agy-test-hook-probe {
    [[ "${AGY_ACCOUNT_ID}" == acc2 && "${AGY_ACCOUNT_LABEL}" == cave &&
       "${ANTIGRAVITY_GEMINI_DIR}" == "${AGY_ACCOUNTS_DIR}/acc2" ]] || return 1
    h-agy-account-info acc2 || return
    print -r -- 'hook-ran'
}
[[ "$(agy-account-hook acc2 "${AGY_ACCOUNTS_DIR}/acc2" cave h-agy-test-hook-probe)" == hook-ran ]] || fail 'account hook silently skipped runner'
[[ "${AGY_ACCOUNTS}" == 'acc1 acc2 acc3' && -z "${AGY_ACCOUNT_ID:-}" ]] || fail 'hook environment leaked'

# Login recovery must inspect every window and never replace a live pane.
command cat > "${test_tmp}/bin/tmux" <<'PY'
#!/usr/bin/env python3
import json, os, sys
args = sys.argv[1:]
with open(os.environ["AGY_TEST_TMUX_LOG"], "a") as f:
    f.write(json.dumps(args) + "\n")
case = os.environ["AGY_TEST_TMUX_CASE"]
if args[0] == "has-session":
    sys.exit(1 if case == "missing" else 0)
if args[0] == "list-panes":
    assert "-s" in args, "must inspect every window"
    if case == "query-failed":
        sys.exit(1)
    print({"dead": "%101 1\n%102 1", "live": "%101 0",
           "mixed": "%101 1\n%102 0", "invalid": "%101 unknown",
           "empty": "", "respawn-failed": "%101 1"}[case])
if args[0] == "respawn-pane":
    assert "-k" not in args, "must not kill a concurrently revived pane"
    sys.exit(1 if case == "respawn-failed" else 0)
PY
command chmod +x "${test_tmp}/bin/tmux"
rehash
typeset -gx AGY_TEST_TMUX_LOG="${test_tmp}/tmux.jsonl" AGY_TEST_TMUX_CASE=''
typeset -g login_output='' login_rc=0
for AGY_TEST_TMUX_CASE in missing dead live mixed query-failed invalid empty respawn-failed ; do
    : > "${AGY_TEST_TMUX_LOG}"
    login_rc=0
    login_output="$(agy-acc-login acc2 2>"${test_tmp}/login.err")" || login_rc=$?
    case "${AGY_TEST_TMUX_CASE}" in
        missing|dead)
            (( login_rc == 0 )) || fail "login ${AGY_TEST_TMUX_CASE} failed: $(<"${test_tmp}/login.err")"
            [[ "${login_output}" == Attach:* ]] || fail 'missing fresh attach hint'
            command jq --slurp --exit-status --arg kind "${AGY_TEST_TMUX_CASE}" '
                map(select(.[0] == "new-session" or .[0] == "respawn-pane")) |
                length == 1 and
                (.[0] | if $kind == "dead" then
                    .[0:3] == ["respawn-pane", "-t", "%101"]
                 else .[0:4] == ["new-session", "-d", "-s", "agy-login-acc2"] end)
            ' "${AGY_TEST_TMUX_LOG}" >/dev/null ;;
        live|mixed)
            (( login_rc == 0 )) || fail 'live login failed'
            [[ "${login_output}" == 'Sign-in already open.'* ]] || fail 'live login lost attach hint' ;;
        *) (( login_rc != 0 )) || fail "login ${AGY_TEST_TMUX_CASE} silently succeeded" ;;
    esac
    if [[ "${AGY_TEST_TMUX_CASE}" == (live|mixed|query-failed|invalid|empty) ]] ; then
        command jq --slurp --exit-status 'all(.[]; .[0] == "has-session" or .[0] == "list-panes")' \
            "${AGY_TEST_TMUX_LOG}" >/dev/null
    fi
done

print -r -- 'ok: launch isolation, named accounts, configuration, status JSON, best, cache-only and login recovery'
