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
#: Launchers keep a real SSH session's SSH_CONNECTION; run over ssh, the file
#: authentication checks below would see that instead of agy-file-auth.
unset SSH_CONNECTION
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
           "cache":os.environ.get("agy_status_cache_file"),
           "env":{k:os.environ.get(k) for k in ("GEMINI_API_KEY", "GOOGLE_API_KEY", "AGY_ADC_AUTH",
               "GOOGLE_CLOUD_PROJECT", "GOOGLE_CLOUD_LOCATION", "GOOGLE_APPLICATION_CREDENTIALS")}},
          open(os.environ["AGY_TEST_CALLS"],"w"))
if sys.argv[-1] == "models":
    print("gemini-fixture\tGemini fixture\nclaude-fixture\tClaude fixture")
    sys.exit(int(os.environ.get("AGY_TEST_MODELS_EXIT", "0")))
if sys.argv[-2:] == ["--mode", "plan"]:
    sys.exit(int(os.environ.get("AGY_TEST_LOGIN_EXIT", "0")))
if "--print" in sys.argv:
    print("Gemini Models\tWeekly Limit Remaining\t93%\t2030-01-01T00:00:00Z")
PY
command chmod +x "${test_tmp}/bin/agy"
#: Stand-ins for the browser sign-in and the Vertex AI API check.
typeset -gx AGY_API_GCLOUD_DIR="${test_tmp}/gcloud-agy-api" AGY_TEST_GCLOUD_CALLS="${test_tmp}/gcloud-calls"
command cat > "${test_tmp}/bin/gcloud" <<'SH'
#!/bin/sh
printf '%s|%s|%s\n' "${CLOUDSDK_CONFIG}" "${CLOUDSDK_CORE_PROJECT}" "$*" >> "${AGY_TEST_GCLOUD_CALLS}"
case "$*" in
    'auth application-default login') printf '{"type":"authorized_user"}\n' > "${CLOUDSDK_CONFIG}/application_default_credentials.json" ;;
    'auth application-default print-access-token') echo fixture-token ;;
esac
SH
command cat > "${test_tmp}/bin/curl" <<'SH'
#!/bin/sh
printf '%s\n' "$*" >> "${AGY_TEST_GCLOUD_CALLS}.curl-argv"
cat > "${AGY_TEST_GCLOUD_CALLS}.curl-stdin"
case "$*" in
    *cloudresourcemanager*pageToken=p2*) echo '{"projects":[{"projectId":"fixture-picked","name":"Picked"}]}' ;;
    *cloudresourcemanager*) echo '{"projects":[{"projectId":"fixture-other","name":"Other"}],"nextPageToken":"p2"}' ;;
    *) echo "{\"state\":\"${AGY_TEST_VERTEX_STATE:-ENABLED}\"}" ;;
esac
SH
command chmod +x "${test_tmp}/bin/gcloud" "${test_tmp}/bin/curl"
typeset -gx PATH="${test_tmp}/bin:${PATH}"
source "${test_root}/zshlang/auto-load/others/antigravity.zsh"
source "${test_root}/zshlang/auto-load/others/agent-quotas.zsh"
source "${test_root}/zshlang/auto-load/others/agy-status.zsh"
#: The zsh engine is what this file tests; the agent-quotas route has its own block.
typeset -g agent_quotas_p=n
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

# With the agent-quotas daemon answering, the account report comes from it;
# the flat bucket array and cache-only mode stay with the zsh engine.
command cat > "${test_tmp}/bin/agent-quotas" <<'SH'
#!/bin/sh
[ "$1" = health ] && exit 0
printf 'AQ %s\n' "$*"
SH
command chmod +x "${test_tmp}/bin/agent-quotas"
#: The directory was hashed before the stub existed.
rehash
agent_quotas_p=auto
[[ "$(agy_status_columns=90 agy-status --account cave --json)" == 'AQ agy-status --columns 90 --color auto --absolute auto --account cave --json' ]] || fail 'agent-quotas route'
[[ "$(agy_status_method=slow agy_status_columns=90 agy-status)" == 'AQ agy-status --columns 90 --color auto --absolute auto --refresh' ]] || fail 'slow method lost on the agent-quotas route'
AGY_ACCOUNT_ID=acc1 agy_status_json_p=y agy-status | command jq --exit-status '.[0].source == "cache"' >/dev/null || fail 'flat array left the zsh engine'
[[ "$(agy-status --bogus 2>&1)" != AQ* ]] || fail 'unknown flag reached agent-quotas'
agent_quotas_p=n

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
: > "${AGY_TEST_TMUX_LOG}"
login_output="$(agy-acc-login cave </dev/null)"
[[ "${login_output}" == *'Account verification exited 0.'* && "${login_output}" != *'Press Enter'* ]] || fail 'foreground login waits for pane close'
[[ ! -s "${AGY_TEST_TMUX_LOG}" ]] || fail 'foreground login invoked tmux'
command jq --exit-status '.account == "acc2" and .argv[-1] == "models"' "${AGY_TEST_CALLS}" >/dev/null
login_rc=0
AGY_TEST_MODELS_EXIT=7 agy-acc-login acc2 >/dev/null || login_rc=$?
(( login_rc == 7 )) || fail 'foreground verification lost exit status'
AGY_ACCOUNT_ID=acc2 agy-acc-login >/dev/null
command jq --exit-status '.account == "acc2"' "${AGY_TEST_CALLS}" >/dev/null
command rm -- "${AGY_ACCOUNTS_DIR}/acc2/antigravity-cli/antigravity-oauth-token"
login_rc=0
AGY_TEST_LOGIN_EXIT=9 agy-acc-login acc2 >/dev/null || login_rc=$?
(( login_rc == 9 )) || fail 'foreground sign-in lost exit status'
command jq --exit-status '.argv[-2:] == ["--mode", "plan"]' "${AGY_TEST_CALLS}" >/dev/null
for bad_args in '--invalid' 'acc1 acc2' ; do
    login_rc=0
    agy-acc-login ${=bad_args} >/dev/null 2>&1 || login_rc=$?
    (( login_rc == 2 )) || fail 'invalid login arguments accepted'
done
agy-acc-login --help >/dev/null
[[ ! -s "${AGY_TEST_TMUX_LOG}" ]] || fail 'foreground options invoked tmux'
for AGY_TEST_TMUX_CASE in missing dead live mixed query-failed invalid empty respawn-failed ; do
    : > "${AGY_TEST_TMUX_LOG}"
    login_rc=0
    # Exercise both spellings and both positions relative to the account.
    if [[ "${AGY_TEST_TMUX_CASE}" == dead ]] ; then
        login_output="$(agy-acc-login acc2 -t 2>"${test_tmp}/login.err")" || login_rc=$?
    else
        login_output="$(agy-acc-login --tmux acc2 2>"${test_tmp}/login.err")" || login_rc=$?
    fi
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

# API-billed profiles: credentials reach agy only through its environment.
(( ${+functions[agy-api-personal]} && ${+functions[agy-api-work-t]} )) || fail 'generated API launchers'
typeset -g api_rc=0
typeset -gx GOOGLE_API_KEY=ambient-google-key   # agy would prefer it to the profile key
agy-api personal models 2>/dev/null || api_rc=$?
(( api_rc == 2 )) || fail 'API profile without credentials launched'
AGY_API_KEY_PERSONAL=fixture-key agy-api personal models
command jq --exit-status --arg root "${AGY_ACCOUNTS_DIR}/api_personal" '
    .account == "api_personal" and .root == $root and .ssh == "agy-file-auth" and
    .env.GEMINI_API_KEY == "fixture-key" and .env.GOOGLE_API_KEY == null and
    .env.AGY_ADC_AUTH == null and (.argv | index("fixture-key") | not)' \
    "${AGY_TEST_CALLS}" >/dev/null || fail 'API key route'
command jq --exit-status '.modelProvider == "gemini" and .statusLine.command == "fixture-statusline"' \
    "${AGY_ACCOUNTS_DIR}/api_personal/antigravity-cli/settings.json" >/dev/null || fail 'API key settings'
[[ -z "${GEMINI_API_KEY-}" ]] || fail 'API key leaked into the caller'
print -r -- '{}' > "${test_tmp}/adc.json"
AGY_API_PROJECT_WORK=fixture-project AGY_API_CREDENTIALS_WORK="${test_tmp}/adc.json" agy-api work models
command jq --exit-status --arg adc "${test_tmp:A}/adc.json" '
    .account == "api_work" and .env.AGY_ADC_AUTH == "1" and
    .env.GOOGLE_CLOUD_PROJECT == "fixture-project" and .env.GOOGLE_CLOUD_LOCATION == "global" and
    .env.GOOGLE_APPLICATION_CREDENTIALS == $adc and
    .env.GEMINI_API_KEY == null and .env.GOOGLE_API_KEY == null' \
    "${AGY_TEST_CALLS}" >/dev/null || fail 'Vertex ADC route'
#: A profile moved from a key to Vertex loses its modelProvider.
AGY_API_PROJECT_PERSONAL=fixture-project AGY_API_LOCATION_PERSONAL=europe-west4 agy-api personal models
command jq --exit-status '.env.GOOGLE_CLOUD_LOCATION == "europe-west4"' "${AGY_TEST_CALLS}" >/dev/null || fail 'Vertex location'
command jq --exit-status '(has("modelProvider") | not) and .statusLine.command == "fixture-statusline"' \
    "${AGY_ACCOUNTS_DIR}/api_personal/antigravity-cli/settings.json" >/dev/null || fail 'Vertex settings kept modelProvider'
for api_case in both missing-file bad-name ; do
    api_rc=0
    case "${api_case}" in
        both) AGY_API_KEY_WORK=k AGY_API_PROJECT_WORK=p agy-api work models 2>/dev/null || api_rc=$? ;;
        missing-file) AGY_API_PROJECT_WORK=p AGY_API_CREDENTIALS_WORK="${test_tmp}/none.json" agy-api work models 2>/dev/null || api_rc=$? ;;
        bad-name) AGY_API_KEY_WORK=k agy-api Work models 2>/dev/null || api_rc=$? ;;
    esac
    (( api_rc == 2 )) || fail "API profile ${api_case} launched"
done
unset GOOGLE_API_KEY
[[ "$(h-agy-account-launcher api_work)" == agy-api-work && "$(h-agy-account-launcher acc2)" == agy-acc2 ]] || fail 'resume launcher'
h-agy-account-info api_work
[[ "${reply[2]}" == api-work && "${reply[3]}" == "${AGY_ACCOUNTS_DIR}/api_work" ]] || fail 'API profile identity'
api_rc=0
AGY_ACCOUNTS='acc1 api_x' h-agy-accounts >/dev/null 2>&1 || api_rc=$?
(( api_rc == 2 )) || fail 'api_ account name accepted'

# Vertex AI through a browser sign-in: agy-api-NAME-login keeps the
# credentials and project in the profile's own gcloud directory.
(( ${+functions[agy-api-work-login]} )) || fail 'generated login command'
for api_case in bad-project key-set ; do
    api_rc=0
    case "${api_case}" in
        bad-project) agy-api-work-login 'Not A Project' 2>/dev/null || api_rc=$? ;;
        key-set) AGY_API_KEY_WORK=k agy-api-work-login fixture-login-project 2>/dev/null || api_rc=$? ;;
    esac
    (( api_rc == 2 )) || fail "login ${api_case} went ahead"
done
[[ ! -e "${AGY_TEST_GCLOUD_CALLS}" ]] || fail 'refused login still ran gcloud'
#: With no project given or saved, the signed-in account's projects (every
#: page) go to fzf; the stand-in picks the one on the second page.
function fz { command grep -F fixture-picked }
agy-api-personal-login >/dev/null
[[ "$(<"${AGY_API_GCLOUD_DIR}/personal/project")" == fixture-picked ]] || fail 'picked project not stored'
[[ "$(command head -n 1 "${AGY_TEST_GCLOUD_CALLS}")" == "${AGY_API_GCLOUD_DIR}/personal||auth application-default login" ]] ||
    fail 'login without a project set one'
command grep -q -x -F "${AGY_API_GCLOUD_DIR}/personal||auth application-default set-quota-project fixture-picked" "${AGY_TEST_GCLOUD_CALLS}" ||
    fail 'picked project not made the quota project'
function fz { return 130 }
api_rc=0
agy-api-login spare 2>/dev/null >&2 || api_rc=$?
(( api_rc == 2 )) && [[ ! -e "${AGY_API_GCLOUD_DIR}/spare/project" ]] || fail 'cancelled picker stored a project'
unfunction fz
command rm -f -- "${AGY_TEST_GCLOUD_CALLS}"
agy-api-work-login fixture-login-project >/dev/null
typeset -g api_gcloud="${AGY_API_GCLOUD_DIR}/work"
[[ "$(command head -n 1 "${AGY_TEST_GCLOUD_CALLS}")" == "${api_gcloud}|fixture-login-project|auth application-default login" ]] ||
    fail 'login did not use the profile gcloud directory and project'
[[ "$(<"${api_gcloud}/project")" == fixture-login-project ]] || fail 'login project not stored'
[[ "$(<"${AGY_TEST_GCLOUD_CALLS}.curl-argv")" != *fixture-token* &&
   "$(<"${AGY_TEST_GCLOUD_CALLS}.curl-stdin")" == 'Authorization: Bearer fixture-token' ]] ||
    fail 'access token on the curl command line'
agy-api work models
command jq --exit-status --arg adc "${api_gcloud}/application_default_credentials.json" '
    .account == "api_work" and .env.AGY_ADC_AUTH == "1" and
    .env.GOOGLE_CLOUD_PROJECT == "fixture-login-project" and
    .env.GOOGLE_APPLICATION_CREDENTIALS == $adc' "${AGY_TEST_CALLS}" >/dev/null || fail 'login-based Vertex route'
AGY_API_PROJECT_WORK=override-project agy-api work models
command jq --exit-status '.env.GOOGLE_CLOUD_PROJECT == "override-project"' "${AGY_TEST_CALLS}" >/dev/null ||
    fail 'project variable did not win over the login'
api_rc=0
AGY_API_KEY_WORK=k agy-api work models 2>/dev/null || api_rc=$?
(( api_rc == 2 )) || fail 'key and login together launched'
api_rc=0
AGY_TEST_VERTEX_STATE=DISABLED agy-api-work-login 2>/dev/null >&2 || api_rc=$?
(( api_rc == 1 )) || fail 'disabled Vertex AI API not reported'
[[ "$(command tail -n 2 "${AGY_TEST_GCLOUD_CALLS}" | command head -n 1)" == "${api_gcloud}|fixture-login-project|auth application-default login" ]] ||
    fail 'second login did not reuse the stored project'

# -light: the same credentials and shared history, without the global
# instructions (config/rules, GEMINI.md, AGENTS.md).
(( ${+functions[agy-api-work-light]} && ${+functions[agy-api-work-light-t]} )) || fail 'generated light launchers'
command mkdir -p "${AGY_SHARED_GEMINI_DIR}/config/rules" "${AGY_SHARED_GEMINI_DIR}/config/skills"
agy-api-work-light models
command jq --exit-status --arg root "${AGY_ACCOUNTS_DIR}/api_work_light" \
    --arg adc "${api_gcloud}/application_default_credentials.json" '
    .account == "api_work_light" and .root == $root and
    .env.GOOGLE_CLOUD_PROJECT == "fixture-login-project" and
    .env.GOOGLE_APPLICATION_CREDENTIALS == $adc' "${AGY_TEST_CALLS}" >/dev/null || fail 'light route'
typeset -g light_root="${AGY_ACCOUNTS_DIR}/api_work_light"
[[ ! -e "${light_root}/config/rules" && ! -e "${light_root}/GEMINI.md" && ! -e "${light_root}/config/skills" ]] ||
    fail 'light profile has global instructions or skills'
[[ -L "${light_root}/config/hooks.json" ]] || fail 'light profile lost the shared hooks'
AGY_API_KEY_SPARE=k agy-api spare models
[[ -L "${AGY_ACCOUNTS_DIR}/api_spare/config/rules" && -L "${AGY_ACCOUNTS_DIR}/api_spare/GEMINI.md" &&
   -L "${AGY_ACCOUNTS_DIR}/api_spare/config/skills" ]] || fail 'full profile lacks the global instructions or skills'
#: A light root that once linked the rules or skills loses the links at its
#: next launch.
command ln -s "${AGY_SHARED_GEMINI_DIR:A}/config/rules" "${light_root}/config/rules"
command ln -s "${AGY_SHARED_GEMINI_DIR:A}/config/skills" "${light_root}/config/skills"
agy-api-work-light models
[[ ! -e "${light_root}/config/rules" && ! -e "${light_root}/config/skills" ]] || fail 'light reconcile kept a link'
h-agy-account-info api_work_light
[[ "${reply[2]}" == api-work-light ]] || fail 'light label'
[[ "$(h-agy-account-launcher api_work_light)" == agy-api-work-light ]] || fail 'light resume launcher'
api_rc=0
AGY_API_KEY_WORK_LIGHT=k agy-api work_light models 2>/dev/null || api_rc=$?
(( api_rc == 2 )) || fail 'reserved _light profile name accepted'

# Paseo providers: only our entries change, options set by hand survive, and
# the daemon reloads them.
typeset -gx PASEO_HOME="${test_tmp}/paseo" AGY_REAL_BIN="${test_tmp}/bin/agy"
command mkdir -p "${PASEO_HOME}"
print -r -- '{"daemon":{"listen":"x"},"agents":{"providers":{"agy-acp":{"extends":"acp","label":"other","command":["x"]},"agy-api-work":{"extends":"acp","label":"old","command":["old"],"options":{"handSet":1}}}}}' > "${PASEO_HOME}/config.json"
command cat > "${test_tmp}/bin/paseo" <<'SH'
#!/bin/sh
printf '%s\n' "$*" >> "${PASEO_HOME}/calls"
echo "{\"appliedPaths\":[${AGY_TEST_PASEO_APPLIED-\"agents.providers\"}]}"
SH
command chmod +x "${test_tmp}/bin/paseo"
rehash   #: the real paseo is already hashed
(( ${+functions[agy-api-work-add-to-paseo]} )) || fail 'generated add-to-paseo command'
typeset -g paseo_before="$(<"${PASEO_HOME}/config.json")"
for api_case in key-route no-credentials ; do
    api_rc=0
    case "${api_case}" in
        key-route) AGY_API_KEY_SPARE=k agy-api-add-to-paseo spare 2>/dev/null || api_rc=$? ;;
        no-credentials) AGY_API_PROJECT_NOCREDS=p agy-api-add-to-paseo nocreds 2>/dev/null || api_rc=$? ;;
    esac
    (( api_rc == 2 )) || fail "add-to-paseo ${api_case} went ahead"
done
[[ "$(<"${PASEO_HOME}/config.json")" == "${paseo_before}" && ! -e "${PASEO_HOME}/calls" ]] || fail 'refused add-to-paseo touched Paseo'
agy-api-work-add-to-paseo >/dev/null
command jq --exit-status --arg agy "${AGY_REAL_BIN}" --arg root "${AGY_ACCOUNTS_DIR}/api_work" \
    --arg light "${AGY_ACCOUNTS_DIR}/api_work_light" --arg adc "${api_gcloud}/application_default_credentials.json" '
    .daemon.listen == "x" and .agents.providers["agy-acp"].label == "other" and
    (.agents.providers["agy-api-work"] | .extends == "acp" and
        .command == [$agy, "acp", "--sandbox", "--gemini_dir=\($root)", "--app_data_dir=antigravity-cli"] and
        .env.AGY_ACCOUNT_ID == "api_work" and .env.AGY_ADC_AUTH == "1" and
        .env.GOOGLE_CLOUD_PROJECT == "fixture-login-project" and .env.GOOGLE_APPLICATION_CREDENTIALS == $adc and
        .env.SSH_CONNECTION == "agy-file-auth" and .options == {handSet: 1, probeModelThinking: true}) and
    (.agents.providers["agy-api-work-light"] | .env.ANTIGRAVITY_GEMINI_DIR == $light and
        .command[3] == "--gemini_dir=\($light)" and .env.AGY_ACCOUNT_ID == "api_work_light") and
    ([.. | strings | select(test("fixture-token|authorized_user"))] | length == 0)' \
    "${PASEO_HOME}/config.json" >/dev/null || fail 'Paseo provider entries'
[[ -s "${PASEO_HOME}/config.json.bak" ]] || fail 'no config backup'
[[ "$(<"${PASEO_HOME}/calls")" == "daemon reload --json --home ${PASEO_HOME}" ]] || fail 'daemon not reloaded'
api_rc=0
AGY_TEST_PASEO_APPLIED='' agy-api-work-add-to-paseo >/dev/null 2>&1 || api_rc=$?
(( api_rc == 1 )) || fail 'unapplied reload not reported'
unset PASEO_HOME AGY_REAL_BIN

print -r -- 'ok: launch isolation, named accounts, configuration, status JSON, best, cache-only, login recovery, API profiles, Vertex sign-in, light profiles and Paseo providers'
