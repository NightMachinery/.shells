#!/usr/bin/env zsh
# zsh -f: isolated file-auth command fixture, never credentials or network.
setopt err_exit pipe_fail extended_glob
typeset -g test_root="${0:A:h:h:h}"
typeset -g test_tmp="$(command mktemp -d /private/tmp/agy-best-models.XXXXXX)"
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
function fail { print -ru2 -- "$*"; exit 1 }
typeset -gx NIGHTDIR="${test_root}" AGY_TEST_DATA="${test_tmp}"
typeset -gx AGY_ACCOUNTS='primary basic broken'
typeset -gx AGY_ACCOUNTS_DIR="${test_tmp}/accounts"
command mkdir -p "${test_tmp}/bin" "${test_tmp}/accounts/primary/antigravity-cli" \
    "${test_tmp}/accounts/basic/antigravity-cli" "${test_tmp}/accounts/broken/antigravity-cli"
for id in primary basic broken ; do
    print -r -- 'inert fixture' > "${test_tmp}/accounts/${id}/antigravity-cli/antigravity-oauth-token"
done
command cat > "${test_tmp}/bin/agy" <<'PY'
#!/usr/bin/env python3
import json, os
from pathlib import Path
import sys
root = Path(os.environ['AGY_TEST_DATA'])
account = os.environ['AGY_ACCOUNT_ID']
assert os.environ['SSH_CONNECTION']
if sys.argv[-1] == 'models':
    with (root / 'queries').open('a') as stream:
        stream.write(account + '\n')
    if account == 'broken' or (root / 'fail-refresh').exists():
        raise SystemExit(1)
    print((root / (account + '.tsv')).read_text(), end='')
else:
    (root / 'launch.json').write_text(json.dumps({'account':account, 'argv':sys.argv[1:]}))
PY
command chmod +x "${test_tmp}/bin/agy"
typeset -gx PATH="${test_tmp}/bin:${PATH}"
print -r -- $'gemini-4-argon-high\tGemini 4 Argon (High)\ngemini-3.8-flash-low\tGemini 3.8 Flash (Low)\nclaude-opus-5-5-low\tClaude Opus 5.5 (Low)' > "${test_tmp}/primary.tsv"
print -r -- $'gemini-3.8-flash-low\tGemini 3.8 Flash (Low)\nclaude-sonnet-4-6\tClaude Sonnet 4.6' > "${test_tmp}/basic.tsv"
command cat > "${test_tmp}/status.json" <<'JSON'
[
 {"account":"primary","signed_in":true,"buckets":[
   {"bucket_id":"gemini-weekly","remaining_percent":40},
   {"bucket_id":"gemini-4-argon-5h","remaining_percent":20},
   {"bucket_id":"gemini-3-8-flash-5h","remaining_percent":0},
   {"bucket_id":"3p-weekly","remaining_percent":30},
   {"bucket_id":"claude-opus-5-5-weekly","remaining_percent":25},
   {"bucket_id":"claude-sonnet-5-5-5h","remaining_percent":0}]},
 {"account":"basic","signed_in":true,"buckets":[
   {"bucket_id":"gemini-weekly","remaining_percent":90},
   {"bucket_id":"gemini-3-8-flash-5h","remaining_percent":80},
   {"bucket_id":"3p-weekly","remaining_percent":90}]},
 {"account":"broken","signed_in":true,"buckets":[
   {"bucket_id":"gemini-weekly","remaining_percent":100}]}
]
JSON
source "${test_root}/zshlang/auto-load/others/antigravity.zsh"
function h-agent-launch { shift; command "$@" }
function agy-status { command cat "${test_tmp}/status.json" }
agy-best --model 'Gemini 4 Argon (High)' --print 'fixture'
command jq --exit-status '.account == "primary" and (.argv | index("gemini-4-argon-high") != null)' "${test_tmp}/launch.json" >/dev/null
agy-best --model=claude-opus-5-5-low --print fixture
command jq --exit-status '.account == "primary"' "${test_tmp}/launch.json" >/dev/null
agy_best_model='Gemini 3.8 Flash (Low)' agy-best --print fixture
command jq --exit-status '.account == "basic" and (.argv | index("gemini-3.8-flash-low") != null)' "${test_tmp}/launch.json" >/dev/null
[[ "$(command rg --count '^primary$' "${test_tmp}/queries")" == 1 ]] || fail 'fresh catalog refreshed repeatedly'
command rm "${test_tmp}/launch.json"
if agy-best --model gemini-4-argon-low --print fixture 2>/dev/null; then fail 'unavailable exact variant launched'; fi
[[ ! -e "${test_tmp}/launch.json" ]] || fail 'unsupported model reached launcher'
if agy-best --model 'gemini-4-argon-*' --print fixture 2>/dev/null; then fail 'wildcard fallback launched'; fi
if agy-best --model 2>/dev/null; then fail 'missing model value accepted'; fi
if agy-best --conversation fixture --model= 2>/dev/null; then fail 'empty model value accepted'; fi
# A signed-out account must never invoke native models or browser sign-in.
command rm "${test_tmp}/accounts/basic/antigravity-cli/antigravity-oauth-token"
typeset -g before_queries="$(command wc -l < "${test_tmp}/queries")"
if agy-models-refresh basic >/dev/null 2>/dev/null; then fail 'signed-out refresh succeeded'; fi
[[ "$(command wc -l < "${test_tmp}/queries")" == "${before_queries}" ]] || fail 'signed-out refresh invoked agy'
print -r -- 'inert fixture' > "${test_tmp}/accounts/basic/antigravity-cli/antigravity-oauth-token"


# Explicit refresh works for names, defaults to all, and preserves each account error.
agy-models-refresh basic >/dev/null
if agy-models-refresh --all >/dev/null 2>/dev/null; then fail 'broken catalog hidden by refresh'; fi

# Failed refresh must not authorize using an expired old successful catalog.
command touch "${test_tmp}/fail-refresh"
command python3 - "${test_tmp}/accounts" <<'PY'
import json, sys
from pathlib import Path
for cache in Path(sys.argv[1]).rglob('models-cache.json'):
    data=json.loads(cache.read_text()); data['captured_at']=1
    cache.write_text(json.dumps(data))
PY
if agy-best --model gemini-4-argon-high --print fixture 2>/dev/null; then fail 'stale catalog used after failed refresh'; fi
[[ ! -e "${test_tmp}/launch.json" ]] || fail 'stale model reached launcher'
command rm "${test_tmp}/fail-refresh"

# Resume remains pinned to its owner; requested variants must also exist there.
function h-agy-session-transcript-of { print -r -- /fixture/transcript }
function h-agy-account-of-transcript { print -r -- basic }
if agy-best --conversation fixture-id --model gemini-4-argon-high --print fixture 2>/dev/null; then fail 'owner model mismatch moved account'; fi
agy-best --conversation fixture-id --model 'Gemini 3.8 Flash (Low)' --print fixture
command jq --exit-status '.account == "basic" and (.argv | index("--conversation") != null)' "${test_tmp}/launch.json" >/dev/null
print -r -- 'ok: catalog eligibility, model-specific quotas, canonical argv, TTL, refresh errors and owner resume'
