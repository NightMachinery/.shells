#!/usr/bin/env zsh
# Isolated registry, tmux and picker fixtures. Never attach or send pane input.
setopt err_exit pipe_fail extended_glob typeset_silent
typeset -gr test_root="${0:A:h:h:h}"
typeset -g test_tmp="$(command mktemp -d /private/tmp/agent-subagent-picker.XXXXXX)"
trap 'command rm -rf -- "${test_tmp}"' EXIT
source "${test_root}/zshlang/basic/basic.plugin.zsh"
function aliasfn { :; }
function aliasfnq { :; }
source "${test_root}/zshlang/auto-load/others/agent-session.zsh"
source "${test_root}/zshlang/auto-load/others/agent-subagents.zsh"
source "${test_root}/zshlang/auto-load/others/fuzzy/fuzzy.zsh"
function fail { print -ru2 -- "$*"; exit 1 }
function h-agent-session-live-list { return 0 }
function h-agent-session-root-agents { return 0 }
typeset -gx TMUX_SUBAGENTS_STATE="${test_tmp}/state"
command mkdir -p "${test_tmp}/bin" "${TMUX_SUBAGENTS_STATE}"
command cat > "${test_tmp}/bin/tmux" <<'PY'
#!/usr/bin/env python3
import sys
if sys.argv[1] == 'list-sessions':
    print('$21\tag--fixture--review--agy--abcdef\t')
elif sys.argv[1] == 'list-panes':
    print('%31\t0\t1234')
else:
    raise SystemExit('unexpected tmux mutation: ' + sys.argv[1])
PY
command chmod +x "${test_tmp}/bin/tmux"
typeset -gx PATH="${test_tmp}/bin:${PATH}"
print -r -- '{"ag--fixture--review--agy--abcdef":{"provider":"agy","tmux_session_id":"$21","tmux_pane_id":"%31","created":"2026-01-01T00:00:00"}}' > "${TMUX_SUBAGENTS_STATE}/agents.json"
typeset -g real_list="$(agent-subagents-list)"
print -r -- "${real_list}" | command awk -F'\t' 'NF != 13 || $12 != "-" || $13 != "agy" {exit 1}' || fail 'registry provider lost without identity'

# A missing, nonexistent or not-yet-written transcript must not prevent goto.
typeset -g known="${test_tmp}/transcript with 'quote.jsonl"
print -r -- '{}' > "${known}"
function agent-subagents-list {
    printf '%s\t%s\t-\t%s\t%%31\tfixture\t-\t0\t%s\t%s\t/work\t%s\tagy\n' \
        known busy '$21' 'known stamp' 100 "${known}" \
        pending mismatch '$22' 'pending stamp' 400 - \
        missing idle '$23' 'missing stamp' 300 "${test_tmp}/absent.jsonl" \
        exited exited '$24' 'exited stamp' 200 - \
        gone gone '$25' 'gone stamp' 500 - \
        no-session unknown - 'unknown stamp' 600 -
}
typeset -g rows="$(h-agent-session-subagent-rows)"
print -r -- "${rows}" > "${test_tmp}/raw.tsv"
command python3 - "${test_tmp}/raw.tsv" "${known}" <<'PY'
from pathlib import Path
import sys
rows=[row.split('\t') for row in Path(sys.argv[1]).read_text().splitlines()]
assert len(rows)==4
assert rows[0][1]==sys.argv[2] and rows[0][3]=='known  -'
assert all(row[1]=='-' and '(no transcript)' in row[3] for row in rows[1:])
assert rows[1][4]=='❌' and rows[3][4]=='🏁'
assert rows[1][5:]==['400','pending stamp']
PY
function h-agents { print -r -- agy }
function h-agent-field { print -r -- A }
function h-agent-session-dep { return 0 }
function h-agent-session-call {
    case "${2}" in
        roots) print -r -- "${test_tmp}" ;;
        owns-p) [[ "${3}" != '-' && -f "${3}" ]] ;;
    esac
}
function agent_session {
    printf '100\t%s\tknown stamp\tKnown\tknown.jsonl\tsnippet\n' "${known}"
}
function h-agent-session-live-list-offerable { fail 'subagents-only queried live sessions' }
typeset -g annotated="$(agent_session_tmux_subagents_p=only h-agent-session-tmux-rows)"
[[ "${annotated%%$'\t'*}" == '$22' ]] || fail 'missing transcript row did not retain activity sorting'
[[ "${annotated}" == *$'❌ A pending  (no transcript)'* ]] || fail 'missing transcript state/label lost'
[[ "$(print -r -- "${annotated}" | command wc -l | command tr -d ' ')" == 4 ]] || fail 'picker dropped an attachable row'

# Capture the actual fzf bindings, then run their shell code with inert previews.
function bella_zsh_disable1 { return 0 }
function h-agent-session-preview-cmd { print -r -- "printf 'preview:%s\\n'" }
function h-agent-session-open-cmd { print -r -- "printf 'open:%s\\n'" }
function h-agent-session-fz {
    local arg
    while (( $# )) ; do
        arg="$1"; shift
        case "${arg}" in
            --preview) print -r -- "$1" > "${test_tmp}/preview"; shift ;;
            --bind) print -r -- "$1" > "${test_tmp}/open"; shift ;;
        esac
    done
    command cat | command head -n 1
}
function h-fftmux-act { print -r -- "$1" > "${test_tmp}/pick" }
fftmux_agent_subagents_p=only fftmux-agent
command python3 - "${test_tmp}" "${known}" <<'PY'
from pathlib import Path
import shlex
import subprocess
import sys
root=Path(sys.argv[1])
preview=(root/'preview').read_text().strip()
opening=(root/'open').read_text().strip().removeprefix('alt-enter:execute-silent(').removesuffix(')')
for transcript in ['-',sys.argv[2]]:
    def run(code):
        result=subprocess.run(['dash','-c',code.replace('{2}',shlex.quote(transcript))],capture_output=True,text=True)
        assert result.returncode==0,result.stderr
        return result.stdout
    if transcript=='-':
        assert run(preview)=='No transcript available; Enter opens this tmux session.\n'
        assert run(opening)==''
    else:
        assert run(preview)=='preview:'+transcript+'\n'
        assert run(opening)=='open:'+transcript+'\n'
assert (root/'pick').read_text().startswith('$22\t-\tagy\t')
PY
print -r -- 'ok: transcript-free registered children, provider fallback, state/recency and safe preview/open'
