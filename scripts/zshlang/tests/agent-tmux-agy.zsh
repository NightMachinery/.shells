#!/usr/bin/env zsh
# Hermetic regression tests. Never connects to a real tmux or kitty server.
# Run with: zsh -f zshlang/tests/agent-tmux-agy.zsh
setopt errexit pipefail typesetsilent

typeset -gr agy_tmux_test_scripts="${0:A:h:h:h}"
typeset -g agy_tmux_test_tmp
agy_tmux_test_tmp="$(command mktemp -d "${TMPDIR:-/tmp}/agy-tmux-tests.XXXXXX")"
function agy-tmux-test-cleanup {
    [[ "${agy_tmux_test_tmp:t}" == agy-tmux-tests.* ]] &&
        command rm -rf -- "${agy_tmux_test_tmp}"
}
trap agy-tmux-test-cleanup EXIT

typeset -gx XDG_STATE_HOME="${agy_tmux_test_tmp}/state"
typeset -gx AGY_TMUX_TEST_CALLS="${agy_tmux_test_tmp}/calls.jsonl"
typeset -gx AGY_TMUX_TEST_FOREGROUND='agy 0'
typeset -gx AGY_TMUX_TEST_IDENTITY_STATUS=0
typeset -gx AGY_TMUX_TEST_MISSING_FALLBACK=0
typeset -gr agy_tmux_test_id='12345678-1234-1234-1234-123456789abc'
typeset -gr agy_tmux_test_payload="{\"conversationId\":\"${agy_tmux_test_id}\"}"
command mkdir -p -- "${agy_tmux_test_tmp}/bin"
command cat > "${agy_tmux_test_tmp}/bin/tmux" <<'SH'
#!/bin/sh
command jq --null-input --compact-output --args '$ARGS.positional' -- "$@" >> "$AGY_TMUX_TEST_CALLS"
case "$1" in
    display-message)
        case "$*" in
            *pane_current_command*)
                if [ "$4" = '%99999' ]; then
                    [ "$AGY_TMUX_TEST_MISSING_FALLBACK" = 1 ] || exit 1
                    printf '%%12 %s\n' "$AGY_TMUX_TEST_FOREGROUND"
                else
                    printf '%s %s\n' "$4" "$AGY_TMUX_TEST_FOREGROUND"
                fi ;;
            *) printf '%s\n' fixture ;;
        esac ;;
    show-option)
        case "$2" in
            -pqv) exit 0 ;;
            -qvA) printf '%s\n' on ;;
            *) exit 99 ;;
        esac ;;
    set-option) exit "$AGY_TMUX_TEST_IDENTITY_STATUS" ;;
    rename-session) exit 0 ;;
    *) exit 99 ;;
esac
SH
command chmod +x "${agy_tmux_test_tmp}/bin/tmux"
typeset -gx PATH="${agy_tmux_test_tmp}/bin:${PATH}"
source "${agy_tmux_test_scripts}/zshlang/basic/core.zsh"
source "${agy_tmux_test_scripts}/zshlang/basic/return-aliases.zsh"
function aliasfn { :; }
function assert-args { :; }
source "${agy_tmux_test_scripts}/zshlang/plugins/agent-session/session.zsh"
source "${agy_tmux_test_scripts}/zshlang/auto-load/others/agent-tmux.zsh"
function h-agent-session-tmux-name { print -r -- '+Agy fixture'; }

function agy-tmux-test-fail {
    print -ru2 -- "FAIL: $*"
    exit 1
}
function agy-tmux-test-call {
    local expected="${1}" label="${2}" ret=0 out
    shift 2
    : >| "${AGY_TMUX_TEST_CALLS}"
    out="$("$@" 2>&1)" || ret=$?
    (( ret == expected )) || agy-tmux-test-fail "${label}: rc ${ret}, expected ${expected}"
    [[ -z "${out}" ]] || agy-tmux-test-fail "${label}: unexpected output"
    print -r -- "ok: ${label} (rc ${ret})"
}
function agy-tmux-test-no-writes {
    command jq --exit-status --slurp \
        'all(.[]; .[0] != "set-option" and .[0] != "rename-session")' \
        "${AGY_TMUX_TEST_CALLS}" >/dev/null || agy-tmux-test-fail 'non-agent pane was modified'
}

# Empty explicit argv must not fall back to an inherited, valid TMUX_PANE.
typeset -gx TMUX_PANE=%12
agy-tmux-test-call 0 empty agy-session-tmux-autoname '' "${agy_tmux_test_payload}"
[[ ! -s "${AGY_TMUX_TEST_CALLS}" ]] || agy-tmux-test-fail 'empty pane queried tmux'
agy-tmux-test-call 0 missing agy-session-tmux-autoname %99999 "${agy_tmux_test_payload}"
agy-tmux-test-no-writes
AGY_TMUX_TEST_MISSING_FALLBACK=1
agy-tmux-test-call 0 missing-with-context-fallback \
    agy-session-tmux-autoname %99999 "${agy_tmux_test_payload}"
agy-tmux-test-no-writes
AGY_TMUX_TEST_MISSING_FALLBACK=0
for AGY_TMUX_TEST_FOREGROUND in 'zsh 0' 'codex 0' 'claude 0' 'agy 1'; do
    agy-tmux-test-call 0 "foreground ${AGY_TMUX_TEST_FOREGROUND}" \
        agy-session-tmux-autoname %12 "${agy_tmux_test_payload}"
    agy-tmux-test-no-writes
done

AGY_TMUX_TEST_FOREGROUND='agy 0'
agy-tmux-test-call 0 interactive agy-session-tmux-autoname %12 "${agy_tmux_test_payload}"
command jq --exit-status --slurp --arg id "${agy_tmux_test_id}" '
    any(.[]; . == ["set-option", "-t", "%12", "@agent_session", "agy\t" + $id + "\t"]) and
    any(.[]; . == ["rename-session", "-t", "%12", "+Agy fixture"])
' "${AGY_TMUX_TEST_CALLS}" >/dev/null || agy-tmux-test-fail 'interactive identity/rename missing'
agy-tmux-test-call 0 stdin agy-session-tmux-autoname %12 <<< "${agy_tmux_test_payload}"

# A recognized pane's genuine identity failure remains observable.
AGY_TMUX_TEST_IDENTITY_STATUS=13
agy-tmux-test-call 13 identity-failure agy-session-tmux-autoname %12 "${agy_tmux_test_payload}"
command jq --exit-status '.agent == "agy" and .outcome == "identity-failed"' \
    "${XDG_STATE_HOME}/agent-sessions/hooks/agy-12.json" >/dev/null ||
    agy-tmux-test-fail 'missing identity-failure diagnostic'
command jq --exit-status --slurp 'all(.[]; .[0] != "rename-session")' \
    "${AGY_TMUX_TEST_CALLS}" >/dev/null || agy-tmux-test-fail 'rename attempted after failed identity'
AGY_TMUX_TEST_IDENTITY_STATUS=0
for input in '{' '{}' '{"conversationId":42}'; do
    agy-tmux-test-call 0 invalid-payload agy-session-tmux-autoname %12 "${input}"
    agy-tmux-test-no-writes
done

# Sibling hooks already treat print sessions without a window/registration as
# ordinary no-ops. Keep all discovery and watcher paths inside this fixture.
source "${agy_tmux_test_scripts}/zshlang/auto-load/others/agent-session.zsh"
source "${agy_tmux_test_scripts}/zshlang/auto-load/others/agy-session.zsh"
source "${agy_tmux_test_scripts}/zshlang/auto-load/others/agent-auto-continue.zsh"
function h-agent-session-kitty-socket { return 1; }
function h-agent-auto-continue-dir { print -r -- "${agy_tmux_test_tmp}/continue"; }
function h-agy-session-roots { print -r -- "${agy_tmux_test_tmp}/brain"; }
function h-agent-session-call { "h-${1}-session-${2}" "${@[3,-1]}"; }
command mkdir -p -- "${agy_tmux_test_tmp}/brain" "${agy_tmux_test_tmp}/continue"
: >| "${agy_tmux_test_tmp}/transcript.jsonl"
typeset transcript_payload="{\"conversationId\":\"${agy_tmux_test_id}\",\"transcriptPath\":\"${agy_tmux_test_tmp}/transcript.jsonl\"}"
for TMUX_PANE in %12 %99999 ''; do
    agy-tmux-test-call 0 "register ${TMUX_PANE:-empty}: missing transcript" \
        agent-session-register agy "${agy_tmux_test_payload}"
    agy-tmux-test-call 0 "register ${TMUX_PANE:-empty}: no kitty window" \
        agent-session-register agy "${transcript_payload}"
    agy-tmux-test-call 0 "auto-continue ${TMUX_PANE:-empty}: unregistered" \
        agent-auto-continue-hook agy "${TMUX_PANE}" "${agy_tmux_test_payload}"
done

# Registered auto-continue failures are also optional; never start a watcher.
command mkdir -p -- "${agy_tmux_test_tmp}/continue/agy"
: >| "${agy_tmux_test_tmp}/continue/agy/agy-${agy_tmux_test_id}"
function h-agent-auto-continue-watch-ensure { return 17; }
function agent-auto-continue-check { return 18; }
agy-tmux-test-call 0 registered-auto-continue \
    agent-auto-continue-hook agy %99999 "${agy_tmux_test_payload}"
print -r -- 'ok: Antigravity tmux and sibling hooks'
