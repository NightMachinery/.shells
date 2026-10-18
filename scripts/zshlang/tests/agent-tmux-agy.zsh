#!/usr/bin/env zsh
# Default: hermetic regressions without real tmux/kitty connections.
# --integration: real DECSET proxy and inert agents on an isolated tmux socket.
# Run with: zsh -f zshlang/tests/agent-tmux-agy.zsh
setopt errexit pipefail typesetsilent

typeset -gr agy_tmux_test_scripts="${0:A:h:h:h}"
# Real launcher coverage is opt-in because it needs tmux socket and ps access.
# Run: zsh -f zshlang/tests/agent-tmux-agy.zsh --integration
if [[ "${1:-}" == --integration ]] ; then
    command python3 - "${agy_tmux_test_scripts}" "$(whence -p decset-rewrite)" <<'PY'
import os, pathlib, shlex, shutil, signal, subprocess, sys, tempfile, time
scripts, decset = sys.argv[1:]
assert decset, 'install decset-rewrite before running --integration'
base = pathlib.Path(tempfile.mkdtemp(prefix='agy-decset-test-'))
socket = 'agy-decset-test-' + str(os.getpid())
tmux = shutil.which('tmux')
created = []
def tm(*args):
    return subprocess.check_output([tmux, '-L', socket, *args], text=True).strip()
def processes():
    rows = {}
    for row in subprocess.check_output(['ps', '-axo', 'pid=,ppid=,comm='], text=True).splitlines():
        pid, parent, comm = row.strip().split(None, 2)
        rows[int(pid)] = (int(parent), comm)
    return rows
def descendants(root, rows):
    owned = {root}
    while True:
        children = {pid for pid, (parent, _) in rows.items() if parent in owned}
        if children <= owned:
            return owned
        owned |= children
def create(name, args, first=False):
    opts = ['-f', '/dev/null'] if first else []
    pane = tm(*opts, 'new-session', '-d', '-P', '-F', '#{pane_id}', '-s', name, shlex.join(args))
    assert pane.startswith('%'), pane
    created.append(pane)
    # Assert the socket before setting options or running any hook.
    assert tm('display-message', '-p', '-t', pane, '#{socket_path}').endswith('/' + socket)
    return pane
try:
    binary_dir = base / 'with spaces'
    binary_dir.mkdir()
    src = base / 'fixture.c'
    src.write_text('#include <unistd.h>\nint main(int n,char **v){if(n>1 && fork()==0){execl(v[1],v[1],(char*)0);_exit(1);}for(;;)pause();}\n')
    for name in ['agy', 'codex']:
        subprocess.run(['cc', str(src), '-o', str(binary_dir / name)], check=True)
    (base / 'bin').mkdir()
    shim = base / 'bin' / 'tmux'
    shim.write_text('#!/bin/sh\nexec ' + shlex.quote(tmux) + ' -L ' + shlex.quote(socket) + ' "$@"\n')
    shim.chmod(0o755)
    direct = create('direct-agy', [str(binary_dir / 'agy')], True)
    wrapped = create('wrapped-agy', [decset, '-map', '1003=1002', '--', str(binary_dir / 'agy')])
    # The other agent spawns an agy grandchild on purpose. Descendant-based
    # recognition would incorrectly accept this inherited-pane case.
    other = create('wrapped-codex', [decset, '-map', '1003=1002', '--', str(binary_dir / 'codex'), str(binary_dir / 'agy')])
    for pane, expected in [(direct, 'agy'), (wrapped, 'decset-rewrite'), (other, 'decset-rewrite')]:
        for _ in range(30):
            if tm('display-message', '-p', '-t', pane, '#{pane_current_command}') == expected:
                break
            time.sleep(.05)
        else:
            raise AssertionError('fixture foreground did not start')
        tm('set-option', '-t', pane, '@agent_autoname', 'on')
    # Proxy foreground can appear before the child exec. Require the actual
    # direct child, and the nested negative fixture, before testing the hook.
    for pane, expected in [(wrapped, 'agy'), (other, 'codex')]:
        root = int(tm('display-message', '-p', '-t', pane, '#{pane_pid}'))
        for _ in range(30):
            rows = processes()
            proxies = {pid for pid in descendants(root, rows) if rows[pid][1] == decset}
            children = {pid for pid, (parent, comm) in rows.items()
                        if parent in proxies and comm == str(binary_dir / expected)}
            nested = any(parent in children and comm == str(binary_dir / 'agy')
                         for parent, comm in rows.values())
            if children and (expected == 'agy' or nested):
                break
            time.sleep(.05)
        else:
            raise AssertionError('direct/nested fixture child did not exec')
    tm('set-option', '-t', other, '@agent_session', 'unchanged-fixture')
    shell = r'''
path=("$2/bin" $path)
rehash
source "$1/zshlang/basic/core.zsh"
source "$1/zshlang/basic/return-aliases.zsh"
function aliasfn { :; }
source "$1/zshlang/plugins/agent-session/session.zsh"
source "$1/zshlang/auto-load/others/agent-tmux.zsh"
function h-agent-session-tmux-name { print -r -- "+Agy $2"; }
XDG_STATE_HOME="$2/state"
[[ "$(command tmux display-message -p -t "$3" '#{socket_path}')" == */"$6" ]] || exit 99
function check-hook {
    local pane="$1" id="$2" rc=0 out
    out="$(agy-session-tmux-autoname "$pane" "{\"conversationId\":\"$id\"}" 2>&1)" || rc=$?
    print -r -- "hook ${pane:-empty}: rc=$rc bytes=${#out}"
    [[ $rc == 0 && -z "$out" ]]
}
check-hook "$3" direct && check-hook "$4" wrapped && check-hook "$5" other &&
    check-hook %99999 absent && check-hook '' empty || exit 98
function h-agent-tmux-identity-set { return 41; }
agy-session-tmux-autoname "$4" '{"conversationId":"wrapped"}'; rc=$?
print -r -- "wrapped genuine failure: rc=$rc"
[[ $rc == 41 ]] || exit 97
'''
    subprocess.run(['zsh', '-f', '-c', shell, 'integration', scripts, str(base), direct, wrapped, other, socket], check=True)
    assert tm('display-message', '-p', '-t', direct, '#S') == '+Agy direct'
    assert tm('display-message', '-p', '-t', wrapped, '#S') == '+Agy wrapped'
    assert tm('show-option', '-qv', '-t', wrapped, '@agent_session') == 'agy\twrapped'
    assert tm('display-message', '-p', '-t', other, '#S') == 'wrapped-codex'
    assert tm('show-option', '-qv', '-t', other, '@agent_session') == 'unchanged-fixture'
    print('ok: real decset-rewrite agy ownership, rename and nested non-agy exclusion')
finally:
    # Record only these panes' trees, then check both their PIDs and exact
    # unique fixture executable paths after tmux teardown. Never match argv.
    rows = processes()
    owned = set()
    for pane in created:
        root = int(tm('display-message', '-p', '-t', pane, '#{pane_pid}'))
        owned |= descendants(root, rows)
    for pane in created:
        subprocess.run([tmux, '-L', socket, 'kill-session', '-t', pane], check=True)
    expected = {str(binary_dir / name) for name in ['agy', 'codex']}
    def leftovers():
        current = processes()
        return {pid for pid, (_, comm) in current.items()
                if comm in expected or (pid in owned and comm == rows[pid][1])}
    time.sleep(.1)
    for sig in [signal.SIGTERM, signal.SIGKILL]:
        remaining = leftovers()
        if not remaining:
            break
        print('terminating isolated fixture leftovers:', sorted(remaining))
        for pid in remaining:
            try:
                os.kill(pid, sig)
            except ProcessLookupError:
                pass
        time.sleep(.1)
    assert not leftovers(), 'fixture processes survived cleanup'
    shutil.rmtree(base)
PY
    exit $?
fi

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
typeset -gx AGY_TMUX_TEST_TTY_ROWS=''
typeset -gx AGY_TMUX_TEST_CHILD_ROWS=''
typeset -gx AGY_TMUX_TEST_PS_CALLS="${agy_tmux_test_tmp}/ps-calls"
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
                    printf '%%12 %s /dev/ttys123\n' "$AGY_TMUX_TEST_FOREGROUND"
                else
                    printf '%s %s /dev/ttys123\n' "$4" "$AGY_TMUX_TEST_FOREGROUND"
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
command cat > "${agy_tmux_test_tmp}/bin/ps" <<'SH'
#!/bin/sh
printf '%s\n' "$*" >> "$AGY_TMUX_TEST_PS_CALLS"
case "$1" in
    -t) printf '%s\n' "$AGY_TMUX_TEST_TTY_ROWS" ;;
    -axo) printf '%s\n' "$AGY_TMUX_TEST_CHILD_ROWS" ;;
    *) exit 99 ;;
esac
SH
command chmod +x "${agy_tmux_test_tmp}/bin/tmux" "${agy_tmux_test_tmp}/bin/ps"
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
    : >| "${AGY_TMUX_TEST_PS_CALLS}"
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

# Direct agy keeps the cheap path: no process-table scans.
[[ ! -s "${AGY_TMUX_TEST_PS_CALLS}" ]] || agy-tmux-test-fail 'direct agy queried ps'
AGY_TMUX_TEST_FOREGROUND='decset-rewrite 0'
AGY_TMUX_TEST_TTY_ROWS='700 700 700 /opt/with spaces/bin/decset-rewrite'
AGY_TMUX_TEST_CHILD_ROWS='700 /opt/with spaces/bin/agy'
agy-tmux-test-call 0 wrapped-agy agy-session-tmux-autoname %12 "${agy_tmux_test_payload}"
command jq --exit-status --slurp 'any(.[]; .[0] == "set-option") and any(.[]; .[0] == "rename-session")' \
    "${AGY_TMUX_TEST_CALLS}" >/dev/null || agy-tmux-test-fail 'wrapped agy not recorded/renamed'
(( $(command wc -l < "${AGY_TMUX_TEST_PS_CALLS}") == 2 )) || agy-tmux-test-fail 'wrapper query count'
AGY_TMUX_TEST_IDENTITY_STATUS=13
agy-tmux-test-call 13 wrapped-identity-failure agy-session-tmux-autoname %12 "${agy_tmux_test_payload}"
AGY_TMUX_TEST_IDENTITY_STATUS=0
for AGY_TMUX_TEST_CHILD_ROWS in '700 /opt/bin/codex' '700 /opt/bin/claude' $'700 /opt/bin/codex\n701 /opt/bin/agy'; do
    agy-tmux-test-call 0 wrapped-other-agent agy-session-tmux-autoname %12 "${agy_tmux_test_payload}"
    agy-tmux-test-no-writes
done
AGY_TMUX_TEST_CHILD_ROWS='700 /opt/with spaces/bin/agy'
AGY_TMUX_TEST_TTY_ROWS='700 700 999 /opt/bin/decset-rewrite'
agy-tmux-test-call 0 background-proxy agy-session-tmux-autoname %12 "${agy_tmux_test_payload}"
agy-tmux-test-no-writes
AGY_TMUX_TEST_TTY_ROWS=$'700 700 700 /opt/bin/decset-rewrite\n701 700 700 /opt/bin/decset-rewrite'
agy-tmux-test-call 0 ambiguous-proxies agy-session-tmux-autoname %12 "${agy_tmux_test_payload}"
agy-tmux-test-no-writes
AGY_TMUX_TEST_FOREGROUND='agy 0'

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
