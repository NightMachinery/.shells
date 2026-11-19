#!/usr/bin/env zsh
# Paseo id -> native transcript, the conversion's progress and per-session
# cancel, and deleting archived sessions. Fixture-only: a fake Paseo home, fake
# Claude/Codex homes and a fake `paseo' on PATH, all under one temp directory;
# HOME points there too, so nothing outside it can be read or deleted.
# Run with: zsh -f zshlang/tests/paseo-transcript.zsh

setopt errexit pipefail typesetsilent nobgnice

typeset -gr pt_root="${0:A:h:h:h}"
typeset -g pt_tmp
pt_tmp="$(command mktemp -d "${TMPDIR:-/tmp}/paseo-transcript-tests.XXXXXX")"

function pt-cleanup {
    if [[ -n "${pt_tmp:-}" && -d "${pt_tmp}" && "${pt_tmp:t}" == paseo-transcript-tests.* ]] ; then
        command rm -rf -- "${pt_tmp}"
    fi
}
trap pt-cleanup EXIT

unset night_basic_plugin_loaded_p
source "${pt_root}/zshlang/basic/basic.plugin.zsh"
function aliasfn { :; }
function aliasfnq { :; }
source "${pt_root}/zshlang/auto-load/others/processes.zsh"
source "${pt_root}/zshlang/auto-load/others/agents.zsh"
source "${pt_root}/zshlang/auto-load/others/agent-session.zsh"
source "${pt_root}/zshlang/auto-load/others/paseo.zsh"
unalias ecerr 2>/dev/null || true
function ecerr { print -ru2 -- "$@"; }
typeset -g color_p="" isColor_override=n

typeset -g t="${pt_tmp}"
#: Under the temp directory: the default homes resolve here, never to ~.
export HOME="${t}/home"
typeset -g paseo_home="${t}/paseo"
typeset -g agent_view_lock_dir="${t}/locks"
unset PASEO_HOST PASEO_HOME PASEO_AGENT_ID 2>/dev/null || true
command mkdir -p -- "${HOME}" "${paseo_home}/agents/-p"

function h-agent-session-call {
    [[ "${2}" == roots ]] || return 1
    case "${1}" in
        claude) print -r -- "${t}/claude-other/projects" ;;
        codex) print -r -- "${t}/codex-other/sessions" ;;
    esac
}
function h-agent-view-banner { print -r -- "banner ${1}: ${@[5,-1]}" >> "${t}/banners"; }
function h-agent-view-dismiss { :; }
function emc-open { print -r -- "${1}" >> "${t}/opened"; }
function fzf-gateway { command fzf "$@"; }

function expect-eq {
    if [[ "${1}" != "${2}" ]] ; then
        print -ru2 -- "FAIL: ${3}"
        print -ru2 -- "  want: ${(q+)2}"
        print -ru2 -- "  got:  ${(q+)1}"
        exit 1
    fi
}
function expect-match {
    if [[ ! "${1}" =~ "${2}" ]] ; then
        print -ru2 -- "FAIL: ${3}"
        print -ru2 -- "  want match: ${2}"
        print -ru2 -- "  got: ${(q+)1}"
        exit 1
    fi
}

#: The providers: two handoff aliases with their own homes.
print -r -- '{"agents":{"providers":{
  "handoff-claude-x":{"extends":"claude","env":{"CLAUDE_CONFIG_DIR":"'"${t}"'/claude-alt"}},
  "handoff-codex-x":{"extends":"codex","env":{"CODEX_HOME":"'"${t}"'/codex"}},
  "claude":{}, "codex":{}}}}' > "${paseo_home}/config.json"

function pt-record {
    #: Usage: pt-record <id> <provider> <persistence json> [extra json]
    print -r -- "{\"id\":\"${1}\",\"provider\":\"${2}\",\"title\":\"T ${1[1,4]}\",\"cwd\":\"/p\",\"lastStatus\":\"closed\",\"internal\":false,\"persistence\":${3}}" \
        | command jq --compact-output ". + ${4:-{\}}" > "${paseo_home}/agents/-p/${1}.json"
}

typeset -g a_claude=aaaaaaaa-0000-4000-8000-000000000001 s_claude=11111111-1111-4111-8111-111111111111
typeset -g a_codex=aaaaaaaa-0000-4000-8000-000000000002 s_codex=22222222-2222-4222-8222-222222222222
typeset -g a_agy=bbbbbbbb-0000-4000-8000-000000000003
typeset -g a_other=cccccccc-0000-4000-8000-000000000004 s_other=44444444-4444-4444-8444-444444444444
typeset -g t_claude="${t}/claude-alt/projects/-p/${s_claude}.jsonl"
typeset -g t_codex="${t}/codex/sessions/2026/10/11/rollout-2026-10-11T00-00-00-${s_codex}.jsonl"
typeset -g t_other="${t}/claude-other/projects/-q/${s_other}.jsonl"
command mkdir -p -- "${t_claude:h}" "${t_codex:h}" "${t_other:h}"
: > "${t_claude}" ; : > "${t_codex}" ; : > "${t_other}"

pt-record "${a_claude}" handoff-claude-x "{\"sessionId\":\"${s_claude}\",\"metadata\":{\"provider\":\"claude\"}}"
#: Codex: the thread id is the one that names the rollout.
pt-record "${a_codex}" handoff-codex-x "{\"sessionId\":\"99999999-9999-4999-8999-999999999999\",\"metadata\":{\"provider\":\"codex\",\"threadId\":\"${s_codex}\"}}"
pt-record "${a_agy}" antigravity '{"sessionId":"33333333-3333-4333-8333-333333333333","metadata":{"provider":"acp"}}'
#: Plain `claude', no home of its own: found in another profile's root.
pt-record "${a_other}" claude "{\"sessionId\":\"${s_other}\"}"

# --- Resolution
expect-eq "$(paseo-transcript-path "${a_claude}")" "${t_claude}" 'claude through a provider alias'
expect-eq "$(paseo-transcript-path "${a_codex[1,8]}-0000-4000-8000-000000000002")" "${t_codex}" 'codex thread id'
expect-eq "$(paseo-transcript-path "${a_other}")" "${t_other}" 'claude fallback to the other roots'
expect-eq "$(PASEO_AGENT_ID="${a_claude}" paseo-transcript-path)" "${t_claude}" 'PASEO_AGENT_ID default'
expect-eq "$(paseo-transcript-path cccc)" "${t_other}" 'unique prefix'
err="$(paseo-transcript-path aaaaaaaa 2>&1)" && exit 1
expect-match "${err}" '2 Paseo agents match' 'ambiguous prefix'
err="$(paseo-transcript-path "${a_agy}" 2>&1)" && exit 1
expect-match "${err}" 'not Claude Code or Codex' 'unsupported provider'
err="$(paseo-transcript-path 'x*' 2>&1)" && exit 1
expect-match "${err}" 'not a Paseo agent id' 'glob characters refused'
command mv -- "${t_claude}" "${t}/moved"
err="$(paseo-transcript-path "${a_claude}" 2>&1)" && exit 1
expect-match "${err}" 'no claude transcript on disk' 'missing transcript'
command mv -- "${t}/moved" "${t_claude}"

# --- Progress text
expect-eq "$(h-agent-view-progress-text '42 pandoc 1048576 3145728')" '42%  pandoc 1.0/3.0 MB' 'progress text bytes'
expect-eq "$(h-agent-view-progress-text '13 render 5 10')" '13%  render 5/10 turns' 'progress text turns'

# --- Cancel on rerun: the second run for one transcript kills the first's tree.
function pt-slow-convert {
    #: Reports progress like the renderer, and leaves a grandchild to be killed.
    sleep 30 &
    print -r -- "$!" > "${t}/grandchild.${2:t}"
    print -r -- "5 read 1 20" > "${agent_session_progress_file}"
    sleep 1
    print -r -- "50 read 10 20" > "${agent_session_progress_file}"
    wait
}
function pt-fast-convert {
    print -r -- "50 read 1 2" > "${agent_session_progress_file}"
    sleep 0.3
    print -r -- converted > "${2}"
}
command mkdir -p -- "${t}/out1" "${t}/out2"
agent_view_progress=tty agent_view_progress_interval=0.05 \
    h-agent-view-transcript-run band-x hint pt-slow-convert "${t_claude}" "${t}/out1/a.org" 2> "${t}/run1.err" &
run1=$!
for i in {1..100} ; do test -s "${t}/grandchild.a.org" && break ; sleep 0.05 ; done
sleep 0.3
grandchild="$(<"${t}/grandchild.a.org")"
agent_view_progress=tty agent_view_progress_interval=0.05 \
    h-agent-view-transcript-run band-x hint pt-fast-convert "${t_claude}" "${t}/out2/a.org" 2> "${t}/run2.err"
ret2=$?
ret1=0 ; wait "${run1}" || ret1=$?
expect-eq "${ret2}" 0 'second run succeeds'
expect-eq "${ret1}" 3 'first run reports it was superseded'
expect-eq "$(<"${t}/out2/a.org")" converted 'second run converted'
kill -0 "${grandchild}" 2>/dev/null && { print -ru2 -- 'FAIL: first run left its tree running' ; exit 1 }
expect-match "$(<"${t}/run1.err")" '5%  read' 'tty progress line drawn'
test ! -e "${agent_view_lock_dir}/${s_claude}.pid" || { print -ru2 -- 'FAIL: claim left behind' ; exit 1 }

#: Different sessions run side by side: neither cancels the other.
: > "${t}/grandchild.b.org"
h-agent-view-transcript-run band-y hint pt-fast-convert "${t_codex}" "${t}/out1/b.org" 2>/dev/null &
run3=$!
agent_view_progress=none h-agent-view-transcript-run band-z hint pt-fast-convert "${t_other}" "${t}/out2/b.org"
wait "${run3}"
expect-eq "$(<"${t}/out1/b.org")" converted 'parallel sessions both finish'

#: Without a terminal the progress goes to the band.
: > "${t}/banners"
agent_view_progress=hs agent_view_progress_interval=0.05 \
    h-agent-view-transcript-run band-h 'run again' pt-fast-convert "${t_claude}" "${t}/out1/c.org"
expect-match "$(<"${t}/banners")" 'banner band-h: .*50%  read.*run again' 'hs band shows progress'

# --- A large synthetic transcript through the real renderer.
h-agent-session-dep
command python3 -I - "${t}/big.jsonl" <<'EOF'
import json, sys
sid = "00000000-0000-4000-8000-0000000000aa"
with open(sys.argv[1], "w") as f:
    for i in range(6000):
        body = "\n".join("line %d of output %d" % (k, i) for k in range(60))
        for r in ({"type": "user", "sessionId": sid, "timestamp": "2026-09-08T10:00:00.000Z",
                   "message": {"role": "user", "content": [{"type": "text", "text": "question %d" % i}]}},
                  {"type": "assistant", "sessionId": sid, "timestamp": "2026-09-08T10:00:01.000Z",
                   "message": {"role": "assistant", "content": [{"type": "text", "text": "answer %d\n\n```\n%s\n```" % (i, body)}]}}):
            f.write(json.dumps(r) + "\n")
EOF
function pt-render-md {
    agent_session claude render -format=md -progress-file="${agent_session_progress_file}" "${1}" > "${2}"
}
agent_view_progress=tty agent_view_progress_interval=0.02 \
    h-agent-view-transcript-run band-big hint pt-render-md "${t}/big.jsonl" "${t}/out1/big.md" 2> "${t}/big.err"
expect-match "$(<"${t}/big.err")" '[0-9]+%  (read|render)' 'renderer progress reaches the line'
expect-match "$(command head -c 200000 "${t}/out1/big.md")" 'answer 0' 'big transcript rendered'

# --- Remote viewer: fake SSH executes the real endpoint in a separate process
# tree so only closing the SSH stream can stop the remote worker.
export PT_ROOT="${pt_root}" PT_TMP="${t}"
typeset -g remote="${t}/remote" ssh_hosts="${HOME}/.config/ypaseo/transcript-hosts"
typeset -g remote_jsonl="${remote}/home/.claude/projects/example/${s_claude}.jsonl"
command mkdir -p -- "${remote_jsonl:h}" "${remote}/paseo/agents/example" \
    "${remote}/startup" "${remote}/tmp" "${t}/ssh-bin" "${ssh_hosts:h}"
print -r -- '  Label With Spaces  label-alias  # comment' > "${ssh_hosts}"
printf '%s\t%s' srv_example id-alias >> "${ssh_hosts}"
expect-eq "$(h-paseo-transcript-ssh-alias 'Label With Spaces' '')" label-alias 'map a spaced label'
expect-eq "$(h-paseo-transcript-ssh-alias 'Label With Spaces' srv_example)" id-alias 'server-id entry wins'
expect-eq "$(h-paseo-transcript-ssh-alias EXAMPLE-HOST '')" example-host 'lowercase unmapped host'
print -r -- $'\nInvalid Alias -oProxyCommand' >> "${ssh_hosts}"
err="$(h-paseo-transcript-ssh-alias 'Invalid Alias' '' 2>&1)" && exit 1
expect-match "${err}" 'Configure a valid SSH alias' 'reject SSH options in map'
print -r -- srv_local > "${paseo_home}/server-id"
h-paseo-transcript-local-host 'Any label' srv_local
h-paseo-transcript-local-host srv_local ''
agent_view_progress=none PASEO_HOST=foreign paseo-view-transcript-raw --host srv_local "${a_claude}"
expect-eq "$(command tail -n 1 "${t}/opened")" "${t_claude}" 'local server id opens native file'

printf '{"id":"%s","provider":"claude","persistence":{"sessionId":"%s"}}\n' \
    "${a_claude}" "${s_claude}" > "${remote}/paseo/agents/example/${a_claude}.json"
print -r -- '{"type":"user","message":{"role":"user","content":"Synthetic remote prompt"}}' > "${remote_jsonl}"
cat > "${remote}/startup/.zshenv" <<'EOF'
setopt nobgnice
[[ "${PT_SSH_MODE:-}" == old ]] && return 0
HOME="${PT_TMP}/remote/home"
TMPDIR="${PT_TMP}/remote/tmp"
paseo_home="${PT_TMP}/remote/paseo"
agent_view_lock_dir="${PT_TMP}/remote/locks"
unset night_basic_plugin_loaded_p
source "${PT_ROOT}/zshlang/basic/basic.plugin.zsh"
function aliasfn { :; }
function aliasfnq { :; }
source "${PT_ROOT}/zshlang/auto-load/others/processes.zsh"
source "${PT_ROOT}/zshlang/auto-load/others/agent-session.zsh"
source "${PT_ROOT}/zshlang/auto-load/others/paseo.zsh"
unalias ecerr 2>/dev/null || true
function ecerr { print -ru2 -- "$@"; }
function h-agent-session-agent-of { print -r -- claude; }
function h-agent-session-call { [[ "${2}" == id-of ]] && print -r -- "${3:t:r}"; }
function h-agent-session-title { print -r -- '# Synthetic transcript'; }
function h-agent-session-dep { :; }
if [[ "${PT_SSH_MODE:-}" == slow ]] ; then
    function h-agent-session-render-document {
        sleep 30 &
        print -r -- "$!" > "${PT_TMP}/remote-grandchild"
        print -r -- '25 read 1 4' > "${agent_session_progress_file}"
        wait
    }
elif [[ "${PT_SSH_MODE:-}" == fail ]] ; then
    function h-agent-session-render-document {
        print -r -- 'partially rendered output'
        print -ru2 -- 'Synthetic renderer failure'
        return 2
    }
elif [[ "${PT_SSH_MODE:-}" == incomplete ]] ; then
    function h-agent-session-render-document {
        print -r -- 'partially rendered output'
        print -ru2 -- 'flag provided but not defined: -progress-file'
        return 0
    }
fi
EOF
cat > "${t}/ssh-bin/ssh" <<'EOF'
#!/usr/bin/env python3
import os
import subprocess
import sys

args = sys.argv[1:]
host, script = args[args.index("--") + 1:]
assert "-T" in args and "BatchMode=yes" in args and "ConnectTimeout=10" in args
tmp = os.environ["PT_TMP"]
with open(f"{tmp}/ssh-log", "a") as log:
    print(host, file=log)
if os.environ.get("PT_SSH_MODE") == "unreachable":
    sys.exit(255)

remote_in, client_out = os.pipe()
client_in, remote_out = os.pipe()
pid = os.fork()
if pid == 0:
    # Reparent the remote supervisor outside the client process tree.
    if os.fork() != 0:
        os._exit(0)
    os.close(client_out)
    os.close(client_in)
    env = {**os.environ, "ZDOTDIR": f"{tmp}/remote/startup"}
    result = subprocess.run(["/bin/sh", "-c", script], stdin=remote_in, env=env)
    os.write(remote_out, str(result.returncode).encode())
    os._exit(0)
os.close(remote_in)
os.close(remote_out)
os.waitpid(pid, 0)
status = os.read(client_in, 10)
sys.exit(int(status) if status else 255)
EOF
command chmod +x "${t}/ssh-bin/ssh"
typeset -gx PATH="${t}/ssh-bin:${PATH}"
typeset -gx agent_view_progress=none agent_view_progress_interval=0.05

paseo_transcript_server_id=srv_example paseo-view-transcript --host 'Label With Spaces' "${a_claude}"
expect-eq "$(command tail -n 1 "${t}/ssh-log")" id-alias 'server-id alias is the SSH destination'
file="$(command tail -n 1 "${t}/opened")"
expect-match "$(<"${file}")" 'Synthetic remote prompt' 'remote org output'
h-agent-view-rm-tmp "${file:h}"
paseo-view-transcript-md --host 'Label With Spaces' "${a_claude}"
file="$(command tail -n 1 "${t}/opened")"
expect-eq "${file:e}" md 'markdown opens a local markdown file'
expect-match "$(<"${file}")" 'Synthetic remote prompt' 'remote markdown output'
h-agent-view-rm-tmp "${file:h}"
paseo-view-transcript-raw --host EXAMPLE-HOST "${a_claude}"
file="$(command tail -n 1 "${t}/opened")"
expect-eq "$(<"${file}")" "$(<"${remote_jsonl}")" 'raw transcript is unchanged'
h-agent-view-rm-tmp "${file:h}"

calls="$(<"${t}/ssh-log")"
err="$(paseo-view-transcript --host 'example; printf sentinel' "${a_claude}" 2>&1)" && exit 1
expect-match "${err}" 'Invalid host label' 'reject host shell syntax'
err="$(paseo-view-transcript --host example-host 'aaaa; printf sentinel' 2>&1)" && exit 1
expect-match "${err}" 'Expected a Paseo agent id' 'reject remote id shell syntax'
expect-eq "$(<"${t}/ssh-log")" "${calls}" 'invalid arguments did not reach SSH'
err="$(PT_SSH_MODE=old paseo-view-transcript --host example-host "${a_claude}" 2>&1)" && exit 1
expect-match "${err}" 'Remote transcript renderer is missing' 'old remote error'
err="$(paseo-view-transcript --host example-host abcd 2>&1)" && exit 1
expect-match "${err}" 'no Paseo agent matches' 'missing agent error'
opened="$(<"${t}/opened")"
err="$(PT_SSH_MODE=fail paseo-view-transcript-md --host example-host "${a_claude}" 2>&1)" && exit 1
expect-match "${err}" 'Synthetic renderer failure' 'remote renderer error'
expect-eq "$(<"${t}/opened")" "${opened}" 'remote rendering errors open no partial output'
err="$(PT_SSH_MODE=incomplete paseo-view-transcript-md --host example-host "${a_claude}" 2>&1)" && exit 1
expect-match "${err}" 'Remote transcript renderer is too old' 'reject incomplete render with zero exit status'
expect-eq "$(<"${t}/opened")" "${opened}" 'incomplete remote renders open no partial output'

: > "${t}/banners"
PT_SSH_MODE=slow agent_view_progress=hs \
    paseo-view-transcript-md --host example-host "${a_claude}" 2> "${t}/remote-first.err" &
remote_run=$!
for i in {1..120} ; do
    command grep -q '25%  read' "${t}/banners" && break
    sleep 0.05
done
expect-match "$(<"${t}/banners")" 'Connecting to example-host' 'connecting band'
expect-match "$(<"${t}/banners")" '25%  read' 'remote progress reached the band'
remote_grandchild="$(<"${t}/remote-grandchild")"
# The replacement SSH cannot connect, so it cannot cancel the first request
# from the remote end. EOF must stop that isolated process tree on its own.
err="$(PT_SSH_MODE=unreachable paseo-view-transcript-md --host example-host "${a_claude}" 2>&1)" && exit 1
expect-match "${err}" 'Could not reach example-host over SSH' 'unreachable error'
wait "${remote_run}"
for i in {1..120} ; do kill -0 "${remote_grandchild}" 2>/dev/null || break; sleep 0.05; done
if kill -0 "${remote_grandchild}" 2>/dev/null ; then
    print -ru2 -- 'FAIL: SSH cancellation left the remote renderer child running'
    exit 1
fi

command rm -- "${t}/remote-grandchild"
PT_SSH_MODE=slow paseo-view-transcript-md --host example-host "${a_claude}" \
    2> "${t}/remote-alias-first.err" &
remote_run=$!
for i in {1..120} ; do test -s "${t}/remote-grandchild" && break; sleep 0.05; done
remote_grandchild="$(<"${t}/remote-grandchild")"
opened="$(<"${t}/opened")"
paseo-view-transcript-md --host 'Label With Spaces' "${a_claude[1,12]}"
wait "${remote_run}"
file="$(command tail -n 1 "${t}/opened")"
expect-eq "$(<"${t}/opened")" "${opened}"$'\n'"${file}" 'only the new remote conversion opens'
expect-match "$(<"${file}")" 'Synthetic remote prompt' 'restart by another alias and ID prefix'
h-agent-view-rm-tmp "${file:h}"
if kill -0 "${remote_grandchild}" 2>/dev/null ; then
    print -ru2 -- 'FAIL: remote native-session takeover left its renderer child running'
    exit 1
fi
unset agent_view_progress agent_view_progress_interval

# --- Deleting archived sessions
typeset -g d_claude=dddddddd-0000-4000-8000-000000000001 ds_claude=55555555-5555-4555-8555-555555555555
typeset -g d_codex=dddddddd-0000-4000-8000-000000000002 ds_codex=66666666-6666-4666-8666-666666666666
typeset -g d_pin=dddddddd-0000-4000-8000-000000000003 ds_pin=77777777-7777-4777-8777-777777777777
typeset -g d_parent=dddddddd-0000-4000-8000-000000000004 ds_parent=88888888-8888-4888-8888-888888888888
typeset -g d_shared=dddddddd-0000-4000-8000-000000000005
typeset -g archived='{"archivedAt":"2026-10-01T00:00:00Z"}'
pt-record "${d_claude}" handoff-claude-x "{\"sessionId\":\"${ds_claude}\"}" "${archived}"
pt-record "${d_codex}" handoff-codex-x "{\"metadata\":{\"threadId\":\"${ds_codex}\"}}" '{"archivedAt":"2026-09-01T00:00:00Z"}'
pt-record "${d_pin}" handoff-claude-x "{\"sessionId\":\"${ds_pin}\"}" '{"archivedAt":"2026-10-01T00:00:00Z","labels":{"keep":"true"}}'
pt-record "${d_parent}" handoff-claude-x "{\"sessionId\":\"${ds_parent}\"}" "${archived}"
pt-record eeeeeeee-0000-4000-8000-000000000006 claude '{"sessionId":"12121212-1212-4121-8121-121212121212"}' \
    "{\"labels\":{\"paseo.parent-agent-id\":\"${d_parent}\"}}"
#: Shares the live claude agent's native session: refused.
pt-record "${d_shared}" handoff-claude-x "{\"sessionId\":\"${s_claude}\"}" "${archived}"

typeset -g dt_claude="${t}/claude-alt/projects/-p/${ds_claude}.jsonl"
typeset -g dt_claude_sub="${t}/claude-alt/projects/-p/${ds_claude}/subagents/agent-1.jsonl"
typeset -g dt_codex="${t}/codex/sessions/2026/09/01/rollout-2026-09-01T00-00-00-${ds_codex}.jsonl"
typeset -g dt_codex_arch="${t}/codex/archived_sessions/rollout-2026-08-31T00-00-00-${ds_codex}.jsonl"
typeset -g dt_pin="${t}/claude-alt/projects/-p/${ds_pin}.jsonl"
typeset -g dt_parent="${t}/claude-alt/projects/-p/${ds_parent}.jsonl"
for f in "${dt_claude}" "${dt_claude_sub}" "${dt_codex}" "${dt_codex_arch}" "${dt_pin}" "${dt_parent}" ; do
    command mkdir -p -- "${f:h}" ; print -r -- x > "${f}"
done

command mkdir -p -- "${t}/bin"
print -r -- '#!/bin/sh
printf "%s\n" "$*" >> "'"${t}"'/paseo.log"' > "${t}/bin/paseo"
command chmod +x "${t}/bin/paseo"
path=( "${t}/bin" "${path[@]}" )

out="$(paseo-transcript-delete-fz --dry-run --filter '')"
expect-match "${out}" "${d_codex}.*\n.*${ds_codex}" 'dry run lists the codex rollouts'
expect-match "${out}" "${ds_claude}/?\$|${ds_claude}\n" 'dry run lists the claude subagent directory'
[[ "${out}" != *"${d_pin}"* && "${out}" != *"${d_parent}"* && "${out}" != *"${d_shared}"* ]] \
    || { print -ru2 -- "FAIL: refused sessions offered:\n${out}" ; exit 1 }
#: Oldest archive first.
expect-match "${out}" "^${d_codex}" 'oldest archive first'
test -e "${dt_claude}" && test -e "${dt_codex}" || { print -ru2 -- 'FAIL: dry run deleted' ; exit 1 }
test ! -e "${t}/paseo.log" || { print -ru2 -- 'FAIL: dry run called paseo' ; exit 1 }

paseo-transcript-delete-fz --yes --filter '' > /dev/null
for f in "${dt_claude}" "${dt_claude_sub:h:h}" "${dt_codex}" "${dt_codex_arch}" ; do
    test ! -e "${f}" || { print -ru2 -- "FAIL: not deleted: ${f}" ; exit 1 }
done
for f in "${dt_pin}" "${dt_parent}" "${t_claude}" "${t_codex}" ; do
    test -e "${f}" || { print -ru2 -- "FAIL: deleted a refused file: ${f}" ; exit 1 }
done
expect-eq "$(command sort "${t}/paseo.log")" \
    "agent delete ${d_claude} --home ${paseo_home}"$'\n'"agent delete ${d_codex} --home ${paseo_home}" 'paseo agent delete per id'

print -r -- 'paseo-transcript: all tests passed'
