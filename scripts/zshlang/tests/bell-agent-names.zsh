#!/usr/bin/env zsh
# Run: zsh -ic 'source "$NIGHTDIR/zshlang/tests/bell-agent-names.zsh"'
# Also safe to source through BrishGarden after brishz-restart.
(
    setopt localoptions pipefail
    unsetopt errexit
    local fixture
    fixture="$(command mktemp -d "${TMPDIR:-/tmp}/bell-agent-names.XXXXXX")" || exit 1
    trap 'command rm -rf -- "$fixture"' EXIT
    local original_search_path="$PATH"
    whence -p agent_session >/dev/null || exit 1
    zmodload zsh/datetime || exit 1
    local -x CODEX_HOME="${fixture}/codex"
    command mkdir -p "$CODEX_HOME" "${fixture}/other" "${fixture}/bin"

    # Keep the real bell-auto, escalation ladder, timeout, and ack entry points.
    # Stub every transport and queue operation; no real notifications are sent.
    function awaysh { "$@"; }
    function app-icon-get { return 0; }
    function isLocal { return 0; }
    function bella_zsh_disable1 { return 0; }
    function oneinstance-setup { ec fixture; }
    function oneinstance { return 0; }
    function idle-get { ec 100000; }
    function hostname { ec fixture-host; }
    function notif { print -r -- "$1" > "${fixture}/desktop"; }
    function tnotif { print -r -- "$1" > "${fixture}/telegram"; }
    function h-bell-notif-enqueue {
        print -r -- "$1" > "${fixture}/queue"
        print -r -- "$2" > "${fixture}/group"
    }
    function h-bell-notif-since { ec 1; }
    function h-bell-notif-drain { command cat "${fixture}/queue"; }
    function notif-os-remove { print -r -- "$1" > "${fixture}/desktop-ack"; }
    function h-bell-notif-remove { print -r -- "$1" > "${fixture}/queue-ack"; }
    function h-bell-claude { print -r -- Claude > "${fixture}/sound"; }
    function h-bell-codex { print -r -- Codex > "${fixture}/sound"; }
    local bell_auto_stop_mode=bell+notif bell_auto_tlg=y bell_auto_tlg_t=0
    local bell_auto_notif_alert=n bell_skip_first='' bell_auto_sf='' bell_auto_exit=()

    function check {
        [[ "$2" == "$3" ]] || {
            print -ru2 -- "FAIL: $1; expected ${(qqq)3}, got ${(qqq)2}"
            exit 1
        }
    }
    function expect-hook {
        local app="$1" payload="$2" expected="$3" group="$4"
        local hook="bell-${app:l}" ack="bell-${app:l}-ack"
        "$hook" "$payload" </dev/null >/dev/null || exit 1
        check desktop "$(<"${fixture}/desktop")" "$expected"
        check telegram "$(<"${fixture}/telegram")" "${expected}"$'\n(fixture-host)'
        check group "$(<"${fixture}/group")" "$group"
        check sound "$(<"${fixture}/sound")" "$app"
        "$ack" "$payload" </dev/null || exit 1
        check desktop-ack "$(<"${fixture}/desktop-ack")" "$group"
        check queue-ack "$(<"${fixture}/queue-ack")" "$group"
    }
    function index-name {
        command jq --null-input --compact-output --arg id "$1" --arg name "$2" \
            '{id:$id,thread_name:$name}' >> "${CODEX_HOME}/session_index.jsonl"
    }

    local codex_a='{"thread-id":"one","cwd":"/work/scripts"}'
    local codex_b='{"thread-id":"two","cwd":"/work/scripts"}'
    index-name one 'Fix terminal titles'
    index-name two 'Review emoji 🎉'
    expect-hook Codex "$codex_a" 'Codex awaits! [scripts · Fix terminal titles]' agent-Codex-one
    expect-hook Codex "$codex_b" 'Codex awaits! [scripts · Review emoji 🎉]' agent-Codex-two
    index-name one 'Renamed: ready!'
    expect-hook Codex "$codex_a" 'Codex awaits! [scripts · Renamed: ready!]' agent-Codex-one
    index-name one ''
    expect-hook Codex "$codex_a" 'Codex awaits! [scripts]' agent-Codex-one
    local clear_record
    for clear_record in '{"id":"one","thread_name":null}' '{"id":"one"}' ; do
        index-name one stale
        print -r -- "$clear_record" >> "${CODEX_HOME}/session_index.jsonl"
        expect-hook Codex "$codex_a" 'Codex awaits! [scripts]' agent-Codex-one
    done
    local display='Fix $(touch NO) `false`; "quotes" & <tags> 👩‍💻'
    index-name one $' \n\t'"$display"$'\r\nnext\u2028line\a\u007f\u0080 '
    expect-hook Codex "$codex_a" "Codex awaits! [scripts · $display next line]" agent-Codex-one
    local long_name=''
    repeat 125 ; do long_name+='🎉'; done
    index-name one "$long_name"
    expect-hook Codex "$codex_a" "Codex awaits! [scripts · ${long_name[1,119]}…]" agent-Codex-one
    expect-hook Codex '{"thread_id":"two"}' 'Codex awaits! [Review emoji 🎉]' agent-Codex-two
    expect-hook Codex '{"session_id":"two","cwd":"/work/scripts"}' \
        'Codex awaits! [scripts · Review emoji 🎉]' agent-Codex-two
    expect-hook Codex '{"thread-id":"absent","cwd":"/work/scripts"}' \
        'Codex awaits! [scripts]' agent-Codex-absent
    CODEX_HOME="${fixture}/other" expect-hook Codex "$codex_b" \
        'Codex awaits! [scripts]' agent-Codex-two
    expect-hook Codex '{"cwd":"/work/scripts"}' 'Codex awaits! [scripts]' agent-Codex-scripts
    expect-hook Codex '{broken' 'Codex awaits!' agent-Codex
    expect-hook Codex '[]' 'Codex awaits!' agent-Codex
    expect-hook Codex '{} {}' 'Codex awaits!' agent-Codex
    expect-hook Codex '{"cwd":{},"message":[],"thread-id":42}' 'Codex awaits!' agent-Codex
    expect-hook Codex '{"cwd":"/first"} {"cwd":"/second"}' 'Codex awaits!' agent-Codex

    # Claude reads a real temporary transcript through the installed Go binary.
    local transcript="${fixture}/claude session.jsonl"
    print -r -- '{"type":"custom-title","customTitle":"Old title"}' > "$transcript"
    print -r -- '{"type":"custom-title","customTitle":"Fix terminal titles 🎉"}' >> "$transcript"
    local claude_stop claude_notify
    claude_stop="$(command jq --null-input --compact-output --arg t "$transcript" \
        '{session_id:"claude-one",cwd:"/work/scripts",transcript_path:$t}')"
    claude_notify="$(print -r -- "$claude_stop" | command jq --compact-output \
        '. + {message:"needs your permission"}')"
    expect-hook Claude "$claude_notify" \
        'Claude [scripts · Fix terminal titles 🎉]: needs your permission' agent-Claude-claude-one
    expect-hook Claude "$claude_stop" \
        'Claude awaits! [scripts · Fix terminal titles 🎉]' agent-Claude-claude-one
    print -r -- "$claude_stop" | bell-claude >/dev/null
    check stdin "$(<"${fixture}/desktop")" 'Claude awaits! [scripts · Fix terminal titles 🎉]'
    expect-hook Claude '{"session_id":"no-transcript","cwd":"/work/scripts"}' \
        'Claude awaits! [scripts]' agent-Claude-no-transcript
    command rm -- "$transcript"
    expect-hook Claude "$claude_stop" 'Claude awaits! [scripts]' agent-Claude-claude-one
    print -r -- 'malformed transcript' > "$transcript"
    expect-hook Claude "$claude_stop" 'Claude awaits! [scripts]' agent-Claude-claude-one
    print -r -- '{broken' >> "${CODEX_HOME}/session_index.jsonl"
    expect-hook Codex "$codex_b" 'Codex awaits! [scripts]' agent-Codex-two

    # Failure and a hung executable must discard any partial output.
    printf '%s\n' '#!/bin/sh' 'printf "partial name\n"' 'exit 1' > "${fixture}/bin/agent_session"
    command chmod +x "${fixture}/bin/agent_session"
    local -x PATH="${fixture}/bin:${original_search_path}"
    expect-hook Claude "$claude_stop" 'Claude awaits! [scripts]' agent-Claude-claude-one
    printf '%s\n' '#!/bin/sh' 'printf "partial name\n"' 'printf "%s\n" "$$" > "$BELL_NAME_TEST_PID"' 'exec /bin/sleep 20' > "${fixture}/bin/agent_session"
    local -x BELL_NAME_TEST_PID="${fixture}/lookup-pid"
    local began="$EPOCHREALTIME"
    expect-hook Claude "$claude_stop" 'Claude awaits! [scripts]' agent-Claude-claude-one
    (( EPOCHREALTIME - began < 4 )) || { print -ru2 -- 'FAIL: lookup timeout'; exit 1; }

    if command kill -0 "$(<"${fixture}/lookup-pid")" 2>/dev/null ; then
        print -ru2 -- 'FAIL: timed-out lookup survived'
        exit 1
    fi

    # Simulate failed executable discovery without hiding the transports' tools.
    function whence {
        [[ "$*" == '-p agent_session' ]] && return 1
        builtin whence "$@"
    }
    expect-hook Claude "$claude_stop" 'Claude awaits! [scripts]' agent-Claude-claude-one
    function whence {
        [[ "$*" == '-p jq' ]] && return 1
        builtin whence "$@"
    }
    expect-hook Codex "$codex_b" 'Codex awaits! [scripts]' agent-Codex-two
    # Missing gtimeout must still reach both stubbed transports.
    printf '%s\n' '#!/bin/sh' 'exit 127' > "${fixture}/bin/gtimeout"
    command chmod +x "${fixture}/bin/gtimeout"
    rehash
    unfunction whence
    expect-hook Claude "$claude_stop" 'Claude awaits! [scripts]' agent-Claude-claude-one
    printf '%s\n' '#!/bin/sh' 'exit 127' > "${fixture}/bin/jq"
    command chmod +x "${fixture}/bin/jq"
    rehash
    expect-hook Codex "$codex_b" 'Codex awaits!' agent-Codex
    print -r -- 'PASS: agent notification names, fallbacks, transports, and acknowledgement'
)
