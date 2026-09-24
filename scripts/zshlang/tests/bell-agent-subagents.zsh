#!/usr/bin/env zsh
# Run: zsh -c 'source "$NIGHTDIR/zshlang/tests/bell-agent-subagents.zsh"'
# Inert: every transport is stubbed and tmux is a fake on PATH, so nothing rings,
# posts or touches a real tmux session. See the Subagents section of docs/bell-auto.md.
(
    setopt localoptions pipefail
    unsetopt errexit
    local fixture
    fixture="$(command mktemp -d "${TMPDIR:-/tmp}/bell-agent-subagents.XXXXXX")" || exit 1
    trap 'command rm -rf -- "$fixture"' EXIT
    local -x CODEX_HOME="${fixture}/codex"
    command mkdir -p "$CODEX_HOME" "${fixture}/bin"

    #: The fake tmux answers the two queries the role lookup makes, from fixture files.
    print -r -- '#!/bin/sh
case "$1" in
    display-message) cat "'"${fixture}"'/display" 2>/dev/null ;;
    list-sessions) cat "'"${fixture}"'/sessions" 2>/dev/null ;;
    *) exit 1 ;;
esac' > "${fixture}/bin/tmux"
    command chmod +x "${fixture}/bin/tmux"
    path=("${fixture}/bin" $path)

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
    function h-bell-notif-enqueue { print -r -- "$1" > "${fixture}/queue"; }
    function h-bell-notif-since { ec 1; }
    function h-bell-notif-drain { command cat "${fixture}/queue"; }
    function notif-os-remove { print -r -- "$1" > "${fixture}/desktop-ack"; }
    function h-bell-notif-remove { return 0; }
    function h-bell-agent-name { return 1; }
    function h-bell-claude { print -r -- Claude > "${fixture}/sound"; }
    function h-bell-codex { print -r -- Codex > "${fixture}/sound"; }
    local bell_auto_stop_mode=bell+notif bell_auto_tlg=y bell_auto_tlg_t=0 bell_auto_tlg_dest=fixture-dest
    local bell_auto_notif_alert=n bell_skip_first='' bell_auto_sf='' bell_auto_exit=()
    local agent_bell_subagents=''
    local agent_bell_subagents_file="${fixture}/mode"

    local failed=0
    function check {
        if [[ "$2" != "$3" ]] ; then
            print -ru2 -- "FAIL: $1; expected ${(qqq)3}, got ${(qqq)2}"
            failed=1
        fi
    }
    function sessions { print -r -- "$1" > "${fixture}/display"; print -r -- "$2" > "${fixture}/sessions"; }
    function outputs {
        local f out=()
        for f in sound desktop telegram ; do
            test -e "${fixture}/$f" && out+=("$f")
        done
        ec "${out[*]:-silent}"
    }
    function fire {
        command rm -f -- "${fixture}"/(sound|desktop|telegram|queue)(N)
        "$@" </dev/null >/dev/null 2>&1
        outputs
    }

    local id=11111111-2222-3333-4444-555555555555
    local payload='{"session_id":"'"$id"'","cwd":"/work/proj"}'
    local codex_payload='{"thread-id":"'"$id"'","cwd":"/work/proj"}'
    local T=$'\t'

    #: Role resolution, first hit wins.
    sessions "plain${T}" ''
    check "plain session" "$(h-bell-agent-role %1 '' "$id")" main
    check "forwarded node" "$(h-bell-agent-role %1 1 "$id")" sub
    sessions "ag--child--x${T}" ''
    check "ag-- name" "$(h-bell-agent-role %1 '' "$id")" sub
    sessions "ag--child--x${T}main" ''
    check "explicit main beats ag--" "$(h-bell-agent-role %1 1 "$id")" main
    sessions "plain${T}sub" ''
    check "explicit sub" "$(h-bell-agent-role %1 '' "$id")" sub
    sessions '' "other${T}${T}claude${T}99999999-2222-3333-4444-555555555555${T}/t
ag--kid${T}${T}claude${T}${id}${T}/t"
    check "no pane: found by identity" "$(h-bell-agent-role '' '' "$id")" sub
    check "no pane, unknown id" "$(h-bell-agent-role '' '' 00000000-2222-3333-4444-555555555555)" main
    check "nothing known" "$(h-bell-agent-role '' '' '')" main

    #: The modes, through the real hook and escalation ladder.
    sessions "plain${T}" ''
    check "main, default" "$(fire bell-claude --pane=%1 --node= "$payload")" "sound desktop telegram"
    sessions "ag--kid${T}" ''
    check "sub, default never" "$(fire bell-claude --pane=%1 --node= "$payload")" silent
    print -r -- os-only > "$agent_bell_subagents_file"
    check "sub, os-only" "$(fire bell-claude --pane=%1 "$payload")" "sound desktop"
    print -r -- normal > "$agent_bell_subagents_file"
    check "sub, normal" "$(fire bell-claude --pane=%1 "$payload")" "sound desktop telegram"
    check "sub, env beats file" "$(agent_bell_subagents=never fire bell-claude --pane=%1 "$payload")" silent
    print -r -- bogus > "$agent_bell_subagents_file"
    check "sub, unknown value is never" "$(fire bell-claude --pane=%1 "$payload")" silent
    check "unknown value warns" "$(h-agent-bell-subagents-mode 2>&1 >/dev/null)" \
        'h-agent-bell-subagents-mode: unknown subagent bell mode "bogus"; treating it as never'
    command rm -f -- "$agent_bell_subagents_file"
    check "payload on stdin" "$(command rm -f -- "${fixture}"/(sound|desktop|telegram)(N); ec "$payload" | bell-claude --pane=%1 >/dev/null 2>&1; outputs)" silent

    #: Codex: the payload arrives as the last argv word, after the forwarded ones.
    sessions "plain${T}" ''
    check "codex main" "$(fire h-codex-notify --pane=%1 --node= "$codex_payload")" "sound desktop telegram"
    check "codex forwarded node" "$(fire h-codex-notify --pane=%1 --node=1 "$codex_payload")" silent
    sessions '' "ag--kid${T}${T}codex${T}${id}${T}"
    check "codex, old notify line (no words)" "$(fire h-codex-notify "$codex_payload")" silent
    print -r -- os-only > "$agent_bell_subagents_file"
    check "codex sub, os-only" "$(fire h-codex-notify --node=1 "$codex_payload")" "sound desktop"
    command rm -f -- "$agent_bell_subagents_file"

    #: The ack still reads the payload, not a forwarded word.
    command rm -f -- "${fixture}/desktop-ack"
    bell-claude-ack "$payload" </dev/null >/dev/null 2>&1
    check "ack group" "$(<"${fixture}/desktop-ack")" "agent-Claude-${id}"

    #: The setter validates and reports.
    check "setter" "$(agent-bell-subagents os-only 2>&1)" "subagent bells: os-only"
    check "setter refuses junk" "$(agent-bell-subagents loud >/dev/null 2>&1; print $?)" 1
    check "setter unset" "$(agent-bell-subagents unset 2>&1)" "subagent bells: never"

    (( failed )) && exit 1
    print -r -- "bell-agent-subagents: all checks passed"
)
