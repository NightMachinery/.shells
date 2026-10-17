#!/usr/bin/env zsh
# Run in a fresh loaded shell. All transports and app builds are inert fixtures.
(
    setopt localoptions pipefail
    unsetopt errexit
    local fixture
    fixture="$(command mktemp -d "${TMPDIR:-/tmp}/notification-apps.XXXXXX")" || exit 1
    trap 'command rm -rf -- "$fixture"' EXIT
    local original_home="$HOME" test_bin="${fixture}/bin"
    command mkdir -p "$test_bin"
    local -x HOME="${fixture}/home" TEST_NOTIFY_LOG="${fixture}/calls"
    local profile app_file
    for profile in 'Agent Bells' 'Night Mail' ; do
        app_file="${HOME}/Applications/${profile}.app/Contents/MacOS/terminal-notifier"
        command mkdir -p "${app_file:h}"
        print -r -- '#!/bin/sh
printf "%s\\n" "$0" "$@" >> "$TEST_NOTIFY_LOG"' > "$app_file"
        command chmod +x "$app_file"
    done
    function isServer { return 1; }
    function isDarwin { return 0; }
    function reval-timeout { shift; "$@"; }
    function check {
        [[ "$2" == "$3" ]] || {
            print -ru2 -- "FAIL: $1; expected ${(qqq)3}, got ${(qqq)2}"
            exit 1
        }
    }
    local notif_ignore_dnd_p=n notif_group=agent-session notif_image=''
    notif_app=agents notif-os '[title]' '$(printf inert)' || exit 1
    local log="$(<"$TEST_NOTIFY_LOG")"
    [[ "$log" == *'Agent Bells.app/Contents/MacOS/terminal-notifier'* ]] || exit 1
    [[ "$log" != *-ignoreDnD* ]] || exit 1
    [[ "$log" == *'\[title]'* && "$log" == *'\$(printf inert)'* ]] || exit 1
    command rm "$TEST_NOTIFY_LOG"
    notif_app=agents notif_ignore_dnd_p=y notif-os title message || exit 1
    [[ "$(<"$TEST_NOTIFY_LOG")" == *-ignoreDnD* ]] || exit 1
    command rm "$TEST_NOTIFY_LOG"
    notif-os-dismiss-agent || exit 1
    check agent-only-dismiss "$(<"$TEST_NOTIFY_LOG")" "${HOME}/Applications/Agent Bells.app/Contents/MacOS/terminal-notifier"$'\n-remove\nALL'
    command rm "$TEST_NOTIFY_LOG"
    notif_app=mail notif-os-remove mail-session || exit 1
    check mail-only-dismiss "$(<"$TEST_NOTIFY_LOG")" "${HOME}/Applications/Night Mail.app/Contents/MacOS/terminal-notifier"$'\n-remove\nmail-session'
    command rm "$TEST_NOTIFY_LOG"
    function notif-app-install { print -r -- unexpected-build >> "$TEST_NOTIFY_LOG"; return 1; }
    command rm -f "${HOME}/Applications/Agent Bells.app/Contents/MacOS/terminal-notifier"
    notif-os-dismiss-agent || exit 1
    [[ ! -e "$TEST_NOTIFY_LOG" ]] || { print -ru2 -- 'FAIL: dismissal installed a helper'; exit 1; }

    # Exercise the real agent hook through the bell ladder, keeping all other
    # transports inert. Verify routing identity and DND default reach notif.
    function awaysh { "$@"; }
    function app-icon-get { return 0; }
    function isLocal { return 0; }
    function bella_zsh_disable1 { return 0; }
    function h-bell-agent-role { ec main; }
    function oneinstance-setup { ec fixture; }
    function h-bell-claude { return 0; }
    function notif { print -r -- "${notif_app}:${notif_ignore_dnd_p}" > "$TEST_NOTIFY_LOG"; }
    local bell_auto_stop_mode=bell+notif bell_auto_tlg=n bell_auto_notif_alert=n
    local bell_auto_exit=() bell_skip_first='' bell_auto_sf=''
    unset notif_ignore_dnd_p
    bell-claude 'fixture' </dev/null >/dev/null || exit 1
    check hook-dnd-default "$(<"$TEST_NOTIFY_LOG")" agents:n
    notif_ignore_dnd_p=y bell-claude 'fixture' </dev/null >/dev/null || exit 1
    check hook-dnd-opt-in "$(<"$TEST_NOTIFY_LOG")" agents:y
    function notif-os-remove { print -r -- "${notif_app}:$1" > "$TEST_NOTIFY_LOG"; }
    function h-bell-notif-remove { return 0; }
    function h-bell-tlg-retract { return 0; }
    bell-claude-ack '{"session_id":"fixture","cwd":"/tmp/test"}' </dev/null || exit 1
    check hook-ack-identity "$(<"$TEST_NOTIFY_LOG")" agents:agent-Claude-fixture
    print -r -- 'notification-apps: all checks passed'
)
