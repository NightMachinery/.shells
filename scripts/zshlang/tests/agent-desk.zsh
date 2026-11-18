#!/usr/bin/env zsh
# Run in a fresh loaded shell: zsh -c 'source "$NIGHTDIR/zshlang/tests/agent-desk.zsh"'.
# Every transport is an inert fixture; all values are made up.
(
    setopt localoptions pipefail
    unsetopt errexit
    local fixture
    fixture="$(command mktemp -d "${TMPDIR:-/tmp}/agent-desk-test.XXXXXX")" || exit 1
    trap 'command rm -rf -- "$fixture"' EXIT
    function check {
        [[ "$2" == "$3" ]] || {
            print -ru2 -- "FAIL: $1; expected ${(qqq)3}, got ${(qqq)2}"
            exit 1
        }
    }
    function notif {
        print -rl -- "${notif_app}" "${notif_group}" "${notif_title}" "${notif_body}" \
            "${notif_ignore_dnd_p}" "${notif_tlg}" "${notif_on_click[@]}" > "${fixture}/card"
    }
    function notif-os-remove { print -r -- "${notif_app}:$1" > "${fixture}/removed"; }
    #: Telegram, faked: sends print id 501; deletes fail when test_delete_fails is set.
    local test_delete_fails=''
    function tsend {
        print -r -- "${(j:|:)@}" >> "${fixture}/tsend"
        case "$1" in
            delete) test -z "${test_delete_fails}" ;;
            edit) ;;
            *) print -r -- 501 ;;
        esac
    }
    function tsend-calls {
        test -e "${fixture}/tsend" && command cat "${fixture}/tsend"
        command rm -f -- "${fixture}/tsend"
    }
    local away=n
    function h-agent-desk-away-p { bool "${away}"; }
    function payload {
        command jq --null-input --compact-output --arg kind "$1" --arg level "$2" --argjson local "$3" \
            --arg channel "${4-}" --arg handle "${5-}" --arg cause "${6-}" '
            { action: "notify", display_id: "q-7f3a", path: "example-proj/rerun-nov/coord",
              local: $local, agent_desk: "/opt/example/bin/agent-desk",
              channel: $channel, handle: $handle, dest: "example-chat",
              url: "https://desk.example.net/#/p/example-proj/rerun-nov/coord?item=q-7f3a",
              project: "example-proj", class: ($kind + "-" + $level), agent_title: "Rerun coordinator",
              event: { id: "01JEXAMPLE0000000000000001", kind: $kind, level: $level,
                       host: "host-b", title: "Which split\tfor the rerun?",
                       body: "Reuse the cache, or **regenerate** it?" } }
            + (if $cause == "" then {} else { cause: { kind: $cause } } end)'
    }

    local handle
    handle="$(payload ask blocked true | agent-desk-notify)" || exit 1
    check desktop-handle "${handle}" agent-desk-01JEXAMPLE0000000000000001
    local -a card=("${(@f)$(<"${fixture}/card")}")
    check app "${card[1]}" desk
    check group "${card[2]}" agent-desk-01JEXAMPLE0000000000000001
    check title "${card[3]}" 'Which split for the rerun?'
    check body "${card[4]}" 'example-proj/rerun-nov/coord · blocked'
    check blocked-respects-dnd "${card[5]}" n
    check no-tlg-through-notif "${card[6]}" n
    check click "${card[7,-1]}" '/opt/example/bin/agent-desk open q-7f3a'
    check desktop-sends-no-telegram "$(tsend-calls)" ''

    payload ask blocked false | agent-desk-notify >/dev/null || exit 1
    card=("${(@f)$(<"${fixture}/card")}")
    check remote-body "${card[4]}" 'example-proj/rerun-nov/coord · blocked · @host-b'

    payload tell notify true | agent-desk-notify >/dev/null || exit 1
    card=("${(@f)$(<"${fixture}/card")}")
    check notice-body "${card[4]}" 'example-proj/rerun-nov/coord · notice'
    check notice-respects-dnd "${card[5]}" n

    # The remote channel: one rich Telegram message, its handle the dest and id.
    handle="$(payload ask blocked true remote | agent-desk-notify)" || exit 1
    check remote-handle "${handle}" $'example-chat\t501'
    local sent
    sent="$(tsend-calls)"
    [[ "$sent" == '--print-ids|--parse-mode=rich|--|example-chat|**Which split for the rerun?**'$'\n''example-proj/rerun-nov/coord · blocked question · Rerun coordinator · q-7f3a'* ]] ||
        { print -ru2 -- "FAIL: remote send: ${sent}"; exit 1; }
    [[ "$sent" == *'[Open on the desk](https://desk.example.net/#/p/example-proj/rerun-nov/coord?item=q-7f3a)'* &&
        "$sent" == *'<details><summary>Details</summary>'*'Reuse the cache, or **regenerate** it?'*'</details>' ]] ||
        { print -ru2 -- "FAIL: remote text: ${sent}"; exit 1; }

    # Taking back: a card by its handle, an old card by its id, a Telegram
    # message by deleting it, or past 48 hours by editing it down.
    payload ask blocked true '' agent-desk-01JEXAMPLE0000000000000001 | agent-desk-dismiss || exit 1
    check dismiss "$(<"${fixture}/removed")" desk:agent-desk-01JEXAMPLE0000000000000001
    command rm "${fixture}/removed"
    payload ask blocked true | agent-desk-dismiss || exit 1
    check dismiss-old "$(<"${fixture}/removed")" desk:agent-desk-01JEXAMPLE0000000000000001
    payload ask blocked true remote $'example-chat\t501' answer | agent-desk-dismiss || exit 1
    check dismiss-remote "$(tsend-calls)" 'delete|--|example-chat|501'
    test_delete_fails=y
    payload ask blocked true remote $'example-chat\t501' answer | agent-desk-dismiss || exit 1
    check dismiss-remote-old "$(tsend-calls)" $'delete|--|example-chat|501\nedit|--|example-chat|501|✓ answered: Which split for the rerun?'
    test_delete_fails=''

    away=y
    agent-desk-away-p || exit 1
    away=n
    agent-desk-away-p && exit 1

    # A done report opens the report itself.
    function h-agent-done-open-report { print -r -- "$1" > "${fixture}/opened"; }
    print -r -- '* fabricated report' > "${fixture}/report.org"
    command jq --null-input --compact-output --arg r "${fixture}/report.org" '
        { action: "open", display_id: "d-k2m9", path: "example-proj/rerun-nov/coord", local: true,
          event: { id: "01JEXAMPLE0000000000000002", kind: "done", meta: { report: $r } } }' |
        agent-desk-open || exit 1
    check open-report "$(<"${fixture}/opened")" "${fixture}/report.org"

    # agent-done's hooks, against a fake agent-desk that logs its calls.
    command mkdir -p "${fixture}/bin"
    print -r -- '#!/bin/sh
printf "%s|" "$@" >> "$FAKE_DESK_LOG"; printf "\n" >> "$FAKE_DESK_LOG"
case "$1" in
  show) printf "%s\n" "{\"needs_you\":[{\"items\":[{\"display_id\":\"q-7f3a\",\"event\":{\"level\":\"blocked\",\"title\":\"Which split?\"}},{\"display_id\":\"q-b2c4\",\"event\":{\"level\":\"default\",\"title\":\"Drop it?\"}}]}]}" ;;
  done) cat >> "$FAKE_DESK_LOG"; echo d-k2m9 ;;
  whoami) [ "$AI_AGENT:$CLAUDE_CODE_SESSION_ID$CODEX_THREAD_ID" = "claude-code:sess-on-node" ] || exit 1; echo example-proj/r1/coord ;;
esac' > "${fixture}/bin/agent-desk"
    command chmod +x "${fixture}/bin/agent-desk"
    local -x FAKE_DESK_LOG="${fixture}/desk-calls"
    (
        path=( "${fixture}/bin" "${path[@]}" )
        rehash
        check desk-open "$(h-agent-done-desk-open)" 'q-7f3a  Which split?'
        agent_done_summary='All six figures regenerated.' h-agent-done-desk-file 'Figures done' "${fixture}/report.org" >/dev/null || exit 1
        # The turn-end bell's test: on a node, and the desk notifies.
        local -x FAKE_DESK_LOG="${fixture}/desk-calls-whoami"
        print -r -- 'notify_command = "x"' > "${fixture}/notifies.toml"
        local -x AGENT_DESK_CONFIG="${fixture}/notifies.toml"
        h-agent-desk-on-node-p Claude sess-on-node || exit 1
        h-agent-desk-on-node-p Claude sess-elsewhere && exit 1
        h-agent-desk-on-node-p Codex sess-on-node && exit 1
        AGENT_DESK_CONFIG="${fixture}/none.toml" h-agent-desk-on-node-p Claude sess-on-node && exit 1
        # A url opens the dashboard page.
        print -r -- '#!/bin/sh
printf "%s\n" "$*" > "$FAKE_OPENED"' > "${fixture}/bin/open"
        command cp "${fixture}/bin/open" "${fixture}/bin/xdg-open"
        command chmod +x "${fixture}/bin/open" "${fixture}/bin/xdg-open"
        rehash
        local -x FAKE_OPENED="${fixture}/opened-url"
        # Never talk to a real browser: tab reuse is off, and a browser
        # outside the list is refused before any AppleScript runs.
        agent_desk_open_browser=com.example.none h-agent-desk-open-tab 'https://desk.example.net/#/' && exit 1
        local agent_desk_open_reuse_tab=n
        payload tell notify true | agent-desk-open || exit 1
        check open-url "$(<"${FAKE_OPENED}")" 'https://desk.example.net/#/p/example-proj/rerun-nov/coord?item=q-7f3a'
        true
    ) || exit 1
    check desk-file "$(command tail -n 2 "${FAKE_DESK_LOG}")" "done|--title|Figures done|--report|${fixture}/report.org|"$'\nAll six figures regenerated.'

    function h-agent-desk-base-url-guess { return 1; }
    XDG_CONFIG_HOME="${fixture}/config" agent-desk-config-install >/dev/null || exit 1
    [[ "$(<"${fixture}/config/agent-desk/config.toml")" == *"-c agent-desk-notify"* ]] || exit 1
    [[ "$(<"${fixture}/config/agent-desk/config.toml")" == *'remote_label = "Telegram"'* ]] || exit 1
    local -A agent_desk_ssh_hosts=( host-b host-b.example.net )
    XDG_CONFIG_HOME="${fixture}/config2" agent-desk-config-install --base-url 'https://desk.example.net/' \
        --ssh-host host-c=host-c.example.net --remote-label Phone >/dev/null || exit 1
    local cfg="$(<"${fixture}/config2/agent-desk/config.toml")"
    [[ "$cfg" == *'away_command = "'*'-c agent-desk-away-p"'* && "$cfg" == *'base_url = "https://desk.example.net/"'* &&
        "$cfg" == *'remote_label = "Phone"'* &&
        "$cfg" == *$'[ssh_hosts]\n"host-b" = "host-b.example.net"\n"host-c" = "host-c.example.net"'* ]] ||
        { print -ru2 -- "FAIL: config: ${cfg}"; exit 1; }
    unset agent_desk_ssh_hosts
    XDG_CONFIG_HOME="${fixture}/config" agent-desk-config-install 2>/dev/null && exit 1
    # A config naming a notifier is what lets agent-done skip its own card.
    AGENT_DESK_CONFIG="${fixture}/config/agent-desk/config.toml" h-agent-desk-notifies-p || exit 1
    AGENT_DESK_CONFIG="${fixture}/none.toml" h-agent-desk-notifies-p && exit 1
    print -r -- 'open_command = "x"' > "${fixture}/open-only.toml"
    AGENT_DESK_CONFIG="${fixture}/open-only.toml" h-agent-desk-notifies-p && exit 1
    print -r -- 'agent-desk: all checks passed'
)
