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
    function tnotif { print -r -- "$*" > "${fixture}/telegram"; }
    function notif-os-remove { print -r -- "${notif_app}:$1" > "${fixture}/removed"; }
    local away=n
    function h-agent-desk-away-p { bool "${away}"; }
    function payload {
        command jq --null-input --compact-output --arg kind "$1" --arg level "$2" --argjson local "$3" '
            { action: "notify", display_id: "q-7f3a", path: "example-proj/rerun-nov/coord",
              local: $local, agent_desk: "/opt/example/bin/agent-desk",
              event: { id: "01JEXAMPLE0000000000000001", kind: $kind, level: $level,
                       host: "host-b", title: "Which split\tfor the rerun?" } }'
    }

    payload ask blocked true | agent-desk-notify || exit 1
    local -a card=("${(@f)$(<"${fixture}/card")}")
    check app "${card[1]}" desk
    check group "${card[2]}" agent-desk-01JEXAMPLE0000000000000001
    check title "${card[3]}" 'Which split for the rerun?'
    check body "${card[4]}" 'example-proj/rerun-nov/coord · blocked'
    check blocked-ignores-dnd "${card[5]}" y
    check no-tlg-through-notif "${card[6]}" n
    check click "${card[7,-1]}" '/opt/example/bin/agent-desk open q-7f3a'
    [[ ! -e "${fixture}/telegram" ]] || { print -ru2 -- 'FAIL: telegram while present'; exit 1; }

    away=y
    payload ask blocked true | agent-desk-notify || exit 1
    check telegram "$(<"${fixture}/telegram")" 'agent-desk q-7f3a: Which split for the rerun?'
    command rm "${fixture}/telegram"
    payload ask default true | agent-desk-notify || exit 1
    payload ask blocked false | agent-desk-notify || exit 1
    [[ ! -e "${fixture}/telegram" ]] || { print -ru2 -- 'FAIL: telegram for a default or remote question'; exit 1; }
    card=("${(@f)$(<"${fixture}/card")}")
    check remote-body "${card[4]}" 'example-proj/rerun-nov/coord · blocked · @host-b'

    payload tell notify true | agent-desk-notify || exit 1
    card=("${(@f)$(<"${fixture}/card")}")
    check notice-body "${card[4]}" 'example-proj/rerun-nov/coord · notice'
    check notice-respects-dnd "${card[5]}" n

    payload ask blocked true | agent-desk-dismiss || exit 1
    check dismiss "$(<"${fixture}/removed")" desk:agent-desk-01JEXAMPLE0000000000000001

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
esac' > "${fixture}/bin/agent-desk"
    command chmod +x "${fixture}/bin/agent-desk"
    local -x FAKE_DESK_LOG="${fixture}/desk-calls"
    (
        path=( "${fixture}/bin" "${path[@]}" )
        rehash
        check desk-open "$(h-agent-done-desk-open)" 'q-7f3a  Which split?'
        agent_done_summary='All six figures regenerated.' h-agent-done-desk-file 'Figures done' "${fixture}/report.org" >/dev/null || exit 1
    ) || exit 1
    check desk-file "$(command tail -n 2 "${FAKE_DESK_LOG}")" "done|--title|Figures done|--report|${fixture}/report.org|"$'\nAll six figures regenerated.'

    XDG_CONFIG_HOME="${fixture}/config" agent-desk-config-install >/dev/null || exit 1
    [[ "$(<"${fixture}/config/agent-desk/config.toml")" == *"-c agent-desk-notify"* ]] || exit 1
    XDG_CONFIG_HOME="${fixture}/config" agent-desk-config-install 2>/dev/null && exit 1
    # A config naming a notifier is what lets agent-done skip its own card.
    AGENT_DESK_CONFIG="${fixture}/config/agent-desk/config.toml" h-agent-desk-notifies-p || exit 1
    AGENT_DESK_CONFIG="${fixture}/none.toml" h-agent-desk-notifies-p && exit 1
    print -r -- 'open_command = "x"' > "${fixture}/open-only.toml"
    AGENT_DESK_CONFIG="${fixture}/open-only.toml" h-agent-desk-notifies-p && exit 1
    print -r -- 'agent-desk: all checks passed'
)
