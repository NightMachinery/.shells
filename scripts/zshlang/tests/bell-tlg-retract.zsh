#!/usr/bin/env zsh
# Run: zsh -c 'source "$NIGHTDIR/zshlang/tests/bell-tlg-retract.zsh"'
# Also safe to source through BrishGarden after brishz-restart.
#
# Real redis, under scratch key names that are removed afterwards; Telegram is
# faked, so nothing is sent.
(
    setopt localoptions pipefail
    unsetopt errexit
    local fixture
    fixture="$(command mktemp -d "${TMPDIR:-/tmp}/bell-tlg-retract.XXXXXX")" || exit 1
    local key_root="bell_tlg_test_${$}_${RANDOM}"
    local bell_tlg_batch_key_prefix="${key_root}_batch"
    local bell_tlg_group_key_prefix="${key_root}_group"
    function h-test-keys {
        redism --scan --pattern "${key_root}_*"
    }
    function h-test-keys-del {
        local k
        for k in "${(@f)$(h-test-keys)}" ; do
            test -n "$k" && silent redism del "$k"
        done
    }
    function h-test-cleanup {
        h-test-keys-del
        command rm -rf -- "$fixture"
    }
    trap 'h-test-cleanup' EXIT

    function awaysh { "$@"; }
    function hostname { ec fixture-host; }
    function notif-os-remove { return 0; }
    function h-bell-notif-remove { return 0; }

    #: Sends "succeed" with increasing ids and log their text; edits and deletes are
    #: logged one per line as `edit <id> <text>' / `delete <ids>'. The counter is a
    #: file because tnotif runs inside a command substitution.
    print -r -- 100 > "${fixture}/next"
    function tnotif {
        print -r -- "$*" > "${fixture}/sent"
        test -n "${test_during_send}" && eval "$test_during_send"
        local id=$(( $(<"${fixture}/next") + 1 ))
        print -r -- "$id" > "${fixture}/next"
        print -r -- "$id"
    }
    function tsend {
        local cmd="$1"
        shift
        [[ "$1" == --parse-mode ]] && shift 2
        [[ "$1" == -- ]] && shift
        shift  #: dest
        case "$cmd" in
            edit)
                print -r -- "edit $1 ${2//$'\n'/|}" >> "${fixture}/calls"
                ;;
            delete)
                print -r -- "delete $*" >> "${fixture}/calls"
                test -z "${test_delete_fails}"
                ;;
        esac
    }

    function check {
        [[ "$2" == "$3" ]] || {
            print -ru2 -- "FAIL: $1; expected ${(qqq)3}, got ${(qqq)2}"
            exit 1
        }
    }
    function calls {
        test -e "${fixture}/calls" && command cat "${fixture}/calls"
        command rm -f -- "${fixture}/calls"
    }
    local test_during_send='' test_delete_fails=''

    #: Two sessions in one batch: the first answer edits its line out, repeating
    #: it changes nothing, and the last answer deletes the message.
    h-bell-tlg-send dest $'g1\tA' $'g2\tB' >/dev/null || exit 1
    check sent "$(<"${fixture}/sent")" $'A\nB\n(fixture-host)'
    h-bell-tlg-retract g1
    check first-answer "$(calls)" 'edit 101 B|(fixture-host)'
    h-bell-tlg-retract g1
    check repeat-answer "$(calls)" ''
    h-bell-tlg-retract g2
    check last-answer "$(calls)" 'delete 101'
    check keys-after-delete "$(h-test-keys)" ''

    #: The same text from two sessions is one line, so answering one of them
    #: leaves the message as it is.
    h-bell-tlg-send dest $'g1\tsame' $'g2\tsame' >/dev/null || exit 1
    h-bell-tlg-retract g1
    check shared-line "$(calls)" ''
    h-bell-tlg-retract g2
    check shared-line-last "$(calls)" 'delete 102'

    #: A line with no session behind it keeps the batch alive.
    h-bell-tlg-send dest $'\tU' $'g1\tA' >/dev/null || exit 1
    h-bell-tlg-retract g1
    check untagged "$(calls)" 'edit 103 U|(fixture-host)'
    h-test-keys-del

    #: An answer that lands while the batch is being sent is caught up once the
    #: ids are known.
    test_during_send='h-bell-tlg-retract g1'
    h-bell-tlg-send dest $'g1\tA' $'g2\tB' >/dev/null || exit 1
    test_during_send=''
    check mid-send "$(calls)" 'edit 104 B|(fixture-host)'
    h-bell-tlg-retract g2
    check mid-send-last "$(calls)" 'delete 104'

    #: A delete Telegram refuses (older than 48 hours) becomes a tombstone edit.
    h-bell-tlg-send dest $'g1\tA' >/dev/null || exit 1
    test_delete_fails=y
    h-bell-tlg-retract g1 2>/dev/null
    test_delete_fails=''
    check tombstone "$(calls)" $'delete 105\nedit 105 ✓ answered'
    check keys-after-tombstone "$(h-test-keys)" ''

    #: A send that yields no ids leaves nothing behind.
    function tnotif { return 0; }
    h-bell-tlg-send dest $'g1\tA' >/dev/null || exit 1
    check keys-without-ids "$(h-test-keys)" ''

    #: End to end through the agent's ack, which derives the same group as the bell.
    function tnotif { print -r -- 200; }
    local payload='{"session_id":"s1","cwd":"/tmp/proj"}'
    h-bell-tlg-send dest "$(h-bell-agent-group Claude /tmp/proj s1)"$'\tA' >/dev/null || exit 1
    bell-claude-ack "$payload" </dev/null
    check ack "$(calls)" 'delete 200'
    check keys-after-ack "$(h-test-keys)" ''

    print -r -- "bell-tlg-retract: ok"
)
