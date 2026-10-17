#!/usr/bin/env zsh
# No real audio or notifications; covers startup and mid-loop headphones.
(
    setopt localoptions pipefail
    unsetopt errexit
    zmodload zsh/datetime || exit 1
    local fixture
    fixture="$(command mktemp -d "${TMPDIR:-/tmp}/bell-headphones.XXXXXX")" || exit 1
    trap 'command rm -rf -- "$fixture"' EXIT
    local worn=1 connect_on_ring=0 loops=0
    function isLocal { return 0; }
    function bella_zsh_disable1 { return 0; }
    function headphones-p { (( worn )); }
    function meeting-p { return 1; }
    function quiet-public-audio-p { return 1; }
    function oneinstance-setup { ec fixture; }
    function oneinstance { (( loops++ < 2 )); }
    function idle-get { ec 100000; }
    function sleep { return 0; }
    function h-bell-auto-notify { print -r -- notify >> "${fixture}/events"; }
    function test-ring {
        print -r -- ring >> "${fixture}/events"
        (( connect_on_ring )) && worn=1
        return 0
    }
    function test-cleanup { print -r -- cleanup >> "${fixture}/events"; }
    function expect {
        local label="$1" expected="$2"
        command rm -f -- "${fixture}/events"
        loops=0
        bell-auto test-ring >/dev/null || exit 1
        [[ "$(<"${fixture}/events")" == "$expected" ]] || {
            print -ru2 -- "FAIL: ${label}: $(<"${fixture}/events")"
            exit 1
        }
    }
    local bell_auto_stop_mode=auto bell_auto_exit=() bell_auto_sleep=0
    local bell_auto_t=30 bell_auto_max_t=3600 bell_auto_single='' bell_auto_sm=''
    local bell_skip_first='' bell_auto_sf='' bell_auto_notif_msg=fixture
    expect auto-headphones $'ring\nnotify'
    bell_auto_stop_mode=idle
    expect explicit-idle-headphones $'ring\nnotify'
    bell_auto_stop_mode=idle+timeout
    expect explicit-timeout-headphones $'ring\nnotify'
    worn=0 connect_on_ring=1
    expect connect-during-ring $'ring\nnotify'
    worn=0 connect_on_ring=0 bell_auto_stop_mode=idle
    expect speakers-still-repeat $'ring\nring\nring'
    worn=1 bell_auto_single=y bell_auto_exit=(test-cleanup)
    expect continuous-engine-cleanup $'ring\ncleanup'
    print -r -- 'bell-headphones: all checks passed'
)
