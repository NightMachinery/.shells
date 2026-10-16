#!/usr/bin/env zsh
# Run: zsh -c 'source "$NIGHTDIR/zshlang/tests/quiet-audio.zsh"'
# All audio, notifications, guard state, and device lookups are fixtures.
(
    setopt localoptions pipefail
    unsetopt errexit
    zmodload zsh/datetime || exit 1
    local fixture
    fixture="$(command mktemp -d "${TMPDIR:-/tmp}/quiet-audio.XXXXXX")" || exit 1
    trap 'command rm -rf -- "$fixture"' EXIT

    function check {
        [[ "$2" == "$3" ]] || {
            print -ru2 -- "FAIL: $1; expected ${(qqq)3}, got ${(qqq)2}"
            exit 1
        }
    }
    function expect-status {
        local label="$1" expected="$2" got=0
        shift 2
        "$@" >/dev/null || got=$?
        check "$label" "$got" "$expected"
    }

    # Exercise the real time conversion with a fixed clock, without changing
    # system time or adding test-only arguments to production functions.
    local test_epoch
    local -x TZ=Europe/Berlin
    function strftime {
        if [[ "$1" == -s && "$2" == hour && "$3" == '%H' ]] ; then
            builtin strftime -s hour '%H' "$test_epoch"
        else
            builtin strftime "$@"
        fi
    }
    function clock-set {
        builtin strftime -r -s test_epoch '%Y-%m-%d %H:%M:%S' "$1" || exit 1
    }
    local clock_case
    for clock_case in '21:59:59 1' '22:00:00 0' '00:00:00 0' '05:59:59 0' '06:00:00 1' '08:00:00 1' ; do
        clock-set "2026-10-16 ${clock_case%% *}"
        expect-status "boundary ${clock_case}" "${clock_case##* }" quiet-hours-p
    done
    # The same instant is daytime in Berlin and quiet time in Los Angeles.
    clock-set '2026-10-16 07:00:00'
    expect-status local-Berlin 1 quiet-hours-p
    TZ=America/Los_Angeles
    expect-status local-Los-Angeles 0 quiet-hours-p
    TZ=Europe/Berlin
    # Quiet hours include both occurrences of the repeated DST hour.
    local stamp
    for stamp in '2026-10-25 00:30:00' '2026-10-25 01:30:00' '2026-03-29 00:30:00' '2026-03-29 01:30:00' ; do
        TZ=UTC clock-set "$stamp"
        expect-status "DST ${stamp}" 0 quiet-hours-p
    done

    local at_office=0 test_idle=7200 test_muted=0 test_meeting=0
    local output_name='Built-in Speakers' output_transport=BuiltIn
    function office-p { (( at_office )); }
    function isDarwin { return 0; }
    function isLocal { return 0; }
    function audio-output-get {
        print -r -- lookup >> "${fixture}/lookups"
        print -rl -- "$output_name" "$output_transport"
    }
    function audio-output-get-system-profiler { audio-output-get; }
    function volume-mute-p { (( test_muted )); }
    function volume-mute { test_muted=1; }
    function idle-get { ec "$test_idle"; }
    function meeting-p { (( test_meeting )); }
    function bella_zsh_disable1 { return 0; }
    function oneinstance-setup { ec fixture; }
    function oneinstance { return 1; }
    function h-bell-auto-notify { print -r -- notify >> "${fixture}/events"; }
    function quiet-test-ring { print -r -- ring >> "${fixture}/events"; }
    function sleep { return 0; }

    local office night worn expected
    for office in 0 1 ; do
        at_office="$office"
        for night in 0 1 ; do
            if (( night )) ; then clock-set '2026-10-16 23:00:00'
            else clock-set '2026-10-16 12:00:00'; fi
            for worn in 0 1 ; do
                if (( worn )) ; then output_name=AirPods; output_transport=Bluetooth
                else output_name='Built-in Speakers'; output_transport=BuiltIn; fi
                expected=1
                (( (office || night) && ! worn )) && expected=0
                expect-status "policy ${office}/${night}/${worn}" "$expected" quiet-public-audio-p
                expect-status known-device "$expected" quiet-public-audio-p "$output_name" "$output_transport"
                expect-status office-only "$(( office && ! worn ? 0 : 1 ))" office-public-audio-p "$output_name" "$output_transport"
            done
        done
    done
    command rm -f -- "${fixture}/lookups"
    expect-status forwarded-speakers 0 quiet-public-audio-p 'Display Speakers' DisplayPort
    expect-status forwarded-headphones 1 quiet-public-audio-p 'External Headphones' BuiltIn
    [[ ! -e "${fixture}/lookups" ]] || { print -ru2 -- 'FAIL: supplied device caused lookup'; exit 1; }

    # Mode resolution and the real playback control flow, with inert engines.
    local bell_auto_stop_mode=auto bell_skip_first='' bell_auto_sf='' bell_auto_single=''
    local bell_auto_exit=() bell_auto_notif_msg=fixture bell_auto_sleep=0
    local bell_auto_t=30 bell_auto_max_t=0
    function expect-mode {
        local mode="$1" log
        command rm -f -- "${fixture}/events"
        log="$(bell-auto quiet-test-ring)" || exit 1
        [[ "$log" == *"started with stop_mode ${mode},"* ]] || {
            print -ru2 -- "FAIL: expected ${mode}: ${log}"; exit 1
        }
        local events="$(<"${fixture}/events")"
        if [[ "$mode" == notif ]] ; then check meeting-events "$events" notify
        elif [[ "$mode" == bell+notif ]] ; then check single-ring "$events" $'ring\nnotify'
        else check private-ring "$events" ring; fi
    }
    at_office=0 output_name='Built-in Speakers' output_transport=BuiltIn
    clock-set '2026-10-16 23:00:00'
    expect-mode bell+notif
    test_meeting=1
    expect-mode notif
    test_meeting=0
    clock-set '2026-10-16 12:00:00'
    expect-mode idle+timeout
    at_office=1
    expect-mode bell+notif
    at_office=0 output_name=AirPods output_transport=Bluetooth
    expect-mode bell+notif

    # Volume escalation must stay disabled, even when loudidle was requested.
    function volume-get { ec 30; }
    function volume-set { print -r -- "$1" >> "${fixture}/volume"; }
    function sharif-vc-p { return 1; }
    function trapexits { return 0; }
    function trapexits-release { return 0; }
    function play { return 0; }
    function assert-dbg { reval "$@"; }
    local hear_loudidle=y
    clock-set '2026-10-16 23:00:00'
    output_name='Built-in Speakers' output_transport=BuiltIn
    hearinvisible-playfast fixture.wav || exit 1
    [[ ! -e "${fixture}/volume" ]] || { print -ru2 -- 'FAIL: night volume boost'; exit 1; }
    clock-set '2026-10-16 12:00:00'
    hearinvisible-playfast fixture.wav || exit 1
    check daytime-boost "$(<"${fixture}/volume")" $'100\n30'

    # Preserve defaults, snooze, pre-existing mute, and owned restoration.
    local snoozed='' test_held='' notified=''
    local audio_guard_restore_at_tick_p=n audio_guard_notif_p=y
    function redism {
        case "$*" in
            'get audio_guard_snooze') ec "$snoozed" ;;
            'setex audio_guard_snooze '*) snoozed=y ;;
            'del audio_guard_snooze') snoozed='' ;;
            *) print -ru2 -- "FAIL: unexpected redis access: $*"; exit 1 ;;
        esac
    }
    function audio_guard_trigger_idle_get { return 0; }
    function audio_guard_trigger_lock_get { return 0; }
    function audio_guard_trigger_headphones_get { return 0; }
    function audio_guard_muted_get { ec "$test_held"; }
    function audio_guard_muted_del { test_held=''; }
    function h-audio-guard-claim-set { test_held="$1"; }
    function h-audio-guard-unmute-device { test_muted=0; }
    function notif { notified="$1"; }

    expect-status default-idle 0 audio-guard-trigger-p idle
    expect-status default-lock 1 audio-guard-trigger-p lock
    expect-status default-headphones 1 audio-guard-trigger-p headphones
    clock-set '2026-10-16 23:00:00'
    expect-status eligible-night 0 h-audio-guard-act-p "$output_name" "$output_transport"
    audio-guard-snooze 1m >/dev/null || exit 1
    expect-status snoozed 1 h-audio-guard-act-p "$output_name" "$output_transport"
    audio-guard-unsnooze || exit 1
    test_muted=1
    expect-status manual-mute 1 h-audio-guard-act-p "$output_name" "$output_transport"
    audio-guard-restore >/dev/null || exit 1
    check manual-mute-not-restored "$test_muted" 1
    test_muted=0 test_idle=0
    audio-guard-on-lock >/dev/null || exit 1
    audio-guard-on-audio-change "$output_name" "$output_transport" >/dev/null || exit 1
    check disabled-triggers-not-muted "$test_muted" 0
    audio-guard-tick >/dev/null || exit 1
    check active-user-not-muted "$test_muted" 0
    test_idle=7200
    audio-guard-tick >/dev/null || exit 1
    check idle-night-muted "$test_muted" 1
    check owned-device "$test_held" "$output_name"
    [[ "$notified" == *'quiet hours (22:00-06:00)'* && "$notified" != *office* ]] || exit 1
    # Unlock restoration is still allowed during quiet hours.
    audio-guard-restore >/dev/null || exit 1
    check restored "$test_muted/$test_held" '0/'
    at_office=1
    check both-reasons "$(h-quiet-reasons)" 'office, quiet hours (22:00-06:00)'
    local report="$(quiet-status)"
    [[ "$report" == *'office: yes'* && "$report" == *'quiet hours: yes'* && "$report" == *'speaker restraint: active'* ]] || exit 1
    output_name=AirPods output_transport=Bluetooth
    report="$(quiet-status)"
    [[ "$report" == *'headphones: yes'* && "$report" == *'speaker restraint: inactive'* ]] || exit 1
    expect-status headphones-exempt 1 h-audio-guard-act-p "$output_name" "$output_transport"

    print -r -- 'PASS: quiet hours, timezones/DST, speaker policy, bells, volume, guard controls, and status'
)
