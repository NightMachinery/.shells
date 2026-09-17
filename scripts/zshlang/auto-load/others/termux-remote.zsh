##
#: Remote control of a Termux phone over SSH: clock alarms, timers,
#: notifications and toasts.
#:
#: The alarms are owned by the phone's own clock app, so they ring through Doze
#: and silent mode and survive Termux being killed, the SSH connection dropping
#: and this machine sleeping. The phone must be reachable when the alarm is
#: *set*, never when it rings.
#:
#: See =./docs/remote-termux-alarms.md= for the failure modes, which are not
#: obvious: =am= reports success even when nothing happened.
##
function h-termux-host-assert {
    : "usage: h-termux-host-assert HOST

Reject hosts that SSH could read as options or split into extra arguments."

    if (( $# != 1 )) || [[ -z "$1" || "$1" == -* || "$1" == *[[:space:]]* ]] ; then
        ecerr "${funcstack[2]:-$0}: exactly one non-empty host is required; it may not begin with '-' or contain whitespace"
        return 2
    fi
}

function h-termux-ssh {
    : "usage: h-termux-ssh HOST SCRIPT

Run SCRIPT, a single shell program, on a Termux HOST over SSH.

Uses [agfi:command] ssh so it bypasses our interactive kitty wrapper, allocates
no TTY, and is detached from local stdin so it cannot eat an enclosing
pipeline's data."

    ensure-cmd ssh @RET
    h-termux-host-assert "${1}" @RET

    local host="$1" script="${2}"
    if [[ -z "$script" ]] ; then
        ecerr "$0: empty remote script"
        return 2
    fi

    command ssh -n -T "$host" "$script"
}
##
#: The remote programs are stored whole and quoted once, rather than assembled
#: by interpolation, so that user-supplied text can never reach the remote shell
#: as code. Every value crosses as an environment variable instead.
##
typeset -g h_termux_alarm_script='
tz="$alarm_tz"
[ -n "$tz" ] || tz="$(getprop persist.sys.timezone 2>/dev/null)"
[ -n "$tz" ] || tz=UTC

#: %-H and %-M drop the zero padding on purpose: am parses --ei values as Java
#: integers, so a padded 08 is read as octal and rejected with
#: For input string: 8 under radix 8.
hm="$(TZ="$tz" date -d "@$alarm_epoch" +"%-H %-M" 2>/dev/null)" || {
    printf "%s\n" "termux-alarm: could not render epoch: $alarm_epoch" >&2
    exit 2
}
h="${hm% *}" ; m="${hm#* }"

set --
[ -n "$alarm_component" ] && set -- "$@" -n "$alarm_component"
set -- "$@" -a android.intent.action.SET_ALARM \
    --ei android.intent.extra.alarm.HOUR "$h" \
    --ei android.intent.extra.alarm.MINUTES "$m" \
    --ez android.intent.extra.alarm.SKIP_UI "$alarm_skip_ui" \
    --ez android.intent.extra.alarm.VIBRATE "$alarm_vibrate"
[ -n "$alarm_msg" ] && set -- "$@" --es android.intent.extra.alarm.MESSAGE "$alarm_msg"

out="$(am start "$@" 2>&1)" ; rc=$?
if [ "$rc" -ne 0 ] ; then
    printf "%s\n" "$out" >&2
    exit "$rc"
fi
printf "%02d:%02d %s\n" "$h" "$m" "$tz"
'

typeset -g h_termux_timer_script='
set --
[ -n "$timer_component" ] && set -- "$@" -n "$timer_component"
set -- "$@" -a android.intent.action.SET_TIMER \
    --ei android.intent.extra.alarm.LENGTH "$timer_seconds" \
    --ez android.intent.extra.alarm.SKIP_UI "$timer_skip_ui"
[ -n "$timer_msg" ] && set -- "$@" --es android.intent.extra.alarm.MESSAGE "$timer_msg"

out="$(am start "$@" 2>&1)" ; rc=$?
if [ "$rc" -ne 0 ] ; then
    printf "%s\n" "$out" >&2
    exit "$rc"
fi
printf "%ss\n" "$timer_seconds"
'
##
function h-termux-alarm-set {
    : "usage: h-termux-alarm-set HOST TIMESPEC [MESSAGE...]

Set an alarm in HOST's clock app. TIMESPEC is anything [agfi:datenat-v2]
accepts: compact durations like 1h30m, 90m or '1h:30m later', and prose like
'7:30', 'tomorrow 8am', 'in 3 hours' or 'next friday 9am'.

The spec is parsed here and crosses the wire as a unix timestamp, which HOST
renders into a wall-clock hour and minute against Android's *system* timezone.
An epoch is unambiguous, so a disagreement between this machine's timezone and
the phone's cannot shift the alarm. Override the rendering zone with alarm_tz.

SET_ALARM carries only an hour and a minute, so Android schedules the next
occurrence; a time already past today rolls over to tomorrow by itself.

Set termux_alarm_component to pin the receiving activity, e.g.
'com.android.deskclock/com.android.deskclock.AlarmClock'. Left empty the intent
is implicit and follows the phone's default clock app."

    local host="${1}" spec="${2}" msg="${@[3,-1]}"
    if (( $# < 2 )) || [[ -z "$spec" ]] ; then
        ecerr "usage: $0 HOST TIMESPEC [MESSAGE...]"
        return 2
    fi

    local epoch
    epoch="$(datenat-v2 "$spec")" @RET

    local tz="${alarm_tz}"
    local component="${termux_alarm_component}"
    local skip_ui="${termux_alarm_skip_ui:-true}"
    local vibrate="${termux_alarm_vibrate:-true}"

    h-termux-ssh "$host" "env \
alarm_tz=$(gquote-sq "$tz") \
alarm_epoch=$(gquote-sq "$epoch") \
alarm_msg=$(gquote-sq "$msg") \
alarm_component=$(gquote-sq "$component") \
alarm_skip_ui=$(gquote-sq "$skip_ui") \
alarm_vibrate=$(gquote-sq "$vibrate") \
sh -c $(gquote-sq "$h_termux_alarm_script")"
}

function h-termux-timer-set {
    : "usage: h-termux-timer-set HOST DURATION [MESSAGE...]

Start a countdown timer in HOST's clock app. DURATION is anything
[agfi:dur2sec] accepts: a bare seconds count, or 90s, 30m, 2h, 3d."

    local host="${1}" dur="${2}" msg="${@[3,-1]}"
    if (( $# < 2 )) || [[ -z "$dur" ]] ; then
        ecerr "usage: $0 HOST DURATION [MESSAGE...]"
        return 2
    fi

    local seconds
    seconds="$(dur2sec "$dur")" @RET

    local component="${termux_alarm_component}"
    local skip_ui="${termux_alarm_skip_ui:-true}"

    h-termux-ssh "$host" "env \
timer_seconds=$(gquote-sq "$seconds") \
timer_msg=$(gquote-sq "$msg") \
timer_component=$(gquote-sq "$component") \
timer_skip_ui=$(gquote-sq "$skip_ui") \
sh -c $(gquote-sq "$h_termux_timer_script")"
}

function h-termux-notify {
    : "usage: h-termux-notify HOST TITLE [CONTENT...]

Post an Android notification on HOST via termux-api. Set termux_notify_id to
reuse (replace) an existing notification instead of stacking a new one.

This is not an alarm: notifications are batched by Doze and do not ring. Use
[agfi:h-termux-alarm-set] when the phone has to wake you."

    local host="${1}" title="${2}" content="${@[3,-1]}"
    if (( $# < 2 )) || [[ -z "$title" ]] ; then
        ecerr "usage: $0 HOST TITLE [CONTENT...]"
        return 2
    fi

    local id="${termux_notify_id}"
    local priority="${termux_notify_priority:-high}"

    local -a cmd
    cmd=(termux-notification --title "$title" --content "$content" --priority "$priority")
    if [[ -n "$id" ]] ; then
        cmd+=(--id "$id")
    fi

    h-termux-ssh "$host" "$(gquote-sq "$cmd[@]")"
}

function h-termux-toast {
    : "usage: h-termux-toast HOST MESSAGE...

Flash a transient toast on HOST's screen via termux-api."

    local host="${1}" msg="${@[2,-1]}"
    if (( $# < 2 )) || [[ -z "$msg" ]] ; then
        ecerr "usage: $0 HOST MESSAGE..."
        return 2
    fi

    h-termux-ssh "$host" "termux-toast -- $(gquote-sq "$msg")"
}
##
#: The clock app's API handler, pinned. This is the activity that declares the
#: SET_ALARM intent filter; it is MIUI's rename of AOSP's HandleApiCalls, and it
#: shows no UI.
#:
#: Pinned because an implicit intent silently does nothing whenever several apps
#: handle SET_ALARM and none is the default: Android raises an invisible chooser
#: and every signal still reports success. An explicit component cannot reach it,
#: and a renamed class fails loudly with exit 2 instead.
#:
#: Do NOT pin com.android.deskclock.AlarmClock: that is the launcher activity,
#: declares no filter, and silently discards the extras.
#:
#: Empty string restores implicit resolution, which is portable to other phones.
#: See ./docs/remote-termux-alarms.md
typeset -g tealy_alarm_component="${tealy_alarm_component-com.android.deskclock/com.android.deskclock.HandleSetAlarmActivity}"

function tealy-alarm {
    : "usage: tealy-alarm TIMESPEC [MESSAGE...]

Set an alarm on the phone. See [agfi:h-termux-alarm-set]."

    local termux_alarm_component="${termux_alarm_component-$tealy_alarm_component}"

    h-termux-alarm-set tealy "$@"
}

function tealy-timer {
    : "usage: tealy-timer DURATION [MESSAGE...]

Start a countdown timer on the phone. See [agfi:h-termux-timer-set]."

    local termux_alarm_component="${termux_alarm_component-$tealy_alarm_component}"

    h-termux-timer-set tealy "$@"
}

function tealy-notify {
    : "usage: tealy-notify TITLE [CONTENT...]

Post a notification on the phone. See [agfi:h-termux-notify]."

    h-termux-notify tealy "$@"
}

function tealy-toast {
    : "usage: tealy-toast MESSAGE...

Flash a toast on the phone. See [agfi:h-termux-toast]."

    h-termux-toast tealy "$@"
}
##
