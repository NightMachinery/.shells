# Remote Termux alarms

`tealy-alarm TIMESPEC [MESSAGE...]` sets an alarm in the phone's own clock app
over SSH:

```zsh
tealy-alarm 7:30 'wake up'
tealy-alarm 'tomorrow 8am' 'standup'
tealy-alarm '+20 minutes' 'tea'
```

The alarm belongs to the clock app, not to Termux. It therefore rings through
Doze and silent mode, and it survives Termux being killed, the SSH connection
dropping, and this machine going to sleep. The phone has to be reachable when
the alarm is *set*; it does not have to be reachable when the alarm rings. This
is why the implementation does not use `termux-notification` or
`termux-job-scheduler`: both are batched by Doze and inexact, and
`termux-job-scheduler` has a documented minimum period of fifteen minutes.

The generic helper is `h-termux-alarm-set HOST TIMESPEC [MESSAGE...]`. The
`tealy-` functions are thin wrappers over the `tealy` SSH alias, in the same
shape as `tealy-paste` and `h-paste-from-remote-termux` in
`./docs/remote-termux-clipboard.md`. Configure hostnames, users, ports and keys
in `~/.ssh/config` rather than in these functions.

## Time specifications

`TIMESPEC` is anything GNU `date -d` accepts, so `7:30`, `23:45`,
`tomorrow 8am` and `+20 minutes` all work. The phrasing `in 3 hours` is **not**
accepted; write `+3 hours` instead.

Android's `SET_ALARM` intent carries only an hour and a minute, so the system
schedules the next occurrence of that time. A time that has already passed today
rolls over to tomorrow on its own, and the code needs no logic for it.

Repeating alarms would use the `android.intent.extra.alarm.DAYS` extra, which is
an `ArrayList<Integer>` and would need `am`'s `--eial`. This is untested and not
implemented.

## Timezones

The hour and minute are resolved **on the phone**, against Android's system
timezone as reported by `getprop persist.sys.timezone`. Neither shell's `TZ`
affects the result, and it costs no extra round trip because the resolution
happens inside the SSH command that sets the alarm. Set `alarm_tz` to interpret
the spec in some other zone.

This matters because the two clocks can disagree. `sshd` on the phone inherits
its environment from whichever shell started it, and it was running with a `TZ`
ninety minutes away from the system zone, so a naive `date` in an SSH session
reported one time while the clock app scheduled alarms in another. The phone's
`~/.zshenv` now exports `TZ` from the system property, which fixes interactive
sessions too, but the helpers do not rely on that.

## When an alarm silently does not appear

This is the failure mode worth knowing, because nothing reports it.

If more than one installed app handles `SET_ALARM` and none is set as the
default, Android launches the disambiguation chooser. The chooser is an
activity, so `ActivityManager` returns `START_SUCCESS` and `am` prints
`Starting: Intent { act=android.intent.action.SET_ALARM (has extras) }` and
exits zero. But the chooser knows nothing about `SKIP_UI`, and because Termux is
in the background the dialog is invisible. The alarm is simply never created,
and every observable signal says it worked.

The fix is to set a default clock app: run the intent once without `SKIP_UI`
with the phone unlocked and in front of you, and the chooser appears and lets
you choose a default.

Set `termux_alarm_component` to pin the receiving activity and bypass resolution
entirely:

```zsh
termux_alarm_component='com.android.deskclock/com.android.deskclock.AlarmClock' \
    tealy-alarm 7:30 'wake up'
```

A pinned component cannot hit a chooser, and if the class ever disappears the
call fails loudly with exit 2 and `Activity class ... does not exist`, rather
than failing silently. The tradeoff is that it stops following the default clock
app and has to be corrected by hand if the clock app changes. Note also that
pinning the main clock activity, rather than a dedicated no-display API handler,
means `am` reports `intent has been delivered to currently running top-most
instance` when the clock app is already open, and handling of that is up to the
app.

Exit codes are worth reading precisely. Exit 2 with `Activity class ... does not
exist` means a wrong or filtered component. Exit 1 with `unknown error code 102`
means Android blocked a background activity launch. Exit 0 means an activity
started, which is **not** the same as an alarm having been created.

## What is not supported

Alarms cannot be listed. Android exposes no read API; the only related intent is
`SHOW_ALARMS`, which merely opens the clock app's UI.

Alarms cannot be removed. `DISMISS_ALARM` was tested with the `android.label`,
`android.next`, `android.all` and `android.time` search modes, and with an
explicit component. Every one returns `START_SUCCESS` and removes nothing. The
clock app on the phone this was developed against is a vendor fork that reuses
the AOSP package name `com.android.deskclock` while omitting
`com.android.deskclock.HandleApiCalls`, the AOSP class that implements the
`AlarmClock` API contract, which is the likely reason. Since `am`'s exit code
cannot distinguish "dismissed" from "ignored", no dismiss function is provided:
it could only ever pretend to work. Delete alarms in the clock app.

`termux-notification-list` hangs unless notification access has been granted
separately, so nothing here depends on it.

## Timers, notifications and toasts

`tealy-timer DURATION [MESSAGE...]` starts a countdown timer through
`SET_TIMER`. `DURATION` is parsed by `dur2sec`, so a bare seconds count works,
as do `90s`, `30m`, `2h` and `3d`.

`tealy-notify TITLE [CONTENT...]` posts an Android notification, and
`tealy-toast MESSAGE...` flashes a transient toast. Both go through the
`termux-api` package and need the Termux:API app installed. Neither is an alarm:
notifications are subject to Doze batching and do not ring.

## Requirements

An SSH server on the Termux device, the `termux-api` package plus the Termux:API
add-on for the notification and toast functions, and Termux itself at version
0.118.1 or newer. That version added `com.android.alarm.permission.SET_ALARM` to
Termux's manifest; without it, `am start -a android.intent.action.SET_ALARM`
fails with a `SecurityException` naming that permission. F-Droid installs
predating mid-2024 are the usual cause.

Only the `app_process` variant of `am`, from the `termux-am` package, is used.
The socket variant `termux-am` from `termux-am-socket` fails with
`Could not connect to socket` unless the Termux app's socket server is running.

All user-supplied text crosses to the phone as environment variables rather than
being interpolated into the remote shell program, so a message containing
quotes, backticks or `$(...)` is inert. Host arguments that are empty, begin
with `-`, or contain whitespace are rejected, so they cannot be read as SSH
options or split into extra arguments.
