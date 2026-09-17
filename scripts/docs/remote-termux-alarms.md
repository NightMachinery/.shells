# Remote Termux alarms

`tealy-alarm TIMESPEC [MESSAGE...]` sets an alarm in the phone's own clock app
over SSH:

```zsh
tealy-alarm 7:30 'wake up'
tealy-alarm 'tomorrow 8am' 'standup'
tealy-alarm '+20 minutes' 'tea'
```

The alarm belongs to the clock app, not to Termux, so it rings through Doze and
silent mode and survives Termux being killed, the SSH connection dropping, and
this machine sleeping. The phone must be reachable when the alarm is *set*, not
when it rings. Verified end to end with the phone locked and idle. This is why
the implementation avoids `termux-notification` and `termux-job-scheduler`, both
of which are Doze-batched and inexact; the latter has a documented minimum
period of fifteen minutes.

The generic helper is `h-termux-alarm-set HOST TIMESPEC [MESSAGE...]`, and the
`tealy-` functions are thin wrappers over the `tealy` SSH alias, in the same
shape as `tealy-paste` and `h-paste-from-remote-termux` in
`./docs/remote-termux-clipboard.md`. Configure hostnames, users, ports and keys
in `~/.ssh/config` rather than in these functions.

## Time specifications

`TIMESPEC` is anything GNU `date -d` accepts. The phrasing `in 3 hours` is
**not** accepted; write `+3 hours`.

`SET_ALARM` carries only an hour and a minute, so Android schedules the next
occurrence and a time already past today rolls over to tomorrow by itself.

Repeating alarms would need the `android.intent.extra.alarm.DAYS` extra, an
`ArrayList<Integer>` passed with `am`'s `--eial`. Untested, not implemented.

## Timezones

The hour and minute are resolved **on the phone**, against
`getprop persist.sys.timezone`, inside the same SSH command that sets the alarm.
Neither shell's `TZ` affects the result and it costs no extra round trip. Set
`alarm_tz` to interpret the spec in another zone.

This matters because the two clocks can disagree. `sshd` there inherits its
environment from whichever shell started it, and was running ninety minutes off
the system zone, so a naive `date` over SSH reported one time while the clock app
scheduled in another. The phone's `~/.zshenv` now exports `TZ` from the system
property, which fixes interactive sessions too, but the helpers do not rely on
it.

## Exit 0 means nothing

This is the trap. `am` reports whether an *activity started*, never what the app
then did, so a completely failed alarm is indistinguishable from a working one.
Read exit codes precisely:

- Exit 2, `Activity class ... does not exist`: wrong or filtered component.
- Exit 1, `unknown error code 102`: Android blocked a background activity launch.
- Exit 0: an activity started. The alarm may or may not exist.

The concrete way this bites: if several apps handle `SET_ALARM` and none is the
default, Android launches the disambiguation chooser. The chooser is an activity,
so `am` prints `Starting: Intent {...}` and exits 0, but it knows nothing about
`SKIP_UI` and is invisible because Termux is in the background. No alarm is
created and every signal says success.

The `tealy-` functions therefore pin the clock app's API handler explicitly
through `tealy_alarm_component`. An explicit component cannot reach a chooser,
and a renamed class fails loudly with exit 2 rather than silently. Set the
variable to the empty string for implicit resolution, which follows the default
clock app and is portable to other phones.

### Finding the right component on another phone

Pin the activity that **declares the intent filter**, not merely one that exists:
a launcher activity accepts the launch, returns 0, and discards the extras.

```zsh
pkg install aapt
apk="$(pm path com.android.deskclock | sed 's/^package://')"
aapt dump xmltree "$apk" AndroidManifest.xml | grep -B40 SET_ALARM
```

Here the clock app is a vendor fork that keeps the AOSP package name
`com.android.deskclock` but renames AOSP's `HandleApiCalls` to
`HandleSetAlarmActivity`. Checking only the AOSP name yields `does not exist` and
invites the wrong conclusion that there is no API handler at all.

One more hint: `Activity not started, intent has been delivered to currently
running top-most instance` means the target was already foregrounded. A
no-display API handler never produces it, so seeing it suggests a wrong pin.

## Timers

`tealy-timer DURATION [MESSAGE...]` starts a countdown through `SET_TIMER`.
`DURATION` is parsed by `dur2sec`, so a bare seconds count works, as do `90s`,
`30m`, `2h` and `3d`.

Timers are **singleton**: setting one replaces any pending timer. There is no
way to cancel silently. `LENGTH 0` does nothing at all, and `LENGTH 1` displaces
the pending timer but then rings a second later, so it is a noisy workaround
rather than a cancel.

## What cannot be done

Alarms cannot be listed. Android exposes no read API, and `SHOW_ALARMS` only
opens the clock app's UI.

Alarms cannot be dismissed or deleted. Every search mode (`android.label`,
`android.next`, `android.all`, `android.time`) was tried against both the API
handler and the launcher activity, and via implicit resolution. All return
`START_SUCCESS` and remove nothing. The app's own bytecode says why:

```zsh
apk="$(pm path com.android.deskclock | sed 's/^package://')"
unzip -o -q -j "$apk" 'classes*.dex' -d dex
cat dex/*.dex | grep -a -c -- android.intent.extra.alarm.SEARCH_MODE   #: 0
```

`SEARCH_MODE` occurs zero times, as do all four of its values, while `SKIP_UI`
and `MESSAGE` occur once each. The app implements the set path and never reads
the extra saying *which* alarm to dismiss, so no argument combination can work,
even though its manifest advertises the `DISMISS_ALARM` filter. Declaring an
intent filter is not evidence of implementing it. Delete alarms in the clock app.

`termux-notification-list` hangs unless notification access is granted
separately, so nothing here depends on it.

## Notifications and toasts

`tealy-notify TITLE [CONTENT...]` posts an Android notification and
`tealy-toast MESSAGE...` flashes a transient toast. Both need the `termux-api`
package and the Termux:API app. Neither is an alarm: notifications are
Doze-batched and do not ring.

## Requirements

An SSH server on the Termux device, `termux-api` plus the Termux:API add-on for
the notification and toast functions, and Termux 0.118.1 or newer. That version
added `com.android.alarm.permission.SET_ALARM` to Termux's manifest; without it
`am start -a android.intent.action.SET_ALARM` fails with a `SecurityException`
naming that permission, and F-Droid installs predating mid-2024 are the usual
cause.

Only the `app_process` variant of `am`, from the `termux-am` package, is used.
The socket variant from `termux-am-socket` fails with `Could not connect to
socket` unless the Termux app's socket server is running.

All user-supplied text crosses as environment variables rather than being
interpolated into the remote shell program, so a message containing quotes,
backticks or `$(...)` is inert. Host arguments that are empty, begin with `-`,
or contain whitespace are rejected so they cannot be read as SSH options.
