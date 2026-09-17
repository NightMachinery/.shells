# Remote Termux alarms

`tealy-alarm TIMESPEC [MESSAGE...]` sets an alarm in the phone's own clock app
over SSH; `tealy-timer DURATION [MESSAGE...]` starts a countdown.

```zsh
tealy-alarm 7:30 'wake up'
tealy-alarm 1h30m 'tea'
tealy-alarm 'tomorrow 8am' 'standup'
tealy-timer 20m 'pasta'
```

The alarm belongs to the clock app, not to Termux, so it rings through Doze and
silent mode and survives Termux being killed, the SSH connection dropping, and
this machine sleeping. The phone must be reachable when the alarm is *set*, not
when it rings. That is why this uses neither `termux-notification` nor
`termux-job-scheduler`: both are Doze-batched and inexact, the latter with a
documented minimum period of fifteen minutes.

The generic helpers take a host first, as `h-termux-alarm-set HOST TIMESPEC`,
in the same shape as `h-paste-from-remote-termux` in
`./docs/remote-termux-clipboard.md`. Hostnames, users, ports and keys belong in
`~/.ssh/config`, not in these functions.

## Time specifications

Parsed on this machine by `datenat-unix-v2`, which takes compact durations
(`1h30m`, `90m`, `3h`, `'1h:30m later'`, `1w2d3h`) and prose (`in 3 hours`,
`tomorrow 8am`, `next friday 9am`, `7:30`). Units are `w d h m s`, and `m`
always means minutes. Fractions such as `1.5h` are not supported.

It tries a duration chain first and falls back to `datenat`, which is chrono.
The order matters: chrono silently misreads `1h:30m later` as thirty minutes,
swallowing the `1h:`. It is also run under `datenat_strict`, which rejects two
further silent wrongnesses instead of acting on them: a **partial match**, where
chrono understands only part of the input, and an **invented hour**, since a
spec like `next friday` names no time of day and chrono fills in noon. Write
`next friday 9am`.

`SET_ALARM` carries only an hour and a minute, so Android schedules the next
occurrence and a time already past today rolls over by itself. Repeating alarms
would need the `android.intent.extra.alarm.DAYS` extra via `am`'s `--eial`;
untested, not implemented.

## Timezones

The spec is parsed here and crosses the wire as a **unix timestamp**, which the
phone renders against `getprop persist.sys.timezone`. An epoch is unambiguous,
so the two machines disagreeing about the timezone cannot shift the alarm.
`alarm_tz` overrides the rendering zone.

This matters because the clocks can disagree. `sshd` on the phone inherits its
environment from whichever shell started it, and was running in a different zone
from the system one, so a naive `date` over SSH reported one time while the
clock
app scheduled in another. The phone's `~/.zshenv` now exports `TZ` from the
system property, which fixes interactive sessions too, but the helpers do not
rely on it.

## Output

One line, before the intent is sent, so a misparse can be aborted with ctrl-c:

```
alarm @ tealy: 08:00
```

The date is shown only when the alarm is not today. Seconds are dropped because
`SET_ALARM` carries only an hour and a minute. With `alarm_tz` the zone is named
and the time rendered in it, since it is then not this machine's.

`termux_alarm_dryrun=y` stops there and sends nothing. Use it to check a spec:
alarms cannot be removed programmatically, so a careless test leaves something
that will ring.

The phone stays silent on success and speaks only when its own rendering
disagrees, which means the two machines disagree about the timezone:

```
termux-alarm: phone set 6:15 (Some/Zone), not 4:45 as shown locally
```

## Exit 0 means nothing

This is the trap the whole interface turns on. `am` reports whether an *activity
started*, never what the app then did with the intent, so a completely failed
alarm is indistinguishable from a working one.

- Exit 2, `Activity class ... does not exist`: wrong or filtered component.
- Exit 1, `unknown error code 102`: Android blocked a background activity
launch.
- Exit 0: an activity started. The alarm may or may not exist.

The concrete way this bites: if several apps handle `SET_ALARM` and none is the
default, Android launches the disambiguation chooser. That is an activity, so
`am` prints `Starting: Intent {...}` and exits 0, but the chooser knows nothing
about `SKIP_UI` and is invisible because Termux is in the background. No alarm
is created and every signal says success. Setting a default clock app fixes it.

The functions therefore pin the clock app's API handler explicitly through
`termux_alarm_component`. An explicit component cannot reach a chooser, and a
renamed class fails loudly with exit 2 rather than silently. Set it to the empty
string for implicit resolution, which follows the default clock app and is
portable to other phones.

### Finding the right component

Pin the activity that **declares the intent filter**, not merely one that
exists: a launcher activity accepts the launch, returns 0, and discards the
extras.

```zsh
pkg install aapt
apk="$(pm path com.android.deskclock | sed 's/^package://')"
aapt dump xmltree "$apk" AndroidManifest.xml | grep -B40 SET_ALARM
```

Vendor forks keep the AOSP package name while renaming AOSP's `HandleApiCalls`,
so checking only the AOSP name yields `does not exist` and invites the wrong
conclusion that there is no API handler at all.

One more hint: `Activity not started, intent has been delivered to currently
running top-most instance` means the target was already foregrounded. A
no-display API handler never produces it, so seeing it suggests a wrong pin.

## What cannot be done

Alarms cannot be listed. Android exposes no read API, and `SHOW_ALARMS` only
opens the clock app's UI.

Alarms cannot be dismissed or deleted. Every search mode (`android.label`,
`android.next`, `android.all`, `android.time`) was tried against both the API
handler and the launcher activity, and via implicit resolution; all return
`START_SUCCESS` and remove nothing. The clock app's own bytecode says why:

```zsh
apk="$(pm path com.android.deskclock | sed 's/^package://')"
unzip -o -q -j "$apk" 'classes*.dex' -d dex
cat dex/*.dex | grep -a -c -- android.intent.extra.alarm.SEARCH_MODE   #: 0
```

`SEARCH_MODE` occurs zero times, as do all four of its values, while `SKIP_UI`
and `MESSAGE` occur once each. The app implements the set path and never reads
the extra saying *which* alarm to dismiss, so no argument combination can work,
even though its manifest advertises the `DISMISS_ALARM` filter. Declaring an
intent filter is not evidence of implementing it.

Timers are singleton: setting one replaces any pending timer. There is no silent
cancel. A zero length does nothing, and a one-second timer displaces the pending
one but then rings.

`termux-notification-list` hangs unless notification access is granted
separately, so nothing here depends on it.

## Latency

About a second per alarm, and almost none of it is SSH:

```
am (app_process + am.apk dex load)   ~706 ms
ssh channel (already multiplexed)    ~250 ms
getprop + date on the phone           ~67 ms
local parse: duration / prose       10 / 150 ms
```

One SSH call per alarm, none for a dry run, over a connection SSH already
multiplexes. Nothing is left to batch: the cost is Android spawning a JVM to run
`am.apk` once per alarm, and the dispatch inside it is only ~7ms.

The helpers therefore prefer `termux-am`, which passes the intent to the running
Termux app over a socket instead of spawning `app_process`, and fall back to
`am`
when its socket is absent. Two socket paths are checked,
`$PREFIX/../apps/com.termux/`
and `$PREFIX/../apps/termux-app/`, because upstream moved it. Older Termux app
builds do not create `files/apps` at all, in which case the fallback runs and
the
saving is not realised; a newer app collects it with no code change.

If upgrading for that, note that F-Droid, GitHub and Play Store builds are
signed
with different keys, so the upgrade must come from whichever source installed
the
app. Crossing sources requires uninstalling, which destroys the Termux prefix.

## Notifications and toasts

`tealy-notify TITLE [CONTENT...]` posts a notification and
`tealy-toast MESSAGE...` flashes a transient toast. Both need the `termux-api`
package and the Termux:API app. Neither is an alarm: notifications are
Doze-batched and do not ring.

## Requirements

An SSH server on the Termux device, `termux-api` plus the Termux:API add-on for
the notification and toast functions, and Termux 0.118.1 or newer. That version
added `com.android.alarm.permission.SET_ALARM` to Termux's manifest; without it
`am start -a android.intent.action.SET_ALARM` fails with a `SecurityException`
naming that permission.

Only the `app_process` variant of `am`, from the `termux-am` package, is
required. The socket variant is used when available but never depended on.

All user-supplied text crosses as environment variables rather than being
interpolated into the remote shell program, so a message containing quotes,
backticks or `$(...)` is inert. Host arguments that are empty, begin with `-`,
or contain whitespace are rejected so they cannot be read as SSH options.
