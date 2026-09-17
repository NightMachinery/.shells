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

`TIMESPEC` is parsed by `datenat-unix-v2`, which accepts compact durations and prose:

```zsh
tealy-alarm 1h30m               tealy-alarm 'in 3 hours'
tealy-alarm 90m                 tealy-alarm 'tomorrow 8am'
tealy-alarm '1h:30m later'      tealy-alarm 'next friday 9am'
tealy-alarm 3h                  tealy-alarm 7:30
```

Parsing happens **on this machine**, and the spec crosses the wire as a unix
timestamp which the phone renders into an hour and minute. An epoch is
unambiguous, so a disagreement between the two machines' timezones cannot shift
the alarm.

The resolved time is printed **before** the intent is sent, so a misparse can be
aborted with ctrl-c rather than discovered when the alarm fails to ring. It is
shown by `unix2human`, which drops the date when the alarm falls on today:

```zsh
$ tealy-alarm 1h30m 'tea'
alarm @ tealy: 18:50
$ tealy-alarm 'tomorrow 8am' 'standup'
alarm @ tealy: 1405/Shahrivar6/27 Friday 18/September9/2026 08:00
```

Seconds are dropped because `SET_ALARM` carries only an hour and a minute, so
showing them would imply a precision the alarm does not have. With `alarm_tz`
the time is rendered in that zone and the zone is named, since it is then not
this machine's: `alarm @ tealy [Asia/Tehran]: ... 22:24`.

That one line is the whole output. The phone stays silent on success rather than
echoing the time back, and speaks only when its own rendering disagrees:

```
termux-alarm: phone set 18:24 (Europe/Berlin), not 99:99 as shown locally
```

which happens when the two machines disagree about the timezone.

`termux_alarm_dryrun=y` stops after the local line and sends nothing. Use it to
check a spec: alarms cannot be removed programmatically, so a careless test
leaves something that will ring.

`datenat-unix-v2` tries two parsers in a deliberate order:

1. A duration chain, via `h-dur-nat2sec` and `dur2sec`. Units are `w d h m s`,
   and `m` always means minutes, never months. `1h30m`, `1h:30m:0s`,
   `'1h 30m later'`, `'in 1h30m'` and `1w2d3h` all work, and `dur2sec` is the
   inverse of `seconds-fmt-short`, so its output round-trips.
2. Otherwise `datenat`, which is chrono. This covers everything conversational.

The order is not arbitrary. **Chrono silently misparses `1h:30m later` as thirty
minutes**, swallowing the `1h:` and returning a perfectly plausible wrong time.
Claiming durations first removes the input from chrono's reach. Fractions like
`1.5h` are supported by neither and fail loudly, as does anything unparseable.

Chrono is also run with `datenat_strict=y`, which rejects two further silent
wrongnesses rather than acting on them:

- A **partial match**, where chrono understands only some of the input and
  discards the rest. `1h:30m later` matches just `30m later`.
- An **invented hour**. `next friday`, `monday` and `dec 25` name no time of
  day, and chrono fills in 12:00; `tomorrow` carries the current clock time
  instead. As an alarm that rings at a time you never specified, so it is
  refused: write `next friday 9am`.

Both are detected from `chrono.parse()`'s match span and `isCertain('hour')`,
which `parseDate()` throws away. Strict mode is off by default for `datenat`
itself, since its other callers pass free prose where partial matching is the
point.

### Two traps this deliberately avoids

`date -d 3h` does **not** mean three hours. GNU date reads a bare trailing
letter as a [military time
zone](https://www.gnu.org/software/coreutils/manual/html_node/Time-zone-items.html),
so `3h` is 03:00 at UTC+8 and does not fail. Measured on the phone at 17:04:
`3h -> 21:00`, `1m -> 15:00`, `8p -> 13:00`. Nothing here ever passes raw user
text to `date`, which is why `3h` now means three hours.

The hour and minute are formatted with `%-H` and `%-M` rather than `%H` and
`%M`. `am` parses `--ei` values as Java integers, so a zero-padded `08` is read
as **octal** and rejected with `For input string: "8" under radix 8`. That broke
`tomorrow 8am` and any `:08` or `:09` minute while leaving other times working.

`SET_ALARM` carries only an hour and a minute, so Android schedules the next
occurrence and a time already past today rolls over to tomorrow by itself.

Repeating alarms would need the `android.intent.extra.alarm.DAYS` extra, an
`ArrayList<Integer>` passed with `am`'s `--eial`. Untested, not implemented.

## Timezones

The epoch is rendered into a wall-clock hour and minute **on the phone**,
against `getprop persist.sys.timezone`, inside the same SSH command that sets the
alarm. Neither shell's `TZ` affects the result and it costs no extra round trip.
Set `alarm_tz` to render in another zone.

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

## Latency

Setting an alarm takes about a second, and almost none of it is ssh. Measured:

```
am (app_process + am.apk dex load)   ~706 ms
ssh channel (already multiplexed)    ~250 ms
getprop + date on the phone           ~67 ms
local parse: duration / prose       10 / 150 ms
```

One ssh call per alarm, none for a dry run, over a connection ssh is already
multiplexing. There is nothing to batch: the cost is Android spawning a JVM to
run `am.apk` once per alarm, and the intent dispatch inside it is only ~7ms.

The helpers therefore prefer `termux-am`, which passes the intent to the running
Termux app over a socket instead of spawning `app_process`, and fall back to `am`
when it is unavailable. The socket lives at
`$PREFIX/../apps/com.termux/termux-am/am.sock` and only exists on Termux app
versions that create `files/apps`; on 0.118.1 that directory is absent, so the
fallback is what runs and the saving is not yet realised. Nothing needs changing
to collect it later, only a newer Termux app.

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
