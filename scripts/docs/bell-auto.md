# bell-auto

`bell-auto` rings a bell until you come back to the machine. That is the right
behaviour when you have stepped away from your own desk, and the wrong one when you
are at the office sharing a room with other people, or when you are not coming back
at all.

So it now escalates instead of only ringing: sound, then a desktop notification,
then your phone.

## The escalation ladder

Every mode except `idle` walks the same four stages. The modes differ only in how
long stage 1 lasts.

1. **Noise.** Ring the engine. `idle+timeout` keeps ringing for up to
   `bell_auto_max_t`; `bell+notif` rings exactly once; `notif` skips this entirely.
2. **Desktop notification.** Fires the moment stage 1 gives up. Desktop only.
3. **Quiet watch.** No sound. Polls `idle-get` every `bell_auto_tlg_poll` seconds.
   If you touch the machine, it exits silently — you saw the notification.
4. **Telegram.** If you stayed away for `bell_auto_tlg_t` seconds, everything queued
   goes to your phone in one message.

At any point during stage 1, user activity ends the bell immediately, exactly as it
always did.

## Modes

Set with `bell_auto_stop_mode`, or `@opts stop_mode <mode> @ bell-auto`.

- `auto` — the default. Resolves once at startup, and logs which way it went.
- `idle` — the original behaviour, unchanged: ring until the user returns. No
  notification, no Telegram, no ladder. This is what the continuous-sound bells use.
- `idle+timeout` — ring for at most `bell_auto_max_t` (default 3600s), then enter the
  ladder at stage 2.
- `notif` — no bell at all; straight to stage 2.
- `bell+notif` — one bell, then stage 2.

`auto` resolves in order:

- `meeting-p` → `notif`. Even one ring is wrong in a meeting: speakers feed it
  into the mic, headphones play it over whoever is talking. `meeting-p` reads the
  *current* browser tab, so a meeting behind another tab still rings — an annoying
  failure rather than a silent one, which is the tolerable direction.
- `office-public-audio-p` or `headphones-p` → `bell+notif`. On office speakers the
  colleagues hear every repeat. On headphones, one ring is all the loop can
  usefully deliver: worn, the first ring did the job and repeats just keep ducking
  your audio; on the desk, no number of rings reaches anyone and looping would
  only delay the notification ladder by up to `bell_auto_max_t`.
- otherwise → `idle+timeout`. Open speakers in a private space is the one
  situation where the ringing loop earns its keep: repetition reaches a person the
  first ring missed, and bothers nobody else.

`headphones-p` classifies the default audio *output device*, not your ears. Buds
in their case disconnect and stop being the default, so that resolves to speakers;
buds connected but lying on the desk still classify as worn, which costs one
unheard ring and an immediate notification — still better than an hour of equally
unheard looping.

## Notifications are opt-in

If `bell_auto_notif_msg` is empty, the whole ladder from stage 2 onward is skipped.
A bell with no message just rings.

This is what makes `auto` safe as a global default. `bella-zsh` fires on every
completed interactive command and sets no message, so it keeps behaving exactly as
before — and it already pops its own `alert "Completed: ..."` overlay, so a second
popup would be noise.

## The Telegram queue

Messages are queued in redis (`bell_notif_pending`) and drained by a single watcher,
which sends them as one batched message.

The watcher holds its **own** single-instance nonce (`bell-auto-notify`), separate
from the one that controls ringing (`bell-auto`). This matters: any `bell-auto`
invocation takes the ringing nonce and cancels every earlier bell, and both
`bella-zsh` and the sc bells fire routinely without a message. If the watcher shared
that nonce, an unrelated bell would cancel a pending escalation and then never drain
the queue, stranding the message in redis with nobody watching it.

Because the queue is shared, a session whose bell was replaced still gets its message
delivered — whoever ends up watching sends everything. Identical messages are
collapsed to one line.

The deadline is anchored to the **oldest** unsent message, not the newest, so a steady
drip of bells cannot postpone the batch forever.

`bellaok` (`bell-auto-stop`) resets both nonces and clears the queue: telling the
machine to be quiet should not resurface later as a phone notification.

Inspect the queue with `bell-notif-pending`.

## Agent hooks

`bell-claude` and `bell-codex` go through `h-bell-agent-hook`, which reads the agent's
hook payload as JSON — from `$1`, or from stdin — and builds the message from it.

Claude Code's `Notification` event carries a real `message`; its `Stop` event does
not, so that falls back to `Claude awaits!`. The `cwd` becomes a `[project]` tag,
which is what tells two waiting sessions apart.

Both the agent name and the tag go in the prefix — `Claude [scripts]: needs your
permission to use Bash` — so a batched Telegram is scannable down its left edge. The
fallback already starts with the agent name, so it takes only the tag:
`Claude awaits! [scripts]`.

For the payload to arrive, the hook in `~/.claude/settings.json` must forward stdin:

    brishz_in=MAGIC_READ_STDIN brishz2.dash bell-claude

Reading stdin is bounded by a 2s `gtimeout`, so an inherited pipe that never closes
cannot wedge the agent's hook.

### One notification per waiting session

Agent hooks fire repeatedly — every permission prompt, every stop — and each firing
used to leave another notification behind. They now pass a `notif_group` of
`agent-<app>-<project>`, and `notif-os` forwards it as `terminal-notifier -group`:
posting removes the previous undismissed notification of the same group first, so
repeats *update* the one notification instead of piling up.

The key is per app+project, not per app, because a new project's wait would
otherwise overwrite another project's still-pending one — exactly the information
the `[project]` tag exists to carry. Sessions without a `cwd` share the app's key.

`notif_group` is a general `notif-os` knob; anything else that restates one fact can
set it. Empty (the default) posts an ordinary ungrouped notification.

### Making them persist until dismissed

Whether a notification fades on its own is decided by its *style*, not by the
sender: banners fade, alerts stay until closed. To make the agent notifications
persistent, set **System Settings → Notifications → terminal-notifier → Alerts**.
Combined with the grouping above this yields one persistent, self-updating
notification per waiting session, each with macOS's own close button — no custom
overlay needed.

The style applies to everything `terminal-notifier` posts, so casual `notif` calls
stop fading too; that is the price of the zero-code route. `notif-os-dismiss-all`
(and the Hyper+D binding that calls it) clears alerts the same as banners.

## Do Not Disturb

`notif-os` passes `-ignoreDnD` by default (`notif_ignore_dnd_p`). A notification
raised by our own code is almost always worth seeing, whereas Do Not Disturb is aimed
at calls and social apps. Set `notif_ignore_dnd_p=n` per call to respect it.

If a banner is ever suppressed anyway, `bell_auto_notif_alert=y` adds a Hammerspoon
`hs-alert` overlay, which is drawn outside the notification system entirely and so
cannot be suppressed. It is off by default because `-ignoreDnD` already works here
and the overlay is then pure duplication.

Stage 4 sidesteps the whole question, since Telegram does not care about DnD.

## Icons

macOS takes a notification's icon from the sending bundle and **silently ignores**
`terminal-notifier -appIcon`. Verified on this machine: `-appIcon` changes nothing,
and `-sender` hangs indefinitely.

What does work is `-contentImage`, an image inside the notification body, exposed as
`notif_image`. `app-icon-get Claude` returns a cached PNG of an app's icon, extracting
it from the bundle's icns on first use.

Note that `terminal-notifier` exits 0 for a nonexistent image, so `notif-os` checks
the path itself rather than trusting the exit code.

## Configuration

Bell behaviour, all `@opts`-settable with the `bell_auto_` prefix:

- `bell_auto_stop_mode` — see Modes. Default `auto`.
- `bell_auto_t` — idle threshold in seconds; below this the user counts as present.
  Default 30.
- `bell_auto_max_t` — wall-clock cap on ringing in `idle+timeout`. Default 3600.
  Deliberately not called `timeout`, because `bell_auto_t` already is one.
- `bell_auto_notif_msg` — notification text. Empty means no ladder.
- `bell_auto_notif_alert` — add the Hammerspoon overlay. Default `n`.
- `bell_auto_alert_dur` — overlay duration. Default 10.
- `bell_auto_tlg` — `auto` (default), `y`, or `n`. `auto` means on iff `office-p`,
  because at the office the laptop gets left behind and the phone is the only channel
  that still reaches you.
- `bell_auto_tlg_t` — seconds of continued idleness before escalating. Default 900.
- `bell_auto_tlg_poll` — watch-loop interval. Default 30.

Notification plumbing:

- `notif_ignore_dnd_p` — bypass Do Not Disturb. Default `y`.
- `notif_image` — path to an image for the notification body.

## Where am I?

`office-p` answers "are we at the LMU/CIS office", layered:

1. A manual redis override — `office-on`, `office-off`, `office-auto` to clear.
   Checked before the cache, so it takes effect immediately.
2. `office-p-net` — the default route is on a physical interface (`en*`, explicitly
   not `utun*`) **and** either the DNS search domain matches `cis.uni-muenchen.de` or
   the address falls in `129.187.148.128/25`. The physical-interface requirement is
   what stops the LMU VPN from making home look like the office.
3. `office-p-display` — an external display is attached.

`office-p-explain` shows what each backend thinks, bypassing the cache. Results are
memoised for `office_p_cache_ttl` seconds (default 120), since `auto` asks on every
bell but the answer changes when you walk somewhere.

Add a second site by editing `office_p_domains` / `office_p_subnets` in
`zshlang/auto-load/others/monitor/location.zsh` — a data change, not a code change.

`office-public-audio-p` names the question `stop_mode=auto` actually asks: at the
office **and** not on headphones, i.e. would a sound right now be audible to
colleagues. It is shared with the audio leak guard, which mutes the output
device when that holds and nobody is at the desk — see `audio-guard.md`. It takes
the same optional `<name> <transport>` arguments as `headphones-p`, for callers
that already know the output device and want to skip the lookup.

### external-display-p

`external-display-p` matches display **names**, and deliberately does not count
screens. In clamshell mode the lid is shut, the built-in panel is not reported at
all, and the single remaining screen is the external one — so a count-based test is
wrong exactly when it matters. Anything not matching `built-in` or `color lcd` counts
as external.

`displays-get` lists attached displays, via Hammerspoon (~8ms) with a
`system_profiler` fallback (~400ms).

## Gotchas

Keep backticks out of `:` docstrings. They are double-quoted, so zsh runs command
substitution on them — a docstring showing an example call of its own function makes
that function recurse on every invocation. This cost `app-icon-get` 1.2s and
`h-ipv4-in-subnet-p` 2.6s per call before it was spotted.

BrishGarden holds persistent shells, so run `brishz-restart` after changing zsh code
you intend to exercise through the agent hooks. Testing in a fresh `zsh -ic` says
nothing about what the garden is running.

The Telegram escalation is time-bounded with `reval-timeout` rather than `gtimeout`,
because `tnotif` is a zsh function and an external timeout binary cannot run one.
