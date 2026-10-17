# Dedicated macOS notification apps

Night Mail and Agent Bells use separate signed application bundles, with their own
icons, notification permissions, alert styles, and dismissal boundaries. They
share `macos/notification-apps/build.py`, the launcher, and the icon renderer.
The system/Homebrew terminal-notifier installation is untouched.

## Installation

- Email setup, `~/doom.d/email/install-night-mail.zsh`, automatically installs
  `~/Applications/Night Mail.app` before starting the sync LaunchAgent.
- `notif-app-install agents` installs `~/Applications/Agent Bells.app`. The first
  agent desktop notification also installs it when missing. Hooks on other
  operating systems retain their existing behavior.
- `notif-app-install mail --check` and `notif-app-install agents --check` verify
  the signature and build inputs, and query macOS notification authorization.
  Checks run through LaunchServices in the Aqua login session and use a result
  receipt, so a successful `open` launch is not mistaken for permission success.
  Authorization codes: 0 not determined, 1 denied, 2 authorized, 3 provisional,
  4 ephemeral. A nonzero command exit means something needs attention.

The first installation requests notification permission and posts a silent setup
notification, respecting Do Not Disturb. Allow the relevant app in System
Settings → Notifications. Agent Bells and Night Mail can use different banner or
alert styles. Installation cannot approve the macOS permission prompt for you. Apple records
that decision and does not prompt again on subsequent authorization requests;
a denied app must be enabled in System Settings. The bootstrapper requests
permission after a first install or rebuild, refreshes the app's source identity,
and reports failures without blocking mail sync. It does not change Focus or
system banner/alert preferences.

Requirements: Python 3.9+, macOS 10.14+, and Apple's Command Line Tools (`clang`,
`xcrun`), plus the system `curl`, `sips`, `iconutil`, and `codesign` tools. The
current host already has these. No administrator access is needed.

Source is downloaded on demand from terminal-notifier's pinned 2.0.0 archive,
verified against a checked-in SHA-256, and cached under
`~/.cache/night-notification-apps/`. Subsequent installs work offline. The app
records hashes of all build inputs and is reused when current; rerun installation
after pulling changes to rebuild it. Builds are serialized. Rebuilds happen in a
staging directory, and compilation/registration failure preserves the previous
app. An unrelated app at the destination is never replaced. Ad-hoc signing uses
a stable designated bundle identifier requirement, so an ordinary rebuild does
not change identity to a new executable hash.

Normal delivery, listing, and dismissal use Apple's `UserNotifications` API.
Successful posting means macOS accepted the request, not that a banner appeared:
Focus and the user's notification settings still decide presentation. The older
`NSUserNotification` service can reject requests even when the current API
reports authorization, producing delivery timeouts. Only explicit DND bypass
retains that legacy upstream delivery path. Removal handles both APIs within the
selected app identity. Modern notifications use the group as their request ID,
so dismissal and replacement address the same alert. Unsupported optional image
attachments are omitted rather than preventing a text alert.

The nib-free launcher removes the stock Terminal sender override, so macOS uses
the real bundle's icon and identity. It also removes upstream's logging of
notification contents on click. Original envelope and terminal artwork is drawn
locally from vector source; no third-party app artwork is redistributed.

## Notification content

Mail alerts use the subject as title, sender as subtitle, and a bounded multiline
body preview. Agent bells use the session/task as title, project/agent as subtitle,
and the actual event text as body. Constant app names do not occupy the title.
The common shell gateway accepts `notif_title`, `notif_subtitle`, and `notif_body`
for callers that have structured context; generic calls keep their previous
behavior. Content changes preserve per-app grouping and dismissal boundaries.

## DND and identity

Mail and agent bells respect Do Not Disturb by default. Mail enables bypass only
through `bypass_dnd: true` in its JSON settings. Agent bells accept an explicit
`notif_ignore_dnd_p=y bell-claude` (or `bell-codex`) call.

Optional bypass uses terminal-notifier 2's private `_ignoresDoNotDisturb` API.
Newer terminal-notifier versions ignore the corresponding option, which is why
this helper pins 2.0.0. This is best effort and depends on macOS compatibility; a rejected legacy
connection can also prevent bypass delivery. Such failures remain nonfatal.
macOS Focus allowlists and notification settings can also affect what is shown.

`notif_app=agents` and `notif_app=mail` select the dedicated identities for
`notif-os` and `notif-os-remove`. The default remains the generic notifier for
unrelated shell notifications. `notif-os-dismiss-agent` clears only Agent Bells'
delivered desktop notifications. It does not stop a ringing job, remove a queued
Telegram escalation, or touch mail. Per-session acknowledgement still cancels
the individual agent's notification and its Telegram escalation.

Email uninstall remains LaunchAgent-only and preserves apps and settings. To
remove an unused app, delete its bundle from `~/Applications` manually.

## Verification

Run `python3 "$NIGHTDIR/macos/notification-apps/test_build.py"` and
`zsh -c 'source "$NIGHTDIR/zshlang/tests/notification-apps.zsh"'` for hermetic
bootstrap and routing tests. These never send desktop or Telegram notifications.
The email repository has a real-notmuch integration suite using a synthetic
mailbox and a fake notifier.
