# Recalling a saved Wi-Fi password

`wifi-password-get-fz` lists the networks this Mac remembers, lets you pick one,
and puts its password on the clipboard. `wifi-password-get <SSID>` is the same
thing when you already know the name.

Both live in `zshlang/auto-load/others/macOS/net.zsh`.

## Why the obvious command finds nothing

The command everyone reaches for first is

    security find-generic-password -w "Example Net"

and on a Mac that has been joined to `Example Net` for years, it fails with
"The specified item could not be found in the keychain". Two separate reasons,
and you need to fix both before you get an answer:

- Wi-Fi secrets live in the **System** keychain,
  `/Library/Keychains/System.keychain`, which is not on the keychain search list
  `security` uses by default. It has to be named as an explicit argument.
- They are generic-password items whose *kind* is `AirPort network password`.
  Without `-D` to select that kind, the search does not match them.

So the working form, and what `wifi-password-get` runs, is

    security find-generic-password -D 'AirPort network password' \
        -a "Example Net" -w /Library/Keychains/System.keychain

The failure mode is what makes this worth writing down: a missing argument and
an unknown network produce the *same* message, so the command confidently tells
you a network has no password when the real problem is that you did not ask the
right keychain.

## The authorization dialog is expected

`security` is not the application that created these items, so macOS raises a
GUI authorization prompt the first time it reads one. This is per item —
granting access for one network does nothing for the next.

`python/claude_code_usage.py` hit the same wall for its own keychain reads and
has a longer note on it, including why a timeout there almost always means an
unanswered dialog.

## Reading the exit codes

`security` exits with the **low byte of the OSStatus**, which is the only reason
its exit codes look arbitrary. `security error <n>` decodes an OSStatus but not
these exit codes, so you have to do the arithmetic yourself:

- `errSecItemNotFound` is `-25300` = `0xFFFF9D2C`, and `0x2C` is **44**
- `errSecInteractionNotAllowed` is `-25308` = `0xFFFF9D24`, and `0x24` is **36**
- `userCanceled` is `-128` = `0xFFFFFF80`, so **128**

**44 is an answer, not a failure.** There is no such item, and `security` says so
without prompting. `wifi-password-get` reports "no saved password" and returns 1,
because a remembered network legitimately has no stored secret when it is open,
or when it is 802.1X and the credential lives elsewhere.

**36 does not mean the prompt was denied.** It means macOS refused to *show*
one, because the calling process is not attached to the GUI session. Running
inside tmux without `pam_reattach`, or over ssh, lands here. The message is
misleading enough that it is worth stating plainly: the network does have a
password and you did not do anything wrong.

The brish garden is **not** one of these contexts, and an earlier version of
this document wrongly said it was. The garden's worker shells are attached to
the GUI session: they see the full keychain search list, and they read login
keychain items without a dialog even when the request arrives from an ssh
session. On a System keychain item like a wifi password the garden is not
refused either — it *raises* the dialog, which is a different problem. A probe
through the garden simply blocks, with `SecurityAgent` waiting for an answer,
until something kills it.

That is precisely why the root fallback stays the default here rather than being
replaced by delegation. Root reads the System keychain with no dialog at all, so
on a 36 the function retries once as `sudo` rather than telling you to go find a
different terminal. Turn that off with `wifi_password_get_sudo_fallback_p=n` if
you would rather see the failure. Both paths were checked to return
byte-identical values. Routing this through the garden instead would trade a
clean answer for a dialog left standing on a machine with nobody in front of
it — so do not "fix" it that way. Delegation to the garden is the right answer
for a *login* keychain read, where root does not help;
`docs/claude_code_usage.md` covers that case.

**128 is a genuine cancel**, reported as such. Keeping all of these apart is what
stops an undisplayable dialog from looking like an absent password. `security`'s
own stderr is deliberately not redirected, so its message appears alongside ours.

## Two sources for the network list

`wifi-ssid-list` — the default. Runs
`networksetup -listpreferredwirelessnetworks` against the Wi-Fi interface. No
sudo, no auth dialog, instant. Its output is a header line plus one tab-indented
SSID per line, and the parser keys on that tab rather than skipping line 1, so
an SSID with leading spaces survives.

`wifi-ssid-list-known` — opt-in, needs root. Reads
`/Library/Preferences/com.apple.wifi.known-networks.plist`, which is
`0600 root:wheel`. Use it through `wifi-password-get-known-fz`, or by setting
`wifi_password_get_fz_source=known`.

Measured on 2026-09-07 on this machine, the two returned **exactly the same
set**. The second one is insurance for the cases where they can diverge — a
network dropped from the preferred list but still present in the keychain, or a
machine with a second Wi-Fi interface — not a coverage gap you are suffering
from today. That is why it stays opt-in: it costs a sudo prompt for a list that
is usually identical.

Two traps in reading that plist, both already handled:

- `plutil -convert json` **fails** on it outright, with "invalid object in plist
  for destination format" — the SSID blobs and the timestamps have no JSON
  representation. `-convert xml1` works.
- The top-level keys are `wifi.network.ssid.<SSID>`. Nothing nested carries that
  prefix — the `wifi.ssid.<hex>` entries under `CollocatedGroup` are `<string>`
  values, not `<key>`s — so matching the key is enough and no real plist parser
  is needed. XML entities are unescaped afterwards, for SSIDs containing `&`.

`security dump-keychain` is a third way to enumerate them. It is not used: it
raises an authorization prompt per item, which is exactly what you are trying to
postpone until after you have chosen one.

## The interface is not `en0`

It usually is, but the Wi-Fi port is `en1` on Macs with built-in ethernet, and
moves again behind some Thunderbolt docks. `h-wifi-device-darwin` derives it from
`networksetup -listallhardwareports` instead of hardcoding. Override with
`wifi_ssid_list_device` if you need a specific interface.

## Picker details

Two things in `wifi-password-get-fz` look optional and are not:

- **`--no-multi`.** `FZF_DEFAULT_OPTS` (in `bash/auto-load/configvars.bash`)
  turns `--multi` on globally, so `fz` returns several lines by default. One
  password on the clipboard is the whole point here.
- **`@RET`, not `@TRET`, on the `fz` line.** Pressing Esc is an ordinary
  outcome, not a bug worth a stacktrace. `@TRET` stays on the list-producing
  calls, where a failure really is one.

`fz` itself ends with `cat-copy-if-tty`, but its stdout is not a terminal inside
the command substitution, so the chosen SSID is *not* copied — only the password
reaches the clipboard. The chosen SSID is echoed to stderr for context, which
keeps it out of both stdout and the clipboard.

No `--no-sort`: with an empty query fzf preserves input order anyway, so macOS's
own preference ordering still shows, and keeping score sorting makes typing a
network name work properly.

## Usage

    wifi-password-get-fz                 # pick, then password to clipboard
    wifi-password-get-fz exam            # picker, pre-filtered
    wifi-password-get 'Example Net'      # skip the picker
    wifi-password-get-known-fz           # pick from the root-read list

Keyword arguments, in the usual `@opts` style:

    @opts source known @ wifi-password-get-fz
    @opts keychain /path/to/other.keychain @ wifi-password-get 'Example Net'
    @opts sudo-fallback-p n @ wifi-password-get 'Example Net'
    @opts device en1 @ wifi-ssid-list

## Known limitation, unverified

`security -w` is reported to print a hex dump rather than the literal string for
passwords containing non-ASCII bytes. No network with such a password was
available to test against, so this is not handled in code. If you hit a password
that comes back looking like `0x...`, that is the cause.
