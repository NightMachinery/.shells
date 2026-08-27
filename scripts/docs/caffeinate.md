# caffeinate

Keeps the display awake on behalf of a named key, so two features can want it
at once without either one switching the other off.

The logic is `zshlang/auto-load/others/power.zsh`, on top of `caffeinate(8)`.

## Commands

    caffeinate-on  [<key>]        # or: caffeinate_key=<key> caffeinate-on
    caffeinate-off [<key>]        # releases only that key
    caffeinate-holders            # which keys are holding it, one per line
    caffeinate-p                  # is anything holding it
    caffeinate-off-all            # force: release every key

The key defaults to `misc`, so a bare `caffeinate-on` still works. It is run
through `str2tmuxname`, because tmux rejects `:` and `.` in session names.

## How it holds

Each key gets its own `caffeinate -d` in its own tmux session, named
`caffeinate-<key>`. Nothing counts the holders. "Every key has released" is
exactly "no such process is left", which the kernel already tracks — so there is
no refcount to go stale across a reboot or a crashed holder, and no way for our
bookkeeping to disagree with the assertion it claims to describe.

Holders do not interfere. A power assertion is per-process, and the
`Assertion status system-wide` block of `pmset -g assertions` is the OR over
every holder listed beneath it, so the display may sleep again only once the
last one exits.

`caffeinate-on` checks `tmux-alive-p` before creating the session rather than
calling `tmuxnewsh2` blind: `tmuxnew` kills a session of the same name before
creating it, so asking twice for a key you already hold would restart its
assertion instead of doing nothing.

`caffeinate-holders` reports our keys only. Other processes hold assertions too
— macOS itself, and anything that ran `caffeinate` outside this scheme — so
`pmset -g assertions` remains the full picture:

    pmset -g assertions | grep -i preventuseridledisplaysleep

Sessions named plainly `caffeinate`, from before keys existed, are reported as
the key `legacy` and released by `caffeinate-off-all`.

## Who uses it

`brightness-off` and `brightness-off-loop` take the key `blackout`, and
`brightness-on` / `brightness-on-loop` release it; see
`external-display-brightness.md`. The point of blanking is a dark screen on a
machine that keeps working, which is exactly an idle-sleep problem.

Before keys existed, `caffeinate-off` was never called at all, so every
hyper+shift+F1 leaked an assertion: one was found still running 19 hours later.

## What it cannot do

Nothing here keeps the machine awake with the lid closed. Closing the lid is
the clamshell path, decided by powerd, and no assertion applies to it —
`caffeinate -d` prevents display sleep, `-i` idle system sleep, and `-s` system
sleep "only when system is running on AC power", all of which are *idle* sleep.
With an external display attached the machine stays up in clamshell mode; with
only the built-in panel it sleeps, whatever we assert.

The one real override is `sudo pmset -a disablesleep 1`, a persistent
system-wide setting rather than an assertion. We deliberately do not use it: a
laptop that ignores its lid can cook itself in a bag.

This is why blanking the built-in panel before shutting the lid buys nothing,
and why the blackout is undone on wake — see the "Keeping it blank" section of
`external-display-brightness.md`.
