# Frontmost application

[agfi:frontapp-get] returns the frontmost macOS application's bundle identifier.
When macOS supplies no identifier, it returns the process name instead. For
example, shell-launched mpv can return `mpv`, while the app bundle returns
`io.mpv` when its identifier is available.

The AppleScript backend, [agfi:frontapp-get-osa], reads both properties from
the same frontmost process. Missing or empty identifiers fall back to the name;
an unavailable process or a missing name and identifier causes a nonzero exit.
Query failures propagate through the output pipeline. A nonempty
`front_app_cached` still bypasses the query and is returned unchanged.

[agfi:frontapp-get-bundle-id-osa] remains identifier-only, and
[agfi:frontapp-get-name-osa] remains name-only.

## mpv and callers

[agfi:mpv-frontmost-p] takes no arguments and writes nothing to stdout. It
returns success when [agfi:frontapp-get] reports `io.mpv` (the app bundle),
`mpv` (the process-name fallback), or the legacy `*/bin/mpv` path form. It
returns nonzero for other apps and propagates query failures. It queries the
front app once and respects `front_app_cached` through [agfi:frontapp-get].

[agfi:luna-skip-p] uses this predicate in addition to its meeting and Do Not
Disturb checks. Frontmost mpv qualifies whether playing or paused; an mpv
running in the background does not. [agfi:nightshift-auto] uses the same
predicate to turn Night Shift off for frontmost mpv and on otherwise, while
retaining its enabled-state gate.

## Verification

Run the regression checks from the repository root:

```zsh
python3 zshlang/tests/frontapp-get.py
python3 zshlang/tests/mpv-frontmost.py
```

The checks use synthetic app identities. On macOS they execute the actual
AppleScript fallback with a fixture in place of the System Events query, so
they do not change focus or inspect running apps. Shell checks stub the backend
to verify cached values and error propagation, including with `pipefail` off.

After changing the shell functions, run [agfi:brishz-restart] so BrishGarden
loads the new definitions.
