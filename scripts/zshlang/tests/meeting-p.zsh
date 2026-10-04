#!/usr/bin/env zsh
# Run: zsh -c 'source "$NIGHTDIR/zshlang/tests/meeting-p.zsh"'
# Inert: browser discovery and tab transports use fixtures, with no GUI changes.
(
    setopt localoptions pipefail
    unsetopt errexit
    local fixture
    fixture="$(command mktemp -d "${TMPDIR:-/tmp}/meeting-p.XXXXXX")" || exit 1
    trap 'command rm -rf -- "$fixture"' EXIT
    command mkdir -p "${fixture}/bin"
    local -x MEETING_TEST_FIXTURE="${fixture}"
    command cat > "${fixture}/bin/chrome-cli" <<'SH'
#!/bin/sh
printf '%s\n' "$CHROME_BUNDLE_IDENTIFIER" >> "$MEETING_TEST_FIXTURE/queried"
case "$CHROME_BUNDLE_IDENTIFIER" in
    com.brave.Browser) exit 1 ;;
    org.chromium.Chromium) sleep 10; exit 1 ;;
esac
cat "$MEETING_TEST_FIXTURE/$CHROME_BUNDLE_IDENTIFIER" 2>/dev/null
SH
    command cat > "${fixture}/bin/osascript" <<'SH'
#!/bin/sh
cat >/dev/null
cat "$MEETING_TEST_FIXTURE/safari"
SH
    command chmod +x "${fixture}/bin/chrome-cli" "${fixture}/bin/osascript"
    local -x PATH="${fixture}/bin:${PATH}"
    local CHROME_BUNDLE_IDENTIFIER=com.brave.Browser
    local fixture_running='' failed=0
    function h-browser-running-bundle-ids {
        local id
        for id in "${(@f)fixture_running}" ; do
            (( ${@[(Ie)$id]} )) && print -r -- "${id}"
        done
        return 0
    }
    function check {
        meeting-p >/dev/null 2>&1
        local result=$?
        if (( result != $2 )) ; then
            print -ru2 -- "FAIL: $1 (expected $2, got ${result})"
            failed=1
        fi
    }

    print -r -- '[1:1] https://example.invalid/
[1:2] https://meet.google.com/aaa-bbbb-ccc' > "${fixture}/com.google.Chrome"
    fixture_running=$'com.brave.Browser\ncom.google.Chrome'
    check 'background Meet tab in Chrome despite default Brave failing' 0
    [[ "${CHROME_BUNDLE_IDENTIFIER}" == com.brave.Browser ]] || failed=1

    fixture_running=''
    check 'closed Chrome with a saved meeting fixture is skipped' 1
    fixture_running=com.google.Chrome
    print -r -- '[1:1] https://example.invalid/' > "${fixture}/com.google.Chrome"
    check 'no meeting pages' 1
    print -r -- '[1:1] https://vc.sharif.edu/room' > "${fixture}/com.google.Chrome"
    check 'Sharif VC still matches' 0

    print -r -- 'https://example.invalid/
https://meet.google.com/aaa-bbbb-ccc' > "${fixture}/safari"
    fixture_running=$'com.brave.Browser\ncom.apple.Safari'
    check 'background Meet tab in Safari after another browser fails' 0

    fixture_running=$'org.chromium.Chromium\ncom.google.Chrome'
    check 'timed-out browser does not hide a later meeting' 0
    [[ "$(<"${fixture}/queried")" != *com.microsoft.edgemac* ]] || failed=1

    (( failed )) && exit 1
    print -r -- 'meeting-p: all checks passed'
)
