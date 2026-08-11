#!/usr/bin/env zsh
# Install (or remove) the LaunchAgent that mutes the output device when audio
# would be leaking into the shared office and nobody is at the desk.
#
#   ./install-audio-guard.zsh              # install / re-install (idempotent)
#   ./install-audio-guard.zsh --uninstall
#   ./install-audio-guard.zsh --check      # report state, change nothing
#
# The logic itself is the zsh function audio-guard-tick, in
# zshlang/auto-load/others/audio-guard.zsh; see ../../docs/audio-guard.md.
#
# No sudo anywhere: this is a per-user LaunchAgent, unlike the LaunchDaemon in
# ../pf/. Running it with sudo would resolve ~ to /var/root and install the job
# for the wrong user.
#
# Not placed directly in launchers/, whose executables are auto-run at every
# boot by zshlang/launch.zsh -- this is run once, by hand.

emulate -L zsh
set -o pipefail

LABEL='com.user.audio-guard'
SRC_DIR="${0:A:h}"
PLIST_SRC="${SRC_DIR}/${LABEL}.plist"
PLIST_DST="${HOME}/Library/LaunchAgents/${LABEL}.plist"
PAYLOAD="${HOME}/scripts/zshlang/wrappers/audio-guard-tick.dash"
LOG_FILE="${HOME}/logs/audio-guard.log"
DOMAIN="gui/$(id -u)"

# --- output helpers -----------------------------------------------------------
_info() { print -r -- "  $*" }
_step() { print -r -- $'\n''=> '"$*" }
_warn() { print -ru2 -- "!! $*" }
_die()  { print -ru2 -- "!! $*"$'\n''!! aborted; nothing further changed.'; exit 1 }

_preflight() {
    (( EUID == 0 )) && _die "run this as your normal user, not with sudo."
    [[ -r "$PLIST_SRC" ]] || _die "plist not found: ${PLIST_SRC}"
    [[ -x "$PAYLOAD" ]]   || _die "payload missing or not executable: ${PAYLOAD}"

    # The payload's shebang is #!/usr/bin/env dash, and the plist's PATH must be
    # able to find it. Catch that here rather than via a job that silently never
    # runs.
    local dash_path
    dash_path="$(command -v dash)" || _die 'dash not found on PATH.'
    if ! command grep -q -- "${dash_path:h}" "$PLIST_SRC" ; then
        _warn "dash lives in ${dash_path:h}, which is not in the plist's PATH."
        _warn "the job would fail to launch. fix EnvironmentVariables/PATH in:"
        _die  "  ${PLIST_SRC}"
    fi
}

_check() {
    _step "state of ${LABEL}"

    if [[ -e "$PLIST_DST" ]] ; then
        _info "plist installed: ${PLIST_DST}"
        if command diff -q "$PLIST_SRC" "$PLIST_DST" >/dev/null 2>&1 ; then
            _info 'plist matches the copy in this repo.'
        else
            _warn 'installed plist DIFFERS from this repo; re-run the installer.'
        fi
    else
        _info 'plist not installed.'
    fi

    if launchctl print "${DOMAIN}/${LABEL}" >/dev/null 2>&1 ; then
        _info 'job is bootstrapped. state and last exit status:'
        launchctl print "${DOMAIN}/${LABEL}" |
            command grep -E '^\s+(state|last exit code|program|run interval) ' |
            command sed 's/^/    /'
    else
        _info 'job is NOT bootstrapped.'
    fi

    if [[ -s "$LOG_FILE" ]] ; then
        _info "log tail (${LOG_FILE}):"
        command tail -n 5 "$LOG_FILE" | command sed 's/^/    /'
    else
        _info "log is empty or absent: ${LOG_FILE}"
    fi
}

_install() {
    _preflight

    _step 'installing the plist'
    command mkdir -p "${PLIST_DST:h}" "${LOG_FILE:h}" || _die 'could not create target directories.'
    command cp -f "$PLIST_SRC" "$PLIST_DST" || _die 'could not copy the plist.'
    # Copied rather than symlinked: launchd has historically been unreliable with
    # symlinked plists, and ~/Library/LaunchAgents is not tracked by vcsh anyway.
    _info "copied to ${PLIST_DST}"

    command plutil -lint "$PLIST_DST" >/dev/null || _die 'the installed plist does not parse.'

    _step 'bootstrapping the job'
    # bootout first so a re-run picks up an edited plist; it fails harmlessly
    # when the job was not loaded, which is the normal first-install case.
    launchctl bootout "${DOMAIN}/${LABEL}" 2>/dev/null
    launchctl bootstrap "$DOMAIN" "$PLIST_DST" ||
        _die "launchctl bootstrap failed. try: launchctl print ${DOMAIN}/${LABEL}"
    _info 'bootstrapped.'

    _step 'done'
    _info 'RunAtLoad means the first tick has already fired.'
    _info 'inspect with:  audio-guard-status'
    _info "or:            ./install-audio-guard.zsh --check"
    _warn 'run brishz-restart after editing audio-guard.zsh, or the job keeps'
    _warn 'executing whatever the garden loaded at startup.'
}

_uninstall() {
    _step 'removing the job'
    if launchctl bootout "${DOMAIN}/${LABEL}" 2>/dev/null ; then
        _info 'booted out.'
    else
        _info 'was not loaded.'
    fi

    if [[ -e "$PLIST_DST" ]] ; then
        command rm -f "$PLIST_DST" || _die "could not remove ${PLIST_DST}"
        _info "removed ${PLIST_DST}"
    else
        _info 'no plist to remove.'
    fi

    _step 'done'
    _info 'any mute the guard was holding is untouched; run audio-guard-restore'
    _info 'if it currently holds one (check with audio-guard-status).'
}

case "${1:-}" in
    --uninstall) _uninstall ;;
    --check)     _check ;;
    ''|--install) _install ;;
    *)
        _die "unknown argument: ${1}. use --install, --uninstall or --check."
        ;;
esac
