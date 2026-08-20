##
function lock-security {
    killall gpg-agent || true
}
##
function h-hook-deluna {
    # Currently implemented via [agfi:deluna], which actually runs it when the computer is unlocked, and also runs it multiple times and when idle etc.
    ##
    lock-security
}
##
function h-hook-lock {
    #: [[id:597345ed-90e6-491b-9c70-43db87bc707d][@good lock_watcher.swift]]
    ##
    h-lunaquit-force-skip-reset

    lock-security

    #: Backgrounded on purpose. [agfi:hammerspoon] is `gtimeout 30s hs -A -t 5',
    #: so a wedged Hammerspoon would otherwise stall locking the screen for
    #: seconds. Disabled by default; see [agfi:audio-guard-trigger-p].
    awaysh-fast audio-guard-on-lock
}

function h-hook-unlock {
    #: [[id:597345ed-90e6-491b-9c70-43db87bc707d][@good lock_watcher.swift]]
    #: @old [agfi:deluna]
    ##
    lock-security

    reval-ecdate last-idle-reset
    reval-ecdate luna-skip-reset

    #: Not gated on any trigger: a mute the guard placed must stay reversible
    #: even if the trigger that placed it has since been turned off. This is the
    #: primary restore path -- the tick deliberately never unmutes.
    reval-ecdate audio-guard-restore

    if false ; then
        # ec $'\n\n'"$0" | sync-append-in "${KARABINER_RESET_LOG}"
        ec $'\n\n'"$0" >> "${KARABINER_RESET_LOG}" @STRUE
        #: I don't want potential lock issues to prevent us from running this.

        karabiner-reset
    fi

    battery-charge-limit-restore-status
    #: If the laptop's battery dies and we turn it on again, our restart hooks won't run but the limit would be reset.
}
##
function h-hook-audio-output-change {
    : "fired when the default audio output device changes

Called by hammerspoon/core/audio-watcher.lua with the new device's name and
transport, already debounced and filtered to dOut events.

The fan-out lives here rather than in the Lua because hs.audiodevice.watcher is
a module-level singleton with a single callback slot, which audio-watcher.lua
has already claimed. A second consumer calling setCallback would silently
replace it and disable the audio guard, so consumers are added here instead.

Each consumer is isolated: one that fails, or blocks on something slow, must
not stop the others from running."
    local name="${1}" transport="${2}"

    awaysh-fast audio-guard-on-audio-change "$name" "$transport"
    awaysh-fast sony-battery-on-audio-change "$name" "$transport"
}
##
function tealy-connect-hook {
    # fsay "Tealy connected"

    darwin-net-static-set
    sleep 5
    darwin-net-static-set
}

function wifi-disconnect-hook {
    darwin-net-static-unset
    sleep 5
    darwin-net-static-unset
}

function wifi-unknown-connect-hook {
    darwin-net-static-unset
    sleep 5
    darwin-net-static-unset
}
##
