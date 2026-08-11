##
#: Mute the output device when audio would be leaking into the shared office and
#: nobody is at the desk. See [[../../../docs/audio-guard.md]].
#:
#: Three triggers, each independently switchable; only `idle' ships enabled.
#: The social question itself lives in [agfi:office-public-audio-p].
##
typeset -g audio_guard_idle_t="${audio_guard_idle_t:-3600}"
typeset -g audio_guard_back_t="${audio_guard_back_t:-60}"
typeset -g audio_guard_notif_p="${audio_guard_notif_p:-y}"
typeset -g audio_guard_snooze_default="${audio_guard_snooze_default:-2h}"
#: Off because the screen lock always beats the idle threshold here, so the
#: unlock hook already covers it. See the doc for the one case that it misses.
typeset -g audio_guard_restore_at_tick_p="${audio_guard_restore_at_tick_p:-n}"

typeset -ga audio_guard_triggers=( idle lock headphones )
typeset -gA audio_guard_trigger_defaults=( idle y lock n headphones n )

() {
    #: Anonymous function purely to keep the loop variable out of the global scope.
    local t
    for t in "$audio_guard_triggers[@]" ; do
        redis-defvar "audio_guard_trigger_${t}"
    done
}
#: Holds the NAME of the device we muted; empty means we hold no claim.
redis-defvar audio_guard_muted
##
function h-audio-guard-trigger-assert {
    local t="${1}"
    assert-args t @RET

    if (( ${audio_guard_triggers[(Ie)$t]} == 0 )) ; then
        ectrace "$0: unknown trigger: ${t} (known: ${audio_guard_triggers[*]})"
        return 1
    fi
}

function audio-guard-trigger-p {
    : "returns 0 iff trigger <name> is enabled, by redis override or by default"
    local t="${1}"
    h-audio-guard-trigger-assert "$t" @RET

    local v
    v="$(reval "audio_guard_trigger_${t}_get" 2>/dev/null)" || v=''
    test -n "$v" || v="${audio_guard_trigger_defaults[$t]}"

    bool "$v"
}

function h-audio-guard-trigger-set {
    local v="${1}" ; shift
    local ts=( "$@" )
    #: No argument means every trigger, so a bare [agfi:audio-guard-disable] is
    #: the master off switch and no separate master flag is needed.
    (( ${#ts} )) || ts=( "$audio_guard_triggers[@]" )

    local t
    for t in "$ts[@]" ; do
        h-audio-guard-trigger-assert "$t" @RET
        reval "audio_guard_trigger_${t}_set" "$v"
        ecgray "audio-guard: ${t} -> ${v}"
    done
}
aliasfn audio-guard-enable h-audio-guard-trigger-set y
aliasfn audio-guard-disable h-audio-guard-trigger-set n

function audio-guard-auto {
    : "clears the override on <trigger...>, returning them to the shipped default"
    local ts=( "$@" )
    (( ${#ts} )) || ts=( "$audio_guard_triggers[@]" )

    local t
    for t in "$ts[@]" ; do
        h-audio-guard-trigger-assert "$t" @RET
        reval "audio_guard_trigger_${t}_del"
        ecgray "audio-guard: ${t} -> auto (${audio_guard_trigger_defaults[$t]})"
    done
}

function audio-guard-toggle {
    local ts=( "$@" )
    (( ${#ts} )) || ts=( "$audio_guard_triggers[@]" )

    local t
    for t in "$ts[@]" ; do
        h-audio-guard-trigger-assert "$t" @RET
        if audio-guard-trigger-p "$t" ; then
            audio-guard-disable "$t"
        else
            audio-guard-enable "$t"
        fi
    done
}
##
function h-audio-guard-dur2sec {
    : "converts 90s, 30m, 2h or a bare seconds count to seconds"
    local d="${1}"
    assert-args d @RET

    local n="${d%[smh]}" unit="${d##*[0-9]}"
    if [[ "$n" != <-> ]] ; then
        ectrace "$0: bad duration: ${d}"
        return 1
    fi

    case "$unit" in
        h) ec $(( n * 3600 )) ;;
        m) ec $(( n * 60 )) ;;
        s|'') ec "$n" ;;
        *)
            ectrace "$0: bad duration unit: ${d}"
            return 1
            ;;
    esac
}

function audio-guard-snooze {
    : "suppresses every trigger for <dur>, default 2h

Expiring by itself is the point: an off switch you have to remember to undo is
one you will find still off next month."
    local dur="${1:-${audio_guard_snooze_default}}"

    local secs
    secs="$(h-audio-guard-dur2sec "$dur")" @RET

    silent redism setex audio_guard_snooze "$secs" y @RET
    ecgray "$0: snoozed for $(seconds-fmt-short "$secs")"
}
aliasfn audio-guard-unsnooze silent redism del audio_guard_snooze

function audio-guard-snooze-p {
    local v
    v="$(redism get audio_guard_snooze 2>/dev/null)" || v=''
    test -n "$v"
}
##
function h-audio-guard-device-name {
    : "name of the current default output device"
    local out
    out="$(audio-output-get 2>/dev/null)" || return 1

    local lines=( "${(@f)out}" )
    print -r -- "${lines[1]}"
}

function h-audio-guard-reconcile {
    : "drops our claim once the user has undone the mute themselves

Ownership is the only thing stopping [agfi:audio-guard-restore] from unmuting a
mute the user set deliberately, and ownership goes stale: we mute, the user
unmutes by hand, the user later mutes again for their own reason, and the next
restore clobbers it. Seeing the device unmuted while we still hold the claim is
proof the mute is no longer ours.

Touches only our own bookkeeping, never the mute state, so it is always safe."
    local held
    held="$(audio_guard_muted_get 2>/dev/null)" || held=''
    test -n "$held" || return 0

    local cur="${1}"
    test -n "$cur" || cur="$(h-audio-guard-device-name)" || return 0

    #: Only meaningful while the device we muted is still the default: the volume
    #: keys act on the default device, so that is the only one the user can
    #: plausibly have unmuted by hand.
    [[ "$held" == "$cur" ]] || return 0

    if ! volume-mute-p ; then
        ecgray "$0: ${held} was unmuted by hand; dropping our claim."
        audio_guard_muted_del
    fi
}
#: Public name because hammerspoon/core/audio-watcher.lua calls this from
#: outside; the h- prefix marks helpers internal to this file.
aliasfn audio-guard-reconcile h-audio-guard-reconcile

function h-audio-guard-act-p {
    : "the gate shared by every trigger: may we mute right now?

Deliberately excludes the idle test, which belongs to the idle trigger alone.
Takes the same optional <name> <transport> as [agfi:office-public-audio-p]."
    isDarwin || return 1
    isLocal || return 1

    if audio-guard-snooze-p ; then
        ecgray "$0: snoozed."
        return 1
    fi

    office-public-audio-p "$@" || return 1

    #: A mute we did not place is not ours to track. Never recording it as ours
    #: is what keeps restore from ever touching a deliberate mute.
    if volume-mute-p ; then
        return 1
    fi

    return 0
}

function h-audio-guard-mute {
    local reason="${1}" name="${2}"
    test -n "$name" || name="$(h-audio-guard-device-name)" || name=''

    volume-mute @RET

    #: Not every output device supports software mute -- a DisplayPort monitor
    #: typically reports a nil volume and simply ignores the request. Claiming a
    #: mute that never happened would be worse than not muting: the next restore
    #: would unmute a device we never touched. Verify before claiming.
    if ! volume-mute-p ; then
        ecerr "$0: ${name:-the output device} ignored the mute request; not claiming it."
        if bool "${audio_guard_notif_p}" ; then
            notif_tlg=n notif "Audio guard could NOT mute ${name:-the output device}"
        fi
        return 1
    fi

    audio_guard_muted_set "$name"

    ecdate "$0: muted ${name:-the output device} (${reason})"
    if bool "${audio_guard_notif_p}" ; then
        notif_tlg=n notif "Audio muted: ${reason}"
    fi
}

function h-audio-guard-unmute-device {
    : "unmutes the output device called <name>

By name rather than by 'the default device' on purpose: macOS mute is per-device
and persistent, so unmuting the wrong one leaves the speakers silently muted
until you next trip over it."
    local name="${1}"
    assert-args name @RET

    local cur
    cur="$(h-audio-guard-device-name 2>/dev/null)" || cur=''
    if [[ "$name" == "$cur" ]] ; then
        volume-unmute
        return $?
    fi

    silence hammerspoon -c "local d = hs.audiodevice.findOutputByName([[${name}]]) ; if d then d:setOutputMuted(false) ; return true else return false end"
}

function audio-guard-restore {
    : "unmutes only a mute this guard placed; a no-op otherwise

Not gated on any trigger: a mute we placed must stay reversible even if you
disabled the trigger that placed it in the meantime."
    local held
    held="$(audio_guard_muted_get 2>/dev/null)" || held=''
    test -n "$held" || return 0

    h-audio-guard-unmute-device "$held"
    audio_guard_muted_del

    ecdate "$0: restored ${held}."
    if bool "${audio_guard_notif_p}" ; then
        notif_tlg=n notif "Audio unmuted: ${held}"
    fi
}
##
function audio-guard-tick {
    : "the launchd payload; installed from launchers/audio-guard/"
    isDarwin || return 0
    isLocal || return 0

    #: Resolve the device with the non-Hammerspoon backend and pass it down, so
    #: the periodic job never touches Hammerspoon's single Lua thread. The ~200ms
    #: is free in a background job; a main-thread stall never is.
    local out
    out="$(audio-output-get-system-profiler 2>/dev/null)" || out=''
    local lines=( "${(@f)out}" )
    local name="${lines[1]}" transport="${lines[2]}"

    h-audio-guard-reconcile "$name"

    local idle
    idle="$(idle-get)" || return 0

    if bool "${audio_guard_restore_at_tick_p}" && (( idle < audio_guard_back_t )) ; then
        audio-guard-restore
        return 0
    fi

    audio-guard-trigger-p idle || return 0
    (( idle >= audio_guard_idle_t )) || return 0

    h-audio-guard-act-p "$name" "$transport" || return 0

    h-audio-guard-mute "office, no headphones, idle $(seconds-fmt-short "${idle%.*}")" "$name"
}

function audio-guard-on-lock {
    : "screen-lock trigger; disabled by default"
    audio-guard-trigger-p lock || return 0
    h-audio-guard-act-p || return 0

    h-audio-guard-mute "office, no headphones, screen locked"
}

function audio-guard-on-audio-change {
    : "output-device-change trigger; disabled by default

Called by hammerspoon/core/audio-watcher.lua with the new device's name and
transport, so the zsh side never calls back into Hammerspoon to rediscover what
the Lua callback already knew."
    local name="${1}" transport="${2}"

    audio-guard-trigger-p headphones || return 0
    h-audio-guard-act-p "$name" "$transport" || return 0

    h-audio-guard-mute "output switched to ${name:-a speaker}" "$name"
}
##
function audio-guard-status {
    : "shows every input this guard acts on, and what it currently believes

The only window into a job you never watch run; modelled on [agfi:office-p-explain]."
    local t v eff
    ec "triggers:"
    for t in "$audio_guard_triggers[@]" ; do
        v="$(reval "audio_guard_trigger_${t}_get" 2>/dev/null)" || v=''
        audio-guard-trigger-p "$t" && eff=enabled || eff=disabled
        ec "  ${t}: ${eff} (default ${audio_guard_trigger_defaults[$t]}, override ${v:-<unset>})"
    done

    local snooze_ttl
    snooze_ttl="$(redism ttl audio_guard_snooze 2>/dev/null)" || snooze_ttl=''
    if audio-guard-snooze-p ; then
        ec "snoozed: yes (${snooze_ttl}s left)"
    else
        ec "snoozed: no"
    fi

    local held
    held="$(audio_guard_muted_get 2>/dev/null)" || held=''
    ec "our mute: ${held:-<none>}"

    ec "idle: $(idle-get)s (threshold ${audio_guard_idle_t}s)"
    ec "output: $(audio-output-get 2>/dev/null | prefixer -o ' / ' --skip-empty)"
    headphones-p && ec "headphones: yes" || ec "headphones: no"
    office-p && ec "office: yes" || ec "office: no"
    office-public-audio-p && ec "public audio: yes" || ec "public audio: no"
    volume-mute-p && ec "muted now: yes" || ec "muted now: no"
}
aliasfn audio-guard-explain audio-guard-status
##
