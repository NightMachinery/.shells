##
#: Sony headphone battery, via [[https://github.com/NightMachinery/sonyctl][sonyctl]].
#: The alert itself is shared with Bose, in [[file:headphones-battery.zsh]].
#: See [[../../../docs/headphones-battery.md]].
##
function sony-battery {
    : "battery levels, for a human"
    @darwinOnly
    ensure-cmd sonyctl @RET

    sonyctl battery "$@"
}

#: How low the battery is is one fact, restated every time this runs -- on a
#: timer and again on each connect -- so every warning is raised under the same
#: id and rewrites the band in place. Without it, a cron run landing while the
#: connect warning is still up leaves two bands disagreeing about the level.
typeset -g sony_battery_alert_id='sony-battery'

function sony-battery-alert-low {
    : "alerts via Hammerspoon iff the case or a bud is below its threshold

Says nothing at all when the headphones are fine, unreachable, or absent, so it
is safe to run on a timer. Knows nothing about scheduling; drive it from cron."
    local case_min="${sony_battery_case_min:-35}"
    local bud_min="${sony_battery_bud_min:-15}"
    local dur="${sony_battery_alert_dur:-15}"
    local charging_skip_p="${sony_battery_charging_skip_p:-y}"
    local color="${sony_battery_alert_color:-warn}"

    #: A WH- model reports one battery, which counts as a bud here.
    @opts tool sonyctl label Sony alert_id "${sony_battery_alert_id}" \
        case_min "${case_min}" min "${bud_min}" alert_dur "${dur}" \
        charging_skip_p "${charging_skip_p}" alert_color "${color}" \
        @ h-headphones-battery-alert-low
}
##
#: Seconds to wait after the audio device appears before asking for a reading.
#: A2DP comes up before the MDR control service does; sonyctl retries anyway, so
#: this only avoids paying for those retries on every connect.
typeset -g sony_battery_connect_delay="${sony_battery_connect_delay:-3}"

function sony-battery-on-audio-change {
    : "warns about a low battery at the moment you put the headphones on

Consumer of [agfi:h-hook-audio-output-change]. This catches the case a timer
never can: reaching for headphones that are nearly flat, while you can still do
something about it. A poll catches the opposite case, a bud draining mid-use."
    local connect_delay="${sony_battery_connect_delay:-3}"

    local name="${1}"

    #: The hook fires for every output device -- speakers, monitors, other
    #: headsets. Anything else is not ours to report on.
    [[ "$name" == (#i)*(WF-|WH-|1000XM)* ]] || return 0

    sleep "${connect_delay}"
    sony-battery-alert-low
}
##
