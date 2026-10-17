##
#: Bose headphone battery, via [[https://github.com/NightMachinery/bosectl][bosectl]].
#: The alert itself is shared with Sony, in [[file:headphones-battery.zsh]].
#: See [[../../../docs/headphones-battery.md]].
##
function bose-battery {
    : "battery level, for a human"
    @darwinOnly
    ensure-cmd bosectl @RET

    bosectl battery "$@"
}

#: One id for every warning, as for Sony: the timer and the connect hook
#: restate the same fact, so a later warning rewrites the band in place.
typeset -g bose_battery_alert_id='bose-battery'

function bose-battery-alert-low {
    : "alerts via Hammerspoon iff the headphones' battery is below its threshold

Says nothing at all when the headphones are fine, unreachable, or off, so it is
safe to run on a timer. Knows nothing about scheduling; drive it from cron."
    local min="${bose_battery_min:-15}"
    local dur="${bose_battery_alert_dur:-15}"
    local charging_skip_p="${bose_battery_charging_skip_p:-y}"
    local color="${bose_battery_alert_color:-warn}"

    @opts tool bosectl label Bose alert_id "${bose_battery_alert_id}" \
        min "${min}" alert_dur "${dur}" \
        charging_skip_p "${charging_skip_p}" alert_color "${color}" \
        @ h-headphones-battery-alert-low
}
##
#: Seconds to wait after the audio device appears before asking for a reading.
#: The BMAP channel can refuse the first open right after the link comes up;
#: bosectl retries anyway, so this only avoids paying for those retries.
typeset -g bose_battery_connect_delay="${bose_battery_connect_delay:-3}"

function bose-battery-on-audio-change {
    : "warns about a low battery at the moment you put the headphones on

Consumer of [agfi:h-hook-audio-output-change]; see [agfi:sony-battery-on-audio-change]
for why a connect hook and a timer are both needed."
    local connect_delay="${bose_battery_connect_delay:-3}"

    local name="${1}"

    #: The hook fires for every output device. Bose names its headphones
    #: "Bose ..." out of the box; one renamed without the word is not caught
    #: here, though the timer still covers it.
    [[ "$name" == (#i)*bose* ]] || return 0

    sleep "${connect_delay}"
    bose-battery-alert-low
}
##
