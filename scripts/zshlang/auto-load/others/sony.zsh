##
#: Sony headphone battery, via [[https://github.com/NightMachinery/sonyctl][sonyctl]].
#: See [[../../../docs/sony-battery.md]].
#:
#: macOS exposes no battery reading for these at all: `ioreg -k BatteryPercent',
#: which [agfi:bluetooth-batteries-darwin] uses, only reports Apple HID devices
#: (Magic Keyboard, Magic Trackpad). The MDR protocol is the only source, so
#: every reading here costs a ~1s Bluetooth session.
##
typeset -g sony_battery_case_min="${sony_battery_case_min:-35}"
typeset -g sony_battery_bud_min="${sony_battery_bud_min:-15}"
typeset -g sony_battery_alert_dur="${sony_battery_alert_dur:-3}"
#: Charging means the number is on its way up, so warning about it is noise.
typeset -g sony_battery_charging_skip_p="${sony_battery_charging_skip_p:-y}"
##
function sony-battery {
    : "battery levels, for a human"
    @darwinOnly
    ensure-cmd sonyctl @RET

    sonyctl battery
}

function h-sony-battery-json {
    : "batteries as JSON, or fail quietly if they are not reachable

Deliberately no --auto-connect: waking the headphones to ask how they are is
exactly the side effect sonyctl exit code 3 exists to prevent. Off or in the
case means nothing to report, and sonyctl says so in milliseconds."
    @darwinOnly
    ensure-cmd sonyctl @RET

    #: sonyctl retries the handshake three times at 8s each, so a wedged control
    #: channel could otherwise pin a cron job for half a minute.
    reval-timeout 30 sonyctl --json --color=never battery 2>/dev/null
}

function sony-battery-alert-low {
    : "alerts via Hammerspoon iff the case or a bud is below its threshold

Says nothing at all when the headphones are fine, unreachable, or absent, so it
is safe to run on a timer. Knows nothing about scheduling; drive it from cron."
    local case_min="${sony_battery_case_min:-35}"
    local bud_min="${sony_battery_bud_min:-15}"
    local dur="${sony_battery_alert_dur:-3}"
    local charging_skip_p="${sony_battery_charging_skip_p:-y}"

    @darwinOnly
    ensure-cmd jq @RET

    local json
    json="$(h-sony-battery-json)" || return 0
    test -n "$json" || return 0

    #: One pass produces both lines of the message: the parts that are actually
    #: low, then the full picture for context. The second is skipped when every
    #: part is already in the first, where it would only repeat itself.
    #: `label' is a jq keyword, hence `abbr'.
    local prog='
def abbr: {"left":"L","right":"R","case":"case","battery":"bat"}[.part] // .part;
def threshold: if .part == "case" then $case_min else $bud_min end;
def charging_now: .charging == "yes" or .charging == "complete";
def mark: if .charging == "yes" then "+" elif .charging == "complete" then "=" else "" end;
def show: "\(abbr) \(.level_percent)%\(mark)";
[ .batteries[] ] as $known
| [ $known[]
    | select( ($skip_charging == "y" and charging_now) | not )
    | select(.level_percent < threshold) ] as $low
| if ($low | length) == 0 then empty
  else
    ( $low | map(show) | join(", ") ),
    ( if ($known | length) > ($low | length)
      then ($known | map(show) | join("  ")) else empty end )
  end
'
    local out
    out="$(ec "$json" | jq --raw-output \
        --argjson case_min "${case_min}" \
        --argjson bud_min "${bud_min}" \
        --arg skip_charging "${charging_skip_p}" \
        "$prog" 2>/dev/null)" || return 0

    #: Nothing low. The overwhelmingly common case, and it must stay silent.
    test -n "$out" || return 0

    local lines=( "${(@f)out}" )
    local msg="🎧 Sony battery low: ${lines[1]}"
    test -z "${lines[2]}" || msg="${msg}"$'\n'"${lines[2]}"

    #: [agfi:hammerspoon] is `gtimeout 30s hs -A -t 5', so a wedged Hammerspoon
    #: could stall this for half a minute; a warning we cannot deliver must not
    #: become a cron job that never exits.
    silence reval-timeout 10 @opts dur "${dur}" @ alert "$msg" ||
        ecgray "$0: hs-alert failed; is Hammerspoon responsive?"
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
