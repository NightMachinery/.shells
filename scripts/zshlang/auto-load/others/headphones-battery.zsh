##
#: Low-battery alerts for headphones whose battery only their maker's protocol
#: reports: Sony through [[https://github.com/NightMachinery/sonyctl][sonyctl]],
#: Bose through [[https://github.com/NightMachinery/bosectl][bosectl]]. The brand
#: files, [[file:sony.zsh]] and [[file:bose.zsh]], own the knobs and the
#: triggers; this file is what they share. See [[../../../docs/headphones-battery.md]].
#:
#: macOS exposes no reading for either: `ioreg -k BatteryPercent', which
#: [agfi:bluetooth-batteries-darwin] uses, only reports Apple HID devices
#: (Magic Keyboard, Magic Trackpad). So every reading costs a ~1s Bluetooth
#: session. Both tools print the same battery JSON, a list of parts each with
#: `part', `level_percent' and `charging', so one renderer serves both.
##
function h-headphones-battery-json {
    : "usage: h-headphones-battery-json <sonyctl|bosectl>
batteries as JSON, or fail quietly if they are not reachable"
    local tool="${1}"
    assert-args tool @RET

    #: Deliberately no --auto-connect: waking the headphones to ask how they
    #: are is exactly the side effect exit status 3 exists to prevent. Off or
    #: in the case means nothing to report, and both tools say so in
    #: milliseconds.
    @darwinOnly
    ensure-cmd "${tool}" @RET

    #: Both tools retry a stuck control channel several times, so a wedged one
    #: could otherwise pin a cron job for half a minute.
    reval-timeout 30 "${tool}" --json --color=never battery 2>/dev/null
}

function h-headphones-battery-alert-low {
    : "alerts via Hammerspoon iff a part is below its threshold; else silent
Keywords, set with @opts: tool, label, alert_id, case_min, min, alert_dur,
charging_skip_p, alert_color."
    local tool="${headphones_battery_tool}"
    local label="${headphones_battery_label}"
    local alert_id="${headphones_battery_alert_id}"
    local case_min="${headphones_battery_case_min:-35}"
    local part_min="${headphones_battery_min:-15}"
    local dur="${headphones_battery_alert_dur:-15}"
    local charging_skip_p="${headphones_battery_charging_skip_p:-y}"
    local color="${headphones_battery_alert_color:-warn}"
    assert-args tool label alert_id @RET

    @darwinOnly
    ensure-cmd jq @RET

    local json
    json="$(h-headphones-battery-json "${tool}")" || return 0
    test -n "$json" || return 0

    #: One line carrying every part, marked up for [agfi:hs-alert-v2]'s `md'
    #: mode: the low ones bold, everything else dimmed, so the part you have to
    #: do something about is the part that stands out. Splitting this into a
    #: "what is low" line plus a "what everything is" line only said the same
    #: thing twice.
    #:
    #: A level of null means the tool has no reading -- see
    #: [[../../../docs/headphones-battery.md]]. It renders as NA and never
    #: counts as low: in jq `null < 35' is *true*, so every comparison has to be
    #: guarded, which is what `known' is for.
    #:
    #: The case has its own threshold; every other part (a bud, or the one
    #: battery of over-ear headphones) uses `part_min'. A lone part goes
    #: unnamed, since the line already says whose battery it is. `label' is a
    #: jq keyword, hence `abbr'.
    local prog='
def abbr: {"left":"L","right":"R","case":"case"}[.part] // .part;
def threshold: if .part == "case" then $case_min else $part_min end;
def charging_now: .charging == "yes" or .charging == "complete";
def mark: if .charging == "yes" then "+" elif .charging == "complete" then "=" else "" end;
def known: .level_percent != null;
def low: known
    and (($skip_charging == "y" and charging_now) | not)
    and (.level_percent < threshold);
def show($lone): (if $lone then "" else "\(abbr) " end)
    + (if known then "\(.level_percent)%\(mark)" else "NA" end);
def render($lone): if low then "**\(show($lone))**" else "[\(show($lone))]{dim}" end;
(.batteries | length == 1) as $lone
| if [ .batteries[] | select(low) ] | length == 0 then empty
  else [ .batteries[] | render($lone) ] | join("  ")
  end
'
    local out
    out="$(ec "$json" | jq --raw-output \
        --argjson case_min "${case_min}" \
        --argjson part_min "${part_min}" \
        --arg skip_charging "${charging_skip_p}" \
        "$prog" 2>/dev/null)" || return 0

    #: Nothing low. The overwhelmingly common case, and it must stay silent.
    test -n "$out" || return 0

    #: [agfi:hammerspoon] is `gtimeout 30s hs -A -t 5', so a wedged Hammerspoon
    #: could stall this for half a minute; a warning we cannot deliver must not
    #: become a cron job that never exits.
    reval-timeout 10 @opts dur "${dur}" markup md color "${color}" id "${alert_id}" @ \
        alert "🎧 ${label} battery  ${out}" ||
        ecgray "$0: hs-alert failed; is Hammerspoon responsive?"
}
@opts-setprefix h-headphones-battery-alert-low headphones_battery

function headphones-battery-alert-low {
    : "every brand's low-battery check, for a single cron entry"
    #: One after the other: headphones that are off answer in milliseconds,
    #: and each brand alerts under its own id, so neither hides the other.
    #: A brand that fails must not skip the next, hence no early return.
    local ret=0
    sony-battery-alert-low || ret=$?
    bose-battery-alert-low || ret=$?
    return $ret
}
##
