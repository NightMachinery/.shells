##
function bluetooth-batteries-darwin {
    @darwinOnly

    ioreg -r -l -k BatteryPercent | awk -F'= ' '
/"Product"/        { name=$2; gsub(/^"|"$|\\/, "", name) }
/"BatteryPercent"/ { pct=$2; gsub(/[^0-9].*/, "", pct); if (name && pct != "") printf "%s\t%s%%\n", name, pct }
'
}
##
