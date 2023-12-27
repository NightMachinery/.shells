#!/opt/homebrew/bin/bash
# #!/usr/bin/env bash
# * @forkedFrom https://xbarapp.com/docs/plugins/Time/date-picker.1m.sh.html
# * @usage
# ** `lnrp "${NIGHTDIR}/zshlang/menubar/date.sh" ~/'Library/Application Support/xbar/plugins/date.1m.bash'`
# *** would refresh every minute (=1m=)
# *** would refresh every hour (=1h=)
## * @bootstrap
# Display todays date and time in various formats including ISO8601 and allows copying to clipboard.

# Comment out the dates you don't need.

# <xbar.title>Date Picker</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Tim Battersby</xbar.author>
# <xbar.author.github>uglygus</xbar.author.github>
# <xbar.desc>Display todays date in various forms including iso8601 and copies to the clipboard.</xbar.desc>
# <xbar.dependencies></xbar.dependencies>
# <xbar.image>https://i.imgur.com/GVSUqFX.png</xbar.image>

source ~/.bashrc
export PATH="$PATH:/usr/local/bin"

## ** =lastunlock=
# function fromnow {
#     local then
#     then="$(gdate --date "$date" "+%s")" || return 1
#     echo $(( EPOCHSECONDS - then ))
# }

# function lastunlock-get {
#     # Using lower precision helps a lot with performance
#     local precision="${1:-2h}" # can only spot the last unlock in this timeframe

#     unset date
#     date="$(command log show --style syslog --predicate 'process == "loginwindow"' --debug --info --last "$precision" | command rg "going inactive, create activity semaphore|releasing the activity semaphore" | tail -n1 |cut -c 1-31)" || {
#         # This means the last login was before the precision set
#         echo 9999998
#         return 0
#     }
#     >&2 echo "date: $date"

#     date="$date" fromnow || {
#         echo 9999999
#         return 1
#     }
# }
# function lastunlock-get-min {
#     echo $(( $(lastunlock-get "$@") / 60 ))
# }
## * menubar
# Appears in the menubar YYYY-MM-DD
printf '%d ¦ %s\n' "$(brishz.dash last-idle-get-min)" "$(date "+%b%-m/%d")"
# printf '%02d ¦ %s\n' "$(brishz.dash last-idle-get-min)" "$(date "+%b%-m/%d")"
echo "---"

#---IR

MDY="$(brishz.dash now)"
echo "$MDY | bash='$0' | param1=copy | param2='$MDY' | terminal=false"

echo "---"

#---ISO8601

YMD=$(date +%F)
echo "$YMD | bash='$0' | param1=copy | param2=$YMD | terminal=false"

DATETIME=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
echo "$DATETIME |bash='$0' param1=copy param2=$DATETIME terminal=false"

echo "---"

#---USA

MDY=$(date "+%D")
echo "$MDY |bash='$0' param1=copy param2=$MDY terminal=false"

MonDY=$(date +"%b %d %Y")
echo "$MonDY |bash='$0' param1=copy param2=\"$MonDY\" terminal=false"

TIME12=$(date +"%r")
echo "$TIME12 |bash='$0' param1=copy param2=\"$TIME12\" terminal=false"

echo "---"


#---REST OF THE WORLD

DMY=$(date +"%d/%m/%y")
echo "$DMY |bash='$0' param1=copy param2=$YMD terminal=false"

DMonY=$(date +"%d %b %Y")
echo "$DMonY |bash='$0' param1=copy param2=\"$DMonY\" terminal=false"

TIMESTAMP=$(date +"%T %D")
echo "$TIMESTAMP |bash='$0' param1=copy param2=\"$TIMESTAMP\" terminal=false"


TIME24=$(date +"%R:%S")
echo "$TIME24 |bash='$0' param1=copy param2=\"$TIME24\" terminal=false"



if [[ "$#" -ge 1 ]];then
    if [[ "$1" == 'copy' ]] ; then

        echo -n "$2" | pbcopy
        echo COPIED "$2"
    fi
fi
