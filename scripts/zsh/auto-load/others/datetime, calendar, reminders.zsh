# Some vars and `withremc` are defined in configvars
###
function fromnow() {
    local then
    test -n "$date" && then="$(gdate --date "$date" "+%s")" || return 1
    ec $(( EPOCHSECONDS - then ))
}
function fromnow-py() {
    : "date= $0 -> seconds since \$date"

    python3 -c 'import datetime ; from dateutil.parser import parse ; import os
date = parse(os.environ["date"])
print((datetime.datetime.now(date.tzinfo) - date).total_seconds())'
}
function dateshort() { date +"%b %d %H:%M:%S" }
dateshortnum() { date +"%Y/%m/%d" }
datej() {
    # alt: Python https://github.com/fitnr/convertdate
    jalalim tojalali "$(dateshortnum)"
}
##
function remj() {
    (($+commands[jalalim])) || {
        ecerr "$0: jalalim not found."
        return 1
    }

    local now=("${(s./.)$(datej)}")
    local cyear="$now[1]"
    local cmonth="$now[2]"
    local cday="$now[3]"

    local text="$1"
    [[ "$text" == '-' ]] && text="$(</dev/stdin)"
    local day="${2:-$cday}"

    [[ "$day" =~ '^\+(\d+)$' ]] && day=$(( cday + $match[1] ))
    # test -z "$day" && { ecerr "$0: empty first arg. Aborting." ; return 1 }
    typeset -Z2 day="$day"
    local month="${3:-$cmonth}"
    [[ "$month" =~ '^\+(\d+)$' ]] && month=$(( cmonth + $match[1] ))
    [[ "$month" =~ '^\d$' ]] && month="0$month"
    local year="${4}"
    [[ "$year" =~ '^\+(\d+)$' ]] && year=$(( cyear + $match[1] ))
    [[ "$year" =~ '^\d$' ]] && year="140$year"
    [[ "$year" =~ '^\d\d$' ]] && year="14$year"

    if test -n "$year" ; then
        color 50 10 255 "Year set explicitly to $year"
    else
        year="$cyear"
    fi
    local target_date="$year/$month/$day"
    if ! {
               [[ "$day" =~ '^\d\d$' && "$month" =~ '^\d\d$' && "$year" =~ '^\d\d\d\d$' ]] && (( day <= 31 || month <= 12 ))
           } ; then # [[ "$(jalalim togregorian "$target_date")" =~ invalid ]] ||
        ecerr "$0: Invalid date: $target_date"
        return 1
    fi
    local dest="$remindayDir/$target_date $(jalalim togregorian "$target_date"|tr '/' '_')"
    reminday_store "$dest" "$text"
}
function reminday_store() {
    local dest="${1}" text="$2"

    test -z "$dest" && return 1
    test -z "$text" && return 1

    dest="$dest".md
    ensure-dir "$dest" || return 1
    ec "$text"$'\n' >> $dest
    Bold ; color 100 255 200 "$dest : $text" ; resetcolor
    ec $'\n'
    cellp # to sync the reminders
}
function remn() {
    local text="$1" natdate="${@:2}"
    [[ "$text" == '-' ]] && text="$(</dev/stdin)"

    local gdate
    gdate="$(datenat.js $natdate)" || { ecerr "$0: datenat failed for: $natdate" ; return 1 }
    local dest="$remindayDir/$(jalalim tojalali "$gdate") $(<<<$gdate tr '/' '_')"
    reminday_store "$dest" "$text"
}
function rem-todaypaths() {
    unset today

    local now=("${(s./.)$(datej)}")
    local cyear="$now[1]"
    local cmonth="$now[2]"
    local cday="$now[3]"

    today=( "$remindayBakDir/$cyear/$cmonth/$cday"*(N.) "$remindayDir/$cyear/$cmonth/$cday"*(N.) ) # backups should be first, as the normal ones get appended to them (and so can cause duplicates if they come first)
}
function rem_extract-date-path() {
    local f="$(realpath "$1")"
    if [[ "$f" =~ '(\d\d\d\d/\d\d/\d\d.*)' ]] ; then
        ec "$match[1]"
    else
        ecerr "Couldn't get date path components of: $f"
        return 1
    fi
}
function rem-today() {
    local deleteMode="${rem_today_delete:-$rem_today_d}"

    ensure-dir "$remindayDir/"
    ensure-dir "$remindayBakDir/"

    local today
    rem-todaypaths

    local text=""
    local f bak
    for f in $today[@] ; do
        if test -e "$f" ; then
            text="$text"$'\n\n'"$(<$f)" #"${$(<$f ; ec .)[1,-2]}"
            if test -n "$deleteMode" ; then
                bak="$(rem_extract-date-path "$f")"
                bak="${remindayBakDir}/$bak"
                serr append-f2f "$f" "$bak" && command rm "$f" # not deleting if the source is the same as the dest
            fi
        fi
    done
    rmdir-empty "$remindayDir"

    trim "$text"
}
function remc-today() {
    withremc rem-today "$@"
}
@opts-setprefix remc-today rem-today

function rem-today-notify() {
    ensure-dir ~/logs/
    {
        ec "---"
        date
        cellp
        local text="$(rem-today ; ec ; remc-today)"
        if test -n "$text" ; then
            reval-ec terminal-notifier -title "$(datej)" -message "$text"
        fi
        ec "---"
    } >> ~/logs/rem-today-notify.txt  2>&1 | cat
}
function tlg-reminday() {
    local rec="$1"
    test -z "$rec" && {
        ecerr "$0: Empty receiver."
        return 1
    }

    local text="$(@opts delete y @ rem-today)"
    if test -n "$text" ; then
        tnotif "$text"
    fi
    local textc="$(@opts delete y @ remc-today)"
    if test -n "$textc" ; then
        text+=$'\n\n'"$textc"
        tnotifc "$textc"
    fi
    text="$text"$'\n\n'"$(datej) $(date +"%A %B %d")"
    tsend --parse-mode markdown -- "$rec" "$text"
}
##
aliasfn remcn withremc remn
aliasfn remcj withremc remj
##
remnd() {
    # Example: `remcnd "⛸ 🚪 Don't put the shoes behind the door" 1 3 7 20 60 360`
    local d msg="$1" ; shift
    for d in "$@" ; do
        fnswap cellp true remn "$msg" "$d day later"
    done
    cellp
}
aliasfn remcnd withremc remnd
##
