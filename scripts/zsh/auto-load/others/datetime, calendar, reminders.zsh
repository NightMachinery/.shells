# Some vars are defined in configvars
###
export remindayCDir="$cellar/remindersC"
export remindayBakCDir="$HOME/tmp/remindersC_bak"
alias withremc='remindayDir="$remindayCDir" remindayBakDir="$remindayBakCDir" '

aliasfn remcnd withremc remnd
aliasfn remcn withremc remn
aliasfn remcj withremc remj
aliasfn remc-fz withremc rem-fz
aliasfn remc remc-fz
aliasfn remcd withremc remd
function remc-today() {
    withremc rem-today "$@"
}
@opts-setprefix remc-today rem-today
aliasfn remc-today-d withremc rem-today-d
aliasfn remc-comingup withremc rem-comingup
##
function rem-comingup() {
    local out='' i pre

    for i in {1..7} ; do
        pre="$i day(s) later:"
        out+="$(prefix-if-ne $'\n\n'"$pre"$'\n' "$(rem-today-d "$i" | prefixer -a '  ')")"
    done

    # out="$(prefix-if-ne "Coming up:"$'\n' "$out")"
    trim "$out"
}
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
    local dest="${1}" text="$(trim "$2")"

    test -z "$dest" && return 1
    test -z "$text" && return 1

    dest="$dest".md
    ensure-dir "$dest" || return 1
    ec "$text" >> $dest
    Bold ; color 100 255 200 "$dest : $text" ; resetcolor
    ec $'\n'
    cellp # to sync the reminders
}
function datenatj() {
    : "GLOBAL OUT: datenatj_date datenatj_datej"

    unset datenatj_date
    unset datenatj_datej
    local natdate="$*"

    local gdate
    gdate="$(datenat.js $natdate)" || { ecerr "$0: datenat failed for: $natdate" ; return 1 }
    datenatj_date="$gdate"
    datenatj_datej="$(jalalim tojalali "$gdate")"
    ecn "$datenatj_datej"
}
function remn() {
    local text="$1" natdate="${@:2}"
    [[ "$text" == '-' ]] && text="$(</dev/stdin)"
    local jdate
    sout datenatj "$natdate" || return 1
    jdate="$datenatj_datej"
    local dest="$remindayDir/$jdate $(<<<$datenatj_date tr '/' '_')"
    reminday_store "$dest" "$text"
}
function rem-todaypaths() {
    unset today

    # FNSWAP: datej
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
function rem-today-d() {
    local day="${1:-1}"
    fnrep datej "datenatj $day day later" rem-today
}
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
    local text="$(@opts delete y notif y @ rem-summary)"
    text="$text"$'\n\n'"$(datej-all)"
    tsend --parse-mode markdown -- "$rec" "$text"
}
function rem-summary() {
    local deleteMode="$rem_summary_delete" notifMode="$rem_summary_notif"

    local text="$(@opts delete "$deleteMode" @ rem-today)"
    if test -n "$text" && test -n "$notifMode" ; then
        tnotif "$text" >&2
    fi
    local textc="$(@opts delete "$deleteMode" @ remc-today)"
    text+="$(prefix-if-ne $'\n\n' "$textc")"
    if test -n "$textc" && test -n "$notifMode" ; then
        tnotifc "$textc" >&2
    fi
    text+="$(prefix-if-ne $'\n\n' "$(rem-comingup)")"
    trim "$text"
}
function monthj2en() {
    local m=$(( ${1:?Month required} ))

    case "$m" in
        1) ec Farvardin;;
        2) ec Ordibehesht;;
        3) ec Khordad;;
        4) ec Tir;;
        5) ec Mordad;;
        6) ec Shahrivar;;
        7) ec Mehr;;
        8) ec Aban;;
        9) ec Azar;;
        10) ec Dey;;
        11) ec Bahman;;
        12) ec Esfand;;
        *) ecerr "$0: Invalid month: $1";;
    esac
}
function monthj2fa() {
    local m=$(( ${1:?Month required} ))

    case "$m" in
        1) ec فروردین;;
        2) ec اردیبهشت;;
        3) ec خرداد;;
        4) ec تیر;;
        5) ec مرداد;;
        6) ec شهریور;;
        7) ec مهر;;
        8) ec آبان;;
        9) ec آذر;;
        10) ec دی;;
        11) ec بهمن;;
        12) ec اسفند;;
        *) ecerr "$0: Invalid month: $1";;
    esac
}
function datej-all() {
    local now=("${(s./.)$(datej)}")
    local cyear="$now[1]"
    cyear="${cyear[3,4]}"
    integer cmonth="$now[2]"
    integer cday="$now[3]"

    # Persian text is not well weupported in widgets or Telegram.
    # Full:
    # ec "$cyear/$(monthj2en $cmonth) $cmonth/$cday $(date +'%A %B %d')"
    # Abbrev: (We don't want it occupying two lines on the widget.)
    ec "$cyear/$(monthj2en $cmonth)$cmonth/$cday $(date +'%a %b%-m/%d')" # %y/
}
##
remnd() {
    # Example: `remcnd "⛸ 🚪 Don't put the shoes behind the door" 1 3 7 20 60 360`
    local d msg="$1" ; shift
    for d in "$@" ; do
        fnswap cellp true remn "$msg" "$d day later"
    done
    cellp
}
##
