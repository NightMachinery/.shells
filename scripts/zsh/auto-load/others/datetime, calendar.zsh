function fromnow() {
    mdoc "date= $0 -> seconds since \$date" MAGIC
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
    ec "$dest : $text"
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

    today=( "$remindayDir/$cyear/$cmonth/$cday"*(N.) )
}
function rem-today() {
    local deleteMode="${rem_today_delete:-$rem_today_d}"

    local today
    rem-todaypaths

    local text=""
    local f bak
    for f in $today[@] ; do
        if test -e "$f" ; then
            text="$text"$'\n'"$(<$f)"
            if test -n "$deleteMode" ; then
                bak="$(realpath --relative-to "$remindayDir" "$f")"
                bak="${remindayBakDir}/$bak"
                ensure-dir "$bak"
                mv "$f" "$bak"
                rmdir-empty "$remindayDir"
            fi
        fi
    done

    ec "$text"
}
function rem-today-notify() {
    local text="$(rem-today)"
    if test -n "$text" ; then
        terminal-notifier -title "$(datej)" -message "$text"
    fi
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
    text="$text"$'\n\n'"$(datej) $(date +"%A %B %d")"
    tsend --parse-mode markdown -- "$rec" "$text"
}
##
