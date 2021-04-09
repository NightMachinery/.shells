### Usage examples
# `remcnd 'How are your health habits going? Nutrition is still not tracked, no? Take care.' {7..90..7}`
###
# Some vars are defined in configvars
###
@opts-setprefix remj reminday_store
@opts-setprefix remn reminday_store
@opts-setprefix remnd reminday_store
@opts-setprefix rem-sync reminday_store
##
function rem-sync() {
    local nosync="${reminday_store_nosync}"

    test -n "$nosync" || {
        ec $'\n'
        cellp
        awaysh iwidget-rem-refresh
    }
}
function iwidget-rem-refresh() {
    if [[ "$remindayDir" == "$remindayCDir" ]] ; then
        ecerr "$0: remindayDir is set to remindayCDir; Skipping refresh."
        return 0
    fi

    deus iwidget-rem # refresh the cache
    if isLocal ; then
        awaysh wallpaper-auto
        brishzr awaysh deus iwidget-rem-refresh # refresh the cache
    fi
}
##
export remindayCDir="$cellar/remindersC"
export remindayBakCDir="$cellar/remindersC_bak" # moving these to a local location will cause one server to lose out on it.
# export remindayBakCDir="$HOME/tmp/remindersC_bak"
alias withremc='remindayDir="$remindayCDir" remindayBakDir="$remindayBakCDir" '

aliasfn remcnd withremc remnd
@opts-setprefixas remcnd remnd
aliasfn remcn withremc remn
@opts-setprefixas remcn remn
aliasfn remcj withremc remj
@opts-setprefixas remcj remj

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

    for i in {1..31} ; do
        pre="$i day(s) later:"
        # bidi chars: https://www.w3.org/International/questions/qa-bidi-unicode-controls.en
        out+="$(prefix-if-ne $'\n\n'"$pre"$'\n' "$(rem-today-d "$i" | prefixer --skip-empty -a $'\U202A''  ' --add-postfix $'\U202C')")"
    done

    # out="$(prefix-if-ne "Coming up:"$'\n' "$out")"
    trim "$out"
}
###
function seconds-fmt-short() {
    integer secs="${1:?}"

    printf '%dh:%dm:%ds\n' $(($secs/3600)) $(($secs%3600/60)) \
        $(($secs%60))
}
function seconds-fmt() {
    local secs="${1:?}"

    printf '%02dd:%02dh:%02dm:%05.2fs\n' $(($secs/86400)) $(($secs%86400/3600)) $(($secs%3600/60)) \
        $(($secs%60))
}
##
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
##
function dateshort() { date +"%b %d %H:%M:%S" }
dateshortnum() { date +"%Y/%m/%d" }
datej() {
    # alt: Python https://github.com/fitnr/convertdate
    # jalalim tojalali "$(dateshortnum)"
    jalalicli today
}
##
function remj() {
    (($+commands[jalalicli])) || {
        ecerr "$0: jalalicli not found."
        return 1
    }

    local now=("${(s./.)$(datej)}")
    local cyear="$now[1]"
    local cmonth="$now[2]"
    local cday="$now[3]"

    local text="$1"
    [[ "$text" == '-' ]] && text="$(</dev/stdin)"
    local D=0 M=0 Y=0
    local day="${2:-$cday}"

    # [[ "$day" =~ '^\+(\d+)$' ]] && day=$(( cday + $match[1] ))
    [[ "$day" =~ '^\+(\d+)$' ]] && { D="${match[1]}" ; day="${cday}" }

    # test -z "$day" && { ecerr "$0: empty first arg. Aborting." ; return 1 }
    typeset -Z2 day="$day"
    local month="${3:-$cmonth}"
    # [[ "$month" =~ '^\+(\d+)$' ]] && month=$(( cmonth + $match[1] ))
    [[ "$month" =~ '^\+(\d+)$' ]] && { M="${match[1]}" ; month="${cmonth}" }
    [[ "$month" =~ '^\d$' ]] && month="0$month"
    local year="${4}"
    # [[ "$year" =~ '^\+(\d+)$' ]] && year=$(( cyear + $match[1] ))
    [[ "$year" =~ '^\+(\d+)$' ]] && { Y="$match[1]" ; year="${cyear}" }
    [[ "$year" =~ '^\d$' ]] && year="140$year"
    [[ "$year" =~ '^\d\d$' ]] && year="14$year"

    if test -n "$year" ; then
        # No longer complete (other increments can increase the year), but better than nothing? :D
        color 50 10 255 "Year set explicitly to $year"
    else
        year="$cyear"
    fi
    local target_date="$year/$month/$day"
    if ! {
               [[ "$day" =~ '^\d\d$' && "$month" =~ '^\d\d$' && "$year" =~ '^\d\d\d\d$' ]] && (( day <= 31 || month <= 12 ))
           } ; then
        ecerr "$0: Invalid date: $target_date"
        return 1
    fi
    local target_date_unix
    target_date_unix="$(jalalicli togregorian "$target_date" -g unix -y "$Y" -m "$M" -d "$D")" || return $?
    target_date="$(jalalicli tojalali "$target_date_unix" -g 'unix')" || return $?
    local gdate="$(jalalicli togregorian "$target_date" -g 'Mon 2006_01_02')"
    local dest="$remindayDir/$target_date $gdate"
    @opts datej "$target_date" @ reminday_store "$dest" "$text"
}
function reminday_store() {
    unset rem_dest
    local dest="${1}" text="$(trim "$2")" nosync="${reminday_store_nosync}" ext="${reminday_store_ext:-.md}" datej="${reminday_store_datej}"

    if [[ "$ext" != .* ]] ; then
        ext=".$ext"
    fi

    test -z "$dest" && return 1
    test -z "$text" && return 1

    dest="${dest}${ext}"
    ensure-dir "$dest" || return 1
    ec "$text" >> $dest || return $?
    ##
    if test -n "$datej" ; then
        Bold ; color 100 255 200 "$(@opts mode 1 @ datej-all "$datej")" ; resetcolor
    fi
    ecn "$dest : " ; Bold ; color 100 200 255 "$text" ; resetcolor
    ##
    rem-sync
    rem_dest="$dest"
}
##
function datenat() {
    datenat.js "$@"
}
aliasfn datenat-future datenat_nopast=y datenat
function datenatj() {
    : "GLOBAL OUT: datenatj_date datenatj_datej"

    unset datenatj_date
    unset datenatj_datej
    local natdate="$*"

    local gdate
    gdate="$(datenat $natdate)" || { ecerr "$0: datenat failed for: $natdate" ; return 1 }
    datenatj_date="$gdate"
    datenatj_datej="$(jalalicli tojalali "$gdate")"
    ecn "$datenatj_datej"
}
aliasfn datenatj-future datenat_nopast=y datenatj
function datenat-full1() {
    # Used in 'remn'
    local natdate="$*"

    local jdate
    sout datenatj "$natdate" || return 1
    jdate="$datenatj_datej" # OUT: datenatj_datej is used in 'remn'
    local gdate="$(gdate --date "$datenatj_date" +'%a %Y_%m_%d')"
    ec "$jdate $gdate"
}
aliasfn datenat-full1-future datenat_nopast=y datenat-full1
function datenat-full2() {
    local natdate="$*"

    local jdate
    sout datenatj "$natdate" || return 1
    jdate="$datenatj_datej"

    @opts mode 1 @ datej-all "$jdate"
}
aliasfn datenat-full2-future datenat_nopast=y datenat-full2
##
function remn() {
    local text="$1" natdate="${@:2}"
    [[ "$text" == '-' ]] && text="$(</dev/stdin)"
    local dest
    dest="$remindayDir/$(datenat-full1-future "$natdate")" || return $?
    @opts datej "$(datenatj-future "$natdate")" @ reminday_store "$dest" "$text"
}
function remn-interactive() {
    local text="$*"
    
    local natdate=""
    # @warn brishz.dash does not quote its arguments, so we essentially have an @unsafeEval here. I think it's worth the speed boost, though I have noot profiled it.
    natdate="$(FZF_DEFAULT_COMMAND=echo fz-empty --header "$(Bold ; ecn "Today: " ; colorfg 0 100 255 ; datej-all-long ; resetcolor)" --reverse --height '20%' --bind "change:reload:brishz.dash reval-true serr datenat-full2-future {q} || true" --disabled --query "" --print-query | ghead -n 1)" || return $?
    remn "$text" "$natdate"
}
aliasfn ri remn-interactive
##
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
    unset rem_today_delete # stop propagating these to inner calls of rem-today SHUDDERS
    unset rem_today_d

    ensure-dir "$remindayDir/"
    ensure-dir "$remindayBakDir/"

    local today
    rem-todaypaths

    local text=""
    local f bak res
    for f in $today[@] ; do
        if test -e "$f" ; then
            local ext="${f:e}"
            if [[ "$ext" == zsh ]] ; then
                if test -n "$deleteMode" ; then # only run the code once
                local out
                ##
                # out="$(fnswap isI false source "$f" 2>&1)" || out+=$'\n\n'"$0: ${(q+)f} returned $?"
                # Run clean zsh so that  our env doesn't pollute it.
                out=$'\n\n'"$(FORCE_INTERACTIVE='' FORCE_NONINTERACTIVE=y zsh "$f" 2>&1)" || out+=$'\n\n'"$0: ${(q+)f} returned $?"
                ##
                text+=$'\n\n'"$out" # will be interpreted in markdown; Escape it perhaps? The power is nice though.
                fi
            elif [[ "$ext" == bak ]] ; then
                res="$0: Skipped: $f"
                ecerr "$res"
                # text+=$'\n\n'"$res"
            else
                # text+=$'\n\n'"$0: Unsupported file: $f"
                text+=$'\n\n'"$(<$f)" #"${$(<$f ; ec .)[1,-2]}"
            fi
            if test -n "$deleteMode" ; then
                bak="$(rem_extract-date-path "$f")"
                bak="${remindayBakDir}/$bak"
                if [[ "$ext" == zsh ]] ; then
                    bak+=".bak"
                fi
                serr append-f2f "$f" "$bak" && command rm "$f" # not deleting if the source is the same as the dest
            fi
        fi
    done
    rmdir-empty "$remindayDir"

    trim "$text"
}
function rem-today-d() {
    local day="${1:-1}"
    fnrep datej "datenatj-future $day day later" rem-today
}
function rem-today-notify() {
    ensure-dir ~/logs/
    {
        ec "---"
        date
        rem-sync
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
    local text="$(@opts delete y notif y md y @ rem-summary)"
    text="$text"$'\n\n'"$(datej-all)"

    tsend --parse-mode markdown -- "$rec" "$text"
}
function rem-summary() {
    local deleteMode="$rem_summary_delete" notifMode="$rem_summary_notif" markdownMode="${rem_summary_md}"

    local text="$(@opts delete "$deleteMode" @ rem-today)"
    if test -n "$text" ; then
        test -n "$notifMode" && tnotif "$text" >&2
        if test -n "$markdownMode" ; then
            # '#' is needed to make this invisible to timetracker.py
            text="$(ecn "$text" | prefixer --add-prefix='# **' --add-postfix='**')"
        fi
    else
        if test -n "$markdownMode" ; then
            text="# 🌠"
        fi
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
    ## test:
    # datej-all 1380/06/20
    # 80/Shahrivar6/20 Tue Sep9/11
    ##
    local datej="${*:-$(datej)}" mode="${datej_all_mode:-0}"

    local now=("${(s./.)datej}")
    local cyear="$now[1]"
    (( mode == 0 )) && cyear="${cyear[3,4]}"
    integer cmonth="$now[2]"
    integer cday="$now[3]"

    if (( mode == 0 )) ; then
        local dateg="$(jalalicli togregorian --gregorian-format='Mon Jan1/2' "$datej")"
        # Persian text is not well supported in widgets or Telegram.
        # Abbrev: (We don't want it occupying two lines on the widget.)
        ec "$cyear/$(monthj2en $cmonth)$cmonth/$cday $dateg"
    elif (( mode == 1 )) ; then
        local dateg="$(jalalicli togregorian --gregorian-format='Monday January1/2/2006' "$datej")"
        ec "$cyear/$(monthj2en $cmonth)$cmonth/$cday $dateg"
    else
        ecerr "$0: Unsupported mode '$mode'"
    fi
}
aliasfn datej-all-long @opts mode 1 @ datej-all
##
remnd() {
    : readmeall
    # Example: `remcnd "⛸ 🚪 Don't put the shoes behind the door" 1 3 7 20 60 360`
    local d msg="$1" ; shift
    for d in "${@[1,-2]}" ; do
        @opts nosync y @ remn "$msg" "$d day later"
    done
    (( ${#@} > 1 )) && test -z "$reminday_store_ext" && msg="Last reminder: $msg"
    @opts nosync y @ remn "$msg" "${@[-1]} day later"
    rem-sync
}
##
function rem-rec() {
    local rem_dest=''
    local cmd="$(gq "$@")"
    fnswap rem-sync true eval "$cmd"
    if test -n "$rem_dest" ; then
        # we can use reminday_store here to be more verbose, but I doubt we'd want that.
        ec "$0 $cmd" >> "${rem_dest:r}.zsh" || ecerr "$0: Failed to write the recursion with $?"
    else
        ecerr "$0: rem_dest has not been set by the cmd."
        return 1
    fi
    silent rem-sync || {
        ecerr "$0: rem-sync returned $?"
    }
}
function rem-reclater() {
    local later="${1:? later required}" ; shift
    local cmd="$(gq "$@")"
    if [[ "$1" == remn ]] ; then
    # @design we currently rely on the message generated by rem-rec to remind ourselves on the first due date. It might be more prudent to get a reminder text specifically for the first due date. Note that if we assume this is only going to be used with remn, we can extract the message automatically.
        @opts nosync y @ remn "$2" "$later"
    fi

    @opts ext zsh @ remn "rem-rec $cmd" "$later"
}
