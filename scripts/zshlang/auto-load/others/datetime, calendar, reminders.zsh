### Usage examples
# `remcnd 'How are your health habits going? Nutrition is still not tracked, no? Take care.' {7..90..7}`
###
# Some vars are defined in configvars
###
alias withcanada="TZ='America/Toronto'"
##
@opts-setprefix remj reminday_store
@opts-setprefix remn reminday_store
@opts-setprefix remnd reminday_store
@opts-setprefix rem-sync reminday_store
##
typeset -gA months2name
months2name[1]='January'
months2name[2]='February'
months2name[3]='March'
months2name[4]='April'
months2name[5]='May'
months2name[6]='June'
months2name[7]='July'
months2name[8]='August'
months2name[9]='September'
months2name[10]='October'
months2name[11]='November'
months2name[12]='December'

function month-number-to-name {
    typeset -i n="${1}"
    ec "${months2name[$n]}"
}
##
function rem-enabled-p {
    #: The directory itself seems to be created even when it doesn't exist by our own functions. So I am checking its children.
    local candidates=("$remindayDir"/14*(DN/))

    (( ${#candidates} >= 1 ))
}
##
function rem-sync {
    local nosync="${reminday_store_nosync}"

    if ! bool "$nosync" ; then
        ec $'\n'
        h-rem-sync
        awaysh iwidget-rem-refresh
    fi
}

function rem-sync-ni {
    if battery-p ; then
        log-to ~/logs/rem_sync ecerr "$0: aborting because on battery"
    fi

    gsync_commit_pre_opts=(-c commit.gpgsign=false) log-to ~/logs/rem_sync rem-sync "$@"
    #: gpg signing will interactively ask for the passphrase when run in cron or tmux
}
##
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
function rem-comingup {
    local days="${rem_comingup_days:-366}"

    goremind comingup --days "$days"
}

function rem-comingup-v1 {
    local days="${rem_comingup_days:-31}"

    local out='' i pre

    for i in {1..${days}} ; do
        pre="$i day(s) later: ($(h-datenatj-rem-summary "${i} day later"))"
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
    assert-args date

    local then
    then="$(assert gdate --date "$date" "+%s")" || return 1
    ec $(( EPOCHSECONDS - then ))
}

function fromnow-py() {
    : "date= $0 -> seconds since \$date"

    python3 -c 'import datetime ; from dateutil.parser import parse ; import os
date = parse(os.environ["date"])
print((datetime.datetime.now(date.tzinfo) - date).total_seconds())'
}
##
function date-time {
    gdate +"%H:%M:%S"
}

function datej-all-long-time {
    ec "$(datej-all-long) $(date-time)"
}
alias now='datej-all-long-time'

function now-year {
    gdate +%Y @TRET
}

function dateshort() { date +"%b %d %H:%M:%S" }

function dateshortnum {
    date +"%Y/%m/%d"
}

function date-tehran {
    TZ='Asia/Tehran' date +'%Y-%m-%d %A %H:%M:%S'
}

function date-canada {
    TZ='Canada/Eastern' date +'%Y-%m-%d %A %H:%M:%S'
}

function date-spain {
    TZ='Europe/Madrid' date +'%Y-%m-%d %A %H:%M:%S'
}

function date-germany-berlin {
    TZ='Europe/Berlin' date +'%Y-%m-%d %A %H:%M:%S'
}

function date-anywhere-on-earth {
    TZ='Etc/GMT+12' date +'%Y-%m-%d %A %H:%M:%S'
}
aliasfn date-aoe date-anywhere-on-earth

function date-usa-est {
    TZ='EST' date +'%Y-%m-%d %A %H:%M:%S'
    #: The Eastern Time Zone (ET) is a time zone encompassing part or all of 23 states in the eastern part of the United States, parts of eastern Canada, and the state of Quintana Roo in Mexico.
    #: Eastern Standard Time (EST) is five hours behind Coordinated Universal Time (UTC−05:00). Observed during standard time (late autumn/winter in the United States and Canada).
    # Eastern Daylight Time (EDT) is four hours behind Coordinated Universal Time (UTC−04:00). Observed during daylight saving time (spring/summer/early autumn in the United States and Canada).
}

function date-usa-pst {
    TZ='PST8PDT' date +'%Y-%m-%d %A %H:%M:%S'
    #: The Pacific Time Zone (PT) is a time zone encompassing parts of western Canada, the western United States, and western Mexico.
    #: Pacific Standard Time (PST) is eight hours behind Coordinated Universal Time (UTC−08:00). Observed during standard time (late autumn/winter in the United States and Canada).
    #: Pacific Daylight Time (PDT) is seven hours behind Coordinated Universal Time (UTC−07:00). Observed during daylight saving time (spring/summer/early autumn in the United States and Canada).
}

function date-usa-cst {
    TZ='CST6CDT' date +'%Y-%m-%d %A %H:%M:%S'
    #: The Central Time Zone (CT) is a time zone in the central part of the United States, parts of Canada, and some areas in Mexico and Central America.
    #: Central Standard Time (CST) is six hours behind Coordinated Universal Time (UTC−06:00). Observed during standard time (late autumn/winter in the United States and Canada).
    #: Central Daylight Time (CDT) is five hours behind Coordinated Universal Time (UTC−05:00). Observed during daylight saving time (spring/summer/early autumn in the United States and Canada).
}

function date-uk {
    TZ='Europe/London' date +'%Y-%m-%d %A %H:%M:%S'
}
aliasfn date-london date-uk

function datej {
    # alt: Python https://github.com/fitnr/convertdate
    # jalalim tojalali "$(dateshortnum)"
    jalalicli today "$@" |
        cat-copy-if-tty
}

function datej-year {
    jalalicli today --jalali-format='yyyy' "$@"
}

function datej-month() {
    jalalicli today --jalali-format='M' "$@"
}

function datej-day() {
    jalalicli today --jalali-format='d' "$@"
}
##
function date-unix-to-3339 {
    local date_unix="$1"

    gdate -d "@${date_unix}" --rfc-3339=s
}
reify date-unix-to-3339

function h-unix-allday-p {
    #: sees if the unix timestamp has hour:minute resolution or is only accurate to the day
    #: [[zf:~\[jalalicli\]/jalalicli.go::pt = ptime.Date(year, ptime.Month(month), day, 12, 59, 59, 0, ptime.Iran())]]
    #: [[NIGHTDIR:javascript/datenat.js::currentTime.setUTCHours(9,29,59,0)]]
    ##
    time_start='' #: @globalOut
    time_end='' #: @globalOut

    local date_unix="$1" verbose="${h_unix_allday_p_v:-y}"
    local date_nat="${reminday_store_date_nat}"

    local time
    time="$(gdate --utc -d "@${date_unix}" '+%H:%M:%S')" @TRET
    # typ time

    local res
    if true || test -z "$date_nat" ; then
        #: @update [jalali:1402/02/05/12:48] datenat parser seems to return other times than 12 PM, so this whole strategy doesn't seem workable.
        #:
        #: @assumption The only way we could have specified the exact time is using 'remn' currently, so if no natural date provided, it should be an allday event.

        res=0
    else
        if [[ "$time" == '09:29:59' ]] ; then
            res=0
        elif [[ "$time" == '08:30:00' ]] ; then
            #: 12 PM in Iran's timezone
            #: This is the default time of the datenat parser for days other than today.
            if [[ "$date_nat" =~ '(?i)12(?::|\d)*\s*pm' ]] ; then
                res=1
            else
                res=0
            fi
        else
            res=1
        fi
    fi

    if (( res == 1 )) ; then
        time_start="$(gdate -d "@${date_unix}" '+%H:%M')" @TRET


        if test -n "${gcal_dur}" ; then
            duration="$(gcal_allday="n" h-gcal-dur-preprocess "$gcal_dur")" @TRET
            duration=$(( duration * 60 )) #: converted duration from min to sec
            time_end="$(gdate -d "@$((date_unix + duration))" '+%H:%M')" @TRET
        fi

        if bool "$verbose" ; then
            ecgray "target time: ${time_start}" || true
        fi
    fi

    return $res
}
##
function remj {
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

    @opts \
        date_unix "${target_date_unix}" \
        @ reminday-store-unix "$text"
}

function reminday-store-unix {
    local opts=("$@") #: should only include 1 arg for 'text'
    local target_date_unix="${reminday_store_date_unix}"
    assert-args target_date_unix @RET
    local date_nat="${reminday_store_date_nat}"

    target_date="$(jalalicli tojalali "$target_date_unix" -g 'unix')" || return $?
    local dateg="$(jalalicli togregorian "$target_date" -g 'Mon 2006_01_02')"
    local dest="${remindayDir}/${target_date} ${dateg}"
    @opts \
        datej "$target_date" \
        dateg "$dateg" \
        date_unix "${target_date_unix}" \
        @ reminday_store "$dest" "${opts[@]}"
}
@opts-setprefix reminday-store-unix reminday_store

function reminday_store {
    unset rem_dest
    local dest="${1}"
    local text
    text="$(trim "$2")" @TRET
    local text_timed="${text}"
    local filesystem_backend_p="${reminday_store_fs_p:-y}"
    local gcal_backend_p="${reminday_store_gcal_p}"
    local nosync="${reminday_store_nosync}" ext="${reminday_store_ext:-.md}"
    local date_unix="${reminday_store_date_unix}"
    local datej="${reminday_store_datej}"
    local dateg="${reminday_store_dateg}"

    local color=false
    if isColorTty ; then
        color=true
    fi

    if test -z "${gcal_backend_p}" ; then
        if isServer ; then
            gcal_backend_p='n'
        elif isMe ; then
            # gcal_backend_p='y'
            gcal_backend_p='n' #: gcalcli needs to login every week!
        fi
    fi

    test -z "$text" && return 1

    local allday time_start='' time_end=''
    if reminday_store_date_nat="${date_nat}" h-unix-allday-p "${date_unix}" ; then
        allday=y
    else
        allday=n

        text_timed="${text} ${time_start}"
        if test -n "${time_end}" ; then
            text_timed+="->${time_end}"
            #: alternative unicode symbols:
            # ➡
            # →
            # →
            # ⟶
            # ⟾
            # ↝
            # ⇒
            # ➙
            # ...
        fi
    fi

    if bool "${filesystem_backend_p}" ; then
        if [[ "$ext" != .* ]] ; then
            ext=".$ext"
        fi

        test -z "$dest" && return 1

        dest="${dest}${ext}"
        ensure-dir "$dest" || return 1
        ec "${text_timed}" >> $dest || return $?
    fi
    ##
    ecgray "now: $(datej-all-long-time)"

    if test -n "$datej" ; then
        Bold ; color 100 255 200 "$(@opts mode 1 @ datej-all "$datej")" ; resetcolor
    fi
    if bool "${filesystem_backend_p}" ; then
        ecn "$(remiday_color="$color" reminday-path-colorize $dest): " ; Bold ; color 100 200 255 "${text_timed}" ; resetcolor
    fi
    ##
    if bool "${gcal_backend_p}" ; then
        @opts \
            title "ϟ $text" \
            when "@${date_unix}" \
            allday "$allday" \
            @ gcal-add @STRUE
        # desc "added by reminday"
    fi
    ##
    if bool "${filesystem_backend_p}" ; then
        rem-sync
        rem_dest="$dest"
    fi
}
##
function datenat {
    local inargs
    in-or-args2 "$@" @RET

    local text="${inargs[*]}"

    #: Making time zones understandable by datenat.js:
    text="$(ec "$text" | perl -lpe 's/Pacific Time/PT/g')"

    local out
    out="$(datenat.js "$text")" @RET

    ecn "${out}" | cat-copy-if-tty
    #: cat-copy will add a newline itself, but it will copy without newline so we can paste it in Excel without overwriting the next cell.
}
aliasfn datenat-future datenat_nopast=y datenat
aliasfn datenat-unix datenat_unix=y datenat
aliasfn datenat-future-unix datenat_nopast=y datenat_unix=y datenat

function datenat-formatted {
    local time
    time="$(datenat_unix=y datenat.js "$*")" @RET

    gdate --date="@${time}"
}

function datenatj {
    : "GLOBAL OUT: datenatj_unix datenatj_date datenatj_datej"

    unset datenatj_unix
    unset datenatj_date
    unset datenatj_datej
    local natdate
    natdate="$(in-or-args "$@")" @RET

    datenatj_unix="$(datenat_unix=y datenat $natdate)" || { ecerr "$0: datenat failed for: $natdate" ; return 1 }
    datenatj_datej="$(jalalicli tojalali "$datenatj_unix" -g 'unix')" || return $?
    datenatj_date="$(jalalicli togregorian "$datenatj_datej" -g 'Mon 2006_01_02')"
    ecn "$datenatj_datej"
}
aliasfn datenatj-future datenat_nopast=y datenatj

function datenatj-human {
    datenatj "$@" | monthj2en-filter
}

function monthj2en-filter {
    local inargs
    in-or-args3 "$@" @RET

    local date year month day pre post
    for date in ${inargs[@]}; do
        if [[ "${date}" =~ "(.*?)(\d+)/(\d+)/(\d+)(.*)" ]] ; then
            pre="${match[1]}"
            year="${match[2]}"
            month="${match[3]}"
            day="${match[4]}"
            post="${match[5]}"

            ec "${pre}${year}/$(monthj2en $month)${month}/${day}${post}"
        else
            ec "${date}" #: return the line unchanged
        fi
    done
}


function datenat-full1 {
    # Used in 'remn'
    local natdate="$*"

    local jdate
    sout datenatj "$natdate" || return 1
    jdate="$datenatj_datej" # OUT: datenatj_datej is used in 'remn'
    local gdate="$(gdate --date "@${datenatj_unix}" +'%a %Y_%m_%d')"
    ec "$jdate $gdate"
}
aliasfn datenat-full1-future datenat_nopast=y datenat-full1

function datenat-full2 {
    local natdate="$*"

    local jdate
    sout datenatj "$natdate" || return 1
    jdate="$datenatj_datej"
    date_unix="$datenatj_unix"

    local datej_all time
    datej_all="$(@opts mode 1 @ datej-all "$jdate")" @TRET
    time="$(gdate --date "@${datenatj_unix}" +"%H:%M:%S")" @TRET
    ec "${datej_all} ${time}" |
        cat-copy-if-tty
}
aliasfn datenat-full2-future datenat_nopast=y datenat-full2

function h-datenatj-rem-summary {
    local natdate="$*"

    local jdate
    sout datenatj "$natdate" || return 1
    jdate="$datenatj_datej"
    local gdate="$(gdate --date "@${datenatj_unix}" +'%a')"
    ec "$gdate ${jdate#14*/}"
}
##
function remn {
    local text="$1" natdate="${@:2}"
    local -x datenat_hardcode_time=y
    [[ "$text" == '-' ]] && text="$(</dev/stdin)"

    @opts \
        date_unix "$(datenat-future-unix "$natdate")" \
        date_nat "$natdate" \
        @ reminday-store-unix "$text"
}

function remn-interactive {
    local text="$*"

    local -x datenat_hardcode_time=y
    
    local natdate=""
    natdate="$(FZF_DEFAULT_COMMAND=echo fz-empty --header "$(Bold ; ecn "Today: " ; colorfg 0 100 255 ; datej-all-long-time ; resetcolor)" --reverse --height '20%' --bind "change:reload:brishzq.zsh reval-true serr datenat-full2-future {q} || true" --disabled --query "" --print-query | ghead -n 1)" || return $?
    remn "$text" "$natdate"
}
aliasfn ri reminday_store_nosync=y remn-interactive

function gcal-add-interactive {
    reminday_store_nosync=y reminday_store_fs_p=n remn-interactive "$@"
}
@opts-setprefix gcal-add-interactive gcal
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
    local f="$1"
    f="$(grealpath -- "$f")" @TRET

    if [[ "$f" =~ '(\d\d\d\d/\d\d/\d\d.*)' ]] ; then
        ec "$match[1]"
    else
        ecerr "Couldn't get date path components of: $f"
        return 1
    fi
}

function rem-today {
    local deleteMode="${rem_today_delete:-$rem_today_d}"
    unset rem_today_delete # stop propagating these to inner calls of rem-today SHUDDERS
    unset rem_today_d

    ensure-dir "$remindayDir/"
    ensure-dir "$remindayBakDir/"

    local text=""


    pushf "$remindayDir/"
    {
        if git-merge-p ; then
            text+=$'\n\n'"Reminder directory is in an active merge!"
        fi
    } always { popf }

    local today
    rem-todaypaths

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
                    ecbold "$0: running $(gq "$f")"

                    out=$'\n\n'"$(env-clean FORCE_INTERACTIVE='' FORCE_NONINTERACTIVE=y color_p=n zsh "$f" 2>&1 | erase-ansi)" || out+=$'\n\n'"$0: ${(q+)f} returned $?"
                    #: Even if there is an error, the output is still stored in =out=.
                    #: E.g., `a= ; a="$(echo hi ; return 1)" || a+=' beautiful' ; echo $a` prints 'hi beautiful'.

                    ecbold "$0: finished $(gq "$f")"
                    ##
                    text+=$'\n\n'"$out" # will be interpreted in markdown; Escape it perhaps? The power is nice though.
                fi

            elif [[ "$ext" == bak ]] ; then
                #: These are always =.zsh.bak= files, as we don't add the =.bak= suffix for text files in [agfi:rem-trs].
                ##
                res="$0: Skipped: $f"
                # ecerr "$res"
                # text+=$'\n\n'"$res"
            else
                # text+=$'\n\n'"$0: Unsupported file: $f"
                text+=$'\n\n'"$(<$f)" #"${$(<$f ; ec .)[1,-2]}"
            fi
            if test -n "$deleteMode" ; then
                rem-trs "$f"
            fi
        fi
    done
    rmdir-empty "$remindayDir"

    trim "$text"
}

function rem-trs {
    local fs=($@)

    local f
    for f in "${fs[@]}" ; do
        bak="$(rem_extract-date-path "$f")"
        bak="${remindayBakDir}/$bak"
        if [[ "$ext" == zsh ]] ; then
            bak+=".bak"
        fi
        revaldbg serr append-f2f "$f" "$bak" && revaldbg silent trs-rm "$f" #: not deleting if the source is the same as the dest
    done
}

function rem-today-d {
    local day="${1:-1}"
    fnrep datej "datej-daylater '$day'" rem-today
}

function datej-daylater {
    local day="${1:-1}"

    datenatj-future "$day" day later
    #: datenatj doesn't work with negative values, so we might as well use datenatj-future
}

function rem-today-notify {
    ensure-dir ~/logs/
    {
        ec "-------"
        date

        ec "---"
        rem-sync-ni
        ec "---"

        local text="$(rem-today ; ec ; remc-today)"
        if test -n "$text" ; then
            reval-ec notif-os "$(datej)" "$text"
        fi
        ec "-------"
    } >> ~/logs/rem-today-notify.txt  2>&1 | cat
}

function tlg-reminday {
    local rec="$1"
    test -z "$rec" && {
        ecerr "$0: Empty receiver."
        return 1
    }
    local text="$(@opts delete y notif y md y @ rem-summary)"
    text="$text"$'\n\n'"$(datej-all)"
    text="$text"$'\n\n'"$(reminders-old-cat | erase-ansi)"

    tsend --parse-mode markdown -- "$rec" "$text"
}

function rem-summary {
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
    ec
}

function rem-summary-today {
    rem_comingup_days=0 rem-summary
}

function monthj2en {
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

function datej-all {
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
    elif (( mode == 2 )) ; then
        ec "$cyear/$(monthj2en $cmonth)/$cday"
    elif (( mode == 3 )) ; then
        local dateg="$(jalalicli togregorian --gregorian-format='Mon Jan1/2' "$datej")"
        ec "$(monthj2en $cmonth)/$cday $dateg"
    elif (( mode == 4 )) ; then
        local dateg="$(jalalicli togregorian --gregorian-format='Jan1/2' "$datej")"
        ec "$(monthj2fa $cmonth)/$cday $dateg"
    else
        ecerr "$0: Unsupported mode '$mode'"
    fi
}
aliasfn datej-all-long @opts mode 1 @ datej-all
aliasfn datej-named @opts mode 2 @ datej-all
aliasfn datej-all-short @opts mode 3 @ datej-all
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
function rem-rec {
    local rem_dest=''
    local cmd="$(gquote-dq "$@")"

    ## @verbose
    # fnswap isColor true ectrace "$0: started" &>/dev/tty
    ##

    reminday_store_nosync=y eval "$cmd"
    if test -n "$rem_dest" ; then
        #: we can use reminday_store here to be more verbose, but I doubt we'd want that.

        local o="${rem_dest:r}.zsh"
        local cmd_recur="$0 $cmd"
        ec "${cmd_recur}" >> "$o" || {
            ecerr "$0: Failed to write the recursion with $?"
            return 1
            }
        fnswap isColor true ecbold "$0: appended to $o: ${cmd_recur}" &>/dev/tty
    else
        ecerr "$0: rem_dest has not been set by the cmd."
        return 1
    fi

    # @warn /dev/tty should be open for writing or this will fail:
    fnswap isColor true ecbold "$0: rem-sync" &>/dev/tty
    fnswap isColor true rem-sync >&/dev/tty || {
        ecerr "$0: rem-sync returned $?"
    }
    fnswap isColor true ecbold "$0: finished" &>/dev/tty

    return 0
}

function h-rem-reclater {
    #: @deprecated
    #: I can no longer fathom what use this funtion has. `rem-rec' itself should work with `remn'.
    ##
    local later="${1:? later required}" ; shift
    local cmd=("$@")
    local cmd_q="$(gquote-dq "$@")"

    if [[ "${cmd[1]}" == remn ]] ; then
    # @design we currently rely on the message generated by rem-rec to remind ourselves on the first due date. It might be more prudent to get a reminder text specifically for the first due date. Note that if we assume this is only going to be used with remn, we can extract the message automatically.
        local text="${cmd[2]}"
        @opts nosync y @ remn "$text" "$later"
    fi

    @opts ext zsh @ remn "rem-rec ${cmd_q}" "$later"
}
##
function date-unix {
    command date "+%s"
}

function date-from-unix {
    local unix
    unix="$(in-or-args "$@")" @RET

    gdate -d "@${unix}" --rfc-3339=s
}
##
function str-center-justify {
  local str="$1"
  local width="$2"
  local str_len=${#str}

  if (( width <= str_len )); then
    echo "$str"  # No need for justification, width is smaller than or equal to string length
  else
    local left_padding=$(( (width - str_len) / 2 ))
    local right_padding=$(( width - str_len - left_padding ))

    printf "%*s%s%*s\n" "$left_padding" "" "$str" "$right_padding" ""
  fi
}

function jalali-calendar {
    local opts=()

    #: =$array[(I)foo]= returns the index of the last occurrence of =foo= in =$array= and 0 if not found. The =e= flag is for it to be an exact match instead of a pattern match.
    if (( ${@[(Ie)--no-true-color]} == 0 )); then
        opts+=("--true-color")
    fi

    command jalali-calendar "$@" "${opts[@]}"
}
alias jcal='jalali-calendar'

function jcal-all {
    #: @seeAlso `python -m calendar`
    ##
    local year
    year="${1:-$(datej-year)}" @TRET

    jcal_all.pl "$year"
}
alias jcala='jcal-all'
##
function jalali-from-natural {
    local inargs
    in-or-args3 "$@" @RET

    local gregorian
    for date_nat in ${inargs[@]} ; do
        gregorian="$(datenat "${date_nat}")" || {
            ecerr "$0: could not parse: ${date_nat}"
            continue
        }

        ecbold "${date_nat} -> ${gregorian}"
        jalalicli tojalali "${gregorian}"
    done |
        cat-copy-if-tty
}
aliasfn jalalinat jalali-from-natural
###
function reminday-path-colorize {
    local colorMode="${remiday_color}"

    local res
    res="$(in-or-args "$@")" @RET

    if { test -z "$colorMode" && isColorTty } || bool "$colorMode" ; then
        ec $res | rg --passthrough --color always --colors 'match:bg:255,255,255' --colors 'match:fg:40,200,30' --colors 'match:style:bold' '(?P<year>\d+)/(?P<month>\d+)/(?P<day>\d+)[^/]*'
    else
        ec $res
    fi
}

function reminders-old-ls {
    datej="$(datej)" reminders_old_ls.rs "$@"
}

function reminders-old-cat-v1 {
    reminders-old-ls "$@" |
        inargsf re 'reval-ec cat'
}

function reminders-old-cat {
    local color=false
    if isColorTty ; then
        color=true
    fi

    local reminders
    reminders="$(reminders-old-ls "$@")"

    local r r_colored content
    for r in ${(@f)reminders} ; do
        content="$(cat "$r")" @TRET
        if [[ "$color" == true ]] ; then
            r_colored="$(ecn ${r} | remiday_color=y reminday-path-colorize)"
        else
            r_colored="$r"
        fi
        ec "* ${r_colored}:"$'\n'"${content}"$'\n'
    done
}

function reminders-old-ask {
    local color=false
    if isColorTty ; then
        color=true
    fi

    local reminders
    reminders="$(reminders-old-ls "$@")"

    local r content
    for r in ${(@f)reminders} ; do
        content="$(cat "$r")" @TRET
        if [[ "$color" == true ]] ; then
            r_colored="$(ecn ${r} | remiday_color=y reminday-path-colorize)"
        else
            r_colored="$r"
        fi
        if ask $'\n'"* ${r_colored}:"$'\n'"${content}"$'\n\n'"Delete? " y ; then
           trs "$r" @TRET
        fi
    done
}
##
function reminders-old-notes-ls {
    reminders-old-ls --end-pattern '\.('"${(j.|.)note_formats}"')' "$@"
}

function reminders-old-notes-cat {
    reminders-old-cat --end-pattern '\.('"${(j.|.)note_formats}"')' "$@"
}
###
function hours-since-nat {
    #: * @tests
    #: ** `hours-since-nat 'last saturday 14'`
    ##
    local from_text="$*"

    local from
    from="$(datenat-unix "${from_text}")" @RET
    from_formatted="$(datenat-formatted "${from_text}")" @TRET

    local duration
    duration=$((EPOCHSECONDS - from)) #: in seconds

    ecgray "Since: ${from_formatted}"
    ec $((duration / 3600))
}
##
function cook-date {
  local current_date=$(date +"%Y-%m-%d %H:%M:%S")
  ec "sudo date -s \"$current_date\"" |
      cat-copy-if-tty
}
##
