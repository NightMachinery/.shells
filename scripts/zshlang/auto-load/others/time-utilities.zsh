##
alias timezone-set-i-linux='sudo dpkg-reconfigure tzdata'

function timezone-set {
    local tz="${1:-Asia/Tehran}" force="${timezone_force}"
    assert-args tz @RET

    if isLinux ; then
        sudo timedatectl set-timezone "$tz"
        #: This doesn't work in some containers:
        #: https://stackoverflow.com/questions/43907925/ubuntu-timedatectl-fails-in-docker-container

        if bool "$force" ; then
            sudo dash -c 'echo "$timezone" > /etc/timezone'
            sudo mv /etc/localtime /etc/localtime.bak
            sudo dpkg-reconfigure -f noninteractive tzdata
        fi
    else
        @NA
    fi
}

function timezone-get {
    local mode="${1:-long}"

    if isLinux ; then
        if [[ "$mode" == 'long' ]] ; then
            timedatectl | rget "^\s*Time zone:\s+(.*)"
        elif [[ "$mode" == 'numeric' ]] ; then
            timedatectl | rget "^\s*Time zone:.*\((\+\d+)"
        elif [[ "$mode" == 'name' ]] ; then
            timedatectl | rget "^\s*Time zone:\s+(\S+)"
        fi
    else
        @NA
    fi
}

function TZ-set-from-system {
    export TZ="$(timezone-get name)"
}
##
function h_timer-bell {
    tts-glados1-cached "The time is now up, commander"
    awaysh @opts redo 2 @ bell-visual-flash1
    ##
    # awaysh tts-gateway-i2 "Deploy the delayed sequence." ;  tts-gateway-i1 "Deploy the delayed sequence."
    # awaysh tts-gateway-i2 "Deploy the delayed sequence." ;  awaysh tts-gateway-i1 "Deploy the delayed sequence." ; tts-gateway "Deploy the delayed sequence."
    ##
    # bell-pp-electricity
    # bell-visual-flash1
}

function timer {
    doc aliased to timer with noglob
    local t=$1 cmd=("${@[2,-1]}") notify="${timer_notify:-y}" msg="${timer_msg}"
    test -z "$cmd[*]" && cmd=(reval-env lo_s=20 loop h_timer-bell)

    sleep-neon $(($t * 60)) && {
        if bool $notify ; then
            local header='Timer'
            if (( t == 0 )) ; then
                header="Alarm"
            fi
            if test -n "$msg" ; then
                header="${header}: $msg"
            fi

            notif-os "$header" ""
        fi

        mark-me-if-unmarked "TIMER"
        reval "$cmd[@]"
        # eval ${(q+@)@[2,-1]:-${(z)/#/loop ot-play-happybirthday}}
    }
}
noglobfn timer

typeset -g marker_alarm_at='ALARM_AT'
function alarm-at {
    local at="$1" msg="${@[2,-1]}"
    assert-args at marker_alarm_at @RET

    ecgray "now: $(datej-all-long-time)"

    ec "${commands[brishzq.zsh]} awaysh-named ${marker_alarm_at} @opts msg $(gquote-sq "$msg") @ timer 0" | command at "${=at}"
}
aliasfn unix-at alarm-at
##
function h_timer-late {
    local i="${prv_loop_iteration}" i_dur="$lo_s"

    ##
    # now using awaysh
    # i_dur=$(( i_dur + 5.3 )) # compensate for our time budget
    ##
    local bell_awaysh=no hear_loudidle=no
    bell-visual-flash1
    bell-lm-whattimeisit
    local m=$(( int(i * i_dur / 60) ))
    if (( m <= 200 )) ; then
        tts-glados1-cached "$m minutes late ... so so late"'!'
    else
        tts-glados1-cached 'I'\''m late, I'\''m late! For a very important date! No time to say '\''hello, goodbye,'\'' I'\''m late, I'\''m late, I'\''m late!'
    fi
}
function timer-late() {
    @opts notify no @ timer "${1:-30}" eval lo_s="${2:-60}" loop awaysh h_timer-late
}
noglobfn timer-late
##
function stopwatch {
    local precision="${1:-3}"
    if [ "$precision" -eq 0 ]; then
        start="$(gdate '+%s')"
    else
        start="$(gdate "+%s%${precision}N")"
    fi

    {
        # colorfg 40 200 30 ;
        Bold
        while true; do
            if [ "$precision" -eq 0 ]; then
                now="$(gdate '+%s')"
                time="$((now - start))"
                printf "%s\r" "$(gdate -u -d "@$time" '+%H:%M:%S')"
            else
                now="$(gdate "+%s%${precision}N")"
                time="$((now - start))"
                seconds="$((time / 10**precision))"
                subseconds="$((time % 10**precision))"
                printf "%s.%0${precision}d\r" "$(gdate -u -d "@$seconds" '+%H:%M:%S')" "$subseconds"
            fi
        done
    } always { resetcolor }
}
##
function time-from-human {
    local days="${time_from_human_days:-${time_from_human_day:-${time_from_human_d:-0}}}"
    local hours="${time_from_human_hours:-${time_from_human_hour:-${time_from_human_h:-0}}}"
    local minutes="${time_from_human_mins:-${time_from_human_min:-${time_from_human_m:-0}}}"
    local secs="${time_from_human_secs:-${time_from_human_sec:-${time_from_human_s:-0}}}"

    local total_secs=$((days*24*60*60 + hours*60*60 + minutes*60 + secs))
    local date_in_time=$(( EPOCHSECONDS + total_secs ))

    ecbold "Days: $days, Hours: $hours, Minutes: $minutes, Seconds: $secs"
    ecbold "$(TZ="${TZ:-Asia/Tehran}" gdate -d "@${date_in_time}" +'%Y-%m-%d %A %H:%M:%S')"

    ec "$total_secs" |
        cat-copy-if-tty
}
##
