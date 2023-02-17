##
function gcalcli-m {
    #: `gcalcli-m --calendar feraidoonmehri@gmail.com --noauth_local_webserver list`
    ##
    assert-args gcal_client_id gcal_client_secret @RET

    command gcalcli \
        --client-id="${gcal_client_id}" \
        --client-secret="${gcal_client_secret}" \
        "$@"
}

function h-gcal-dur-preprocess {
    local duration="$1"
    local allday="${gcal_allday}"

    if [[ "$duration" =~ '^(.*)h$' ]] ; then
        duration="${match[1]}"

        if ! bool "$allday" ; then
            duration=$(( int(duration * 60) )) #: converts hours to minutes
        fi
    fi

    ec "$duration"
}

function gcal-add {
    #: * @docs `lesh gcalcli add`
    ##
    assert-args gcal_client_id gcal_client_secret @RET

    local opts=("$@")
    local cal="${gcal_cal:-${gcal_cal_default}}"
    local title="${gcal_title}"
    local when="${gcal_when}"
    #: * when is parsed first by
    #: ** [[https://dateutil.readthedocs.io/en/stable/parser.html][parser — dateutil 2.8.2 documentation]]
    #: ** and if failed by [[https://github.com/bear/parsedatetime][bear/parsedatetime: Parse human-readable date/time strings]]

    assert-args cal title when @RET
    local duration="${gcal_dur}"
    #: duration is in minutes unless --allday provided in which case it is in days

    local desc="${gcal_desc}"
    local color="${gcal_color}"
    #: lavender, sage, grape, flamingo, banana,  tangerine, peacock, graphite, blueberry, basil,  tomato

    local allday="${gcal_allday}"
    #: The event will be an all-day event (possibly multi-day if --duration is greater  than 1). The time part of the --when will be ignored.

    local reminder="${gcal_rem}"
    #: Reminders in the form "TIME METH" or "TIME". TIME is a  number which may be followed by an optional "w", "d",  "h", or "m" (meaning weeks, days, hours, minutes) and  default to minutes. METH is a string "popup", "email",  or "sms" and defaults to popup.

    if [[ "$when" =~ '^@(.*)$' ]] ; then
        #: unix timestamp
        when="${match[1]}"
        when="$(date-unix-to-3339 "${when}")" @TRET
    fi

    duration="$(gcal_allday="$allday" h-gcal-dur-preprocess "$duration")" @TRET

    if bool "$allday" ; then
        opts+=("--allday")

        if test -z "$duration" ; then
            duration=1
        fi
    fi
    if test -z "$duration" ; then
        duration=60
    fi

    if test -n "$desc" ; then
        opts+=(--description "$desc")
    fi

    if test -n "$color" ; then
        opts+=(--color "$color")
    fi

    if test -n "$reminder" ; then
        opts+=(--reminder "$reminder")
        #: --default-reminders is false by default
    fi

    if (( duration <= 0 )) ; then
        ecerr "$0: duration should be positive: $duration"
        return 1
    fi

    reval-ec gcalcli-m \
        --calendar "${cal}" \
        add \
        --title "${title}" \
        --when "${when}" \
        --duration "${duration}" \
        --noprompt \
        "${opts[@]}" \
        "$@"
}
@opts-setprefix gcal-add gcal
##
