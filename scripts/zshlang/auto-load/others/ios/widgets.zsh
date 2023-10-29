##
function iwidget-rem {
    #: * @see =rem_comingup_days=
    #: * @usage
    #  ** =withremc iwidget-rem=
    ##
    if ! rem-enabled-p ; then
        return 1
    fi

    datej-all
    ec
    ##
    # Beware that the cache hides the newly added upcoming events. Use `iwidget-rem-refresh`.
    # @opts key "$(date '+%Y/%m/%d')" @ memoi-eval rem-summary
    ##
    rem-summary
}

function iwidget-rem-filter {
    local query="$*"
    iwidget-rem |
        rgm --no-context-separator --before-context=1 --after-context=1 "${query}"
}

function iwidget-rem-oneliner {
    ec "$(datej-all-short): $(rem-summary-today)"
}

function iwidget-rem-short {
    ec "$(datej_all_mode=4 datej-all)"$'\n'"$(rem_comingup_days=0 rem-summary)"
}

function iwidget-rem-refresh {
    return 0
    #: no longer using a cache or a wallpaper
    ##
    if [[ "$remindayDir" == "$remindayCDir" ]] ; then
        ecerr "$0: remindayDir is set to remindayCDir; Skipping refresh."
        return 0
    fi

    deus iwidget-rem #: refresh the cache

    if isLocal ; then
        if bool "$wallpaper_reminder_p" ; then
            awaysh wallpaper-auto
        fi

        brishzr awaysh deus iwidget-rem-refresh #: refresh the cache
    fi
}
##
