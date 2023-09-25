##
function iwidget-rem {
    #: @see =rem_comingup_days=
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
##
