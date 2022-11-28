##
function iwidget-rem {
    if ! rem-enabled-p ; then
        return 1
    fi

    datej-all
    ec
    # Beware that the cache hides the newly added upcoming events. Use `iwidget-rem-refresh`.
    @opts key "$(date '+%Y/%m/%d')" @ memoi-eval rem-summary
}
##
