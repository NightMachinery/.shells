function iwidget-rem() {
    # Beware that the cache hides the newly added upcoming events. We can invalidate the cache in reminday_store though.
    @opts key "$(date '+%Y/%m/%d')" @ memoi-eval iwidget-rem_h "$@"
}
function iwidget-rem_h() {
    datej-all
    ec
    rem-summary
}
