function '.a'() {
    if isDarwin ; then
        brishzr .a "$@"
    else
        # eval "aget $(gquote "$@")"
        aget "$(gquote "$@")"
    fi
}
