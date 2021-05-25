##
function coursera-dl() {
    if isIran ; then
        ecerr "$0: coursera-dl does not support proxies, please enable a VPN."
        return 1
    fi

    local opts=()

    local CAUTH="${coursera_dl_ca}"
    if test -z "$CAUTH" ; then
        CAUTH="$(cookies 'https://www.coursera.org/' | cookies-extract CAUTH)"
        assert test -n "$CAUTH" @RET
    fi

    opts+=( --cookies-header "$(getcookies 'https://www.coursera.org/')" )

    # @redundant the user and pass don't seem to be used when CAUTH is present
    reval-ec command coursera-dl -ca="$CAUTH" -n -pl --aria2 --video-resolution 720p --download-quizzes --download-notebooks -sl "en,fa" --resume --aria2 aria2c --playlist --debug "$opts[@]" "$@"
    ##
    # --specialization
    # --cookies-header "$(getcookies 'https://www.coursera.org/')"
    # -u "$coursera_user" -p "$coursera_pass"
    ##
    ecgray "$0: search for 'Error|exam|assignment' (exams and assignments might produce some errors, that's okay) when the download finishes, it may not have downloaded everything"
    ## tests:
    # `coursera-dl international-criminal-law`
    ##
}
function coursera-auth-test() {
    # perhaps update 'useragent_chrome'
    curl-cookies 'https://api.coursera.org/api/memberships.v1?includes=courseId,courses.v1&q=me&showHidden=true&filter=current,preEnrolled'
}
##
