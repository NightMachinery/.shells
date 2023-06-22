##
function sleep-neon  {
    mdoc 'Usage: sleep-neon <seconds>' MAGIC

    integer inter="$1"

    if (( inter <= 0 )) ; then
        return 0
    fi

    ##
    # chalk-animation neon --duration $((inter*1000)) "Sleeping for $(seconds-fmt $inter) ..."
    ##

    local c
    while (( inter > 0 )) ; do
        if (( inter % 2 == 0 )) ; then
            c=(40 0 255)
            # c=(0 0 0)
            c="$(colorfg "$c[@]" ; Bold ; Invert)"
        else
            c=(40 0 255)
            # c=(0 0 0)
            c="$(colorfg "$c[@]" ; Bold)"
        fi
        print -n -- "\r${c}Sleeping for $(seconds-fmt-short $inter) ...$(colorreset)"
        sleep 1
        inter=$((inter-1))
    done
    print -- "\rSleep Done!                                                               "
}
##
function erase-ansi-old {
    in-or-args "$@" |
        gsed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[mGK]//g" |
        cat-copy-if-tty
}

function erase-ansi {
    # @alt http://www.andre-simon.de/doku/ansifilter/en/ansifilter.php
    ##
    in-or-args "$@" |
        {
            if true ; then
                perl -MTerm::ANSIColor=colorstrip -ne 'print colorstrip($_)'
                # Term::ANSIColor is part of the Perl core
            elif false && test -n "${commands[strip-ansi]}" ; then
                # too slow (nodeJS), but more reliable than erase-ansi-old
                command strip-ansi
            else
                erase-ansi-old
            fi
        } |
        cat-copy-if-tty
}

function reval-erase-ansi() {
    reval "$@" | erase-ansi
}
alias rea='reval-erase-ansi'
##
