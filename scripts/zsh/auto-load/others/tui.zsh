sleep-neon()  {
    mdoc 'Usage: sleep-neon <seconds>' MAGIC
    integer inter="$1"

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
    print -- "\rWoke up!                                                               "
}
