##
function h-postmsg {
    local msg="${NIGHT_POSTMSG}"
    #: Note that forked shells can't set variables for the main shell, so need to be careful where you set NIGHT_POSTMSG.

    unset NIGHT_POSTMSG

    #: @hack to get around the above forking problem
    local last_cmd
    last_cmd="$(hist-last 2> /dev/null)" || last_cmd=""

    if [[ "${last_cmd}" == *seg-result-load* ]] ; then
        clipboard-add "$(pbpaste)"
        #: ensures we don't lose data copied by the last command

        pbcopy "${last_cmd}"

        local f
        f="$(seg_result_input_file_get)" || f=""
        seg_result_input_file_del || true

        if test -n "$f" ; then
            local my_msg="${f:h:h:t}/${f:h:t}/${f:t}"

            PROMPT="$(colorfg "$gray[@]")${my_msg}$(colorreset)"$'\n'"${PROMPT}"

            ##
            # if test -n "$msg" ; then
            #     msg="${my_msg} ${msg}"
            # else
            #     msg="${my_msg}"
            # fi
            ##
        fi
    fi

    if test -n "$msg" ; then
        ##
        # ecgray "$msg"
        ##
        # RPROMPT="$(colorfg "$gray[@]")${msg}$(resetcolor)"
        ##
        RPROMPT="${msg}"
        ##
    else
        RPROMPT=""
    fi
}
##
