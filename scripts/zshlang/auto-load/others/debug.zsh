##
function zerr_handler {
    # [[https://github.com/sindresorhus/pure/issues/603][sindresorhus/pure#603 Do not throw uncatched exceptions (i.e., handle all non...]]
    #
    # [[https://unix.stackexchange.com/questions/664117/zsh-how-to-store-the-return-code-of-a-statement-without-triggering-set-e][shell - zsh: How to store the return code of a statement without triggering `...]]
    ##
    local r=$? ps=("$pipestatus[@]") skip="${zerr_skip}" # @alt `&& true` also skips zerr

    if ! bool "$zerr_skip" ; then
        case $r in
            895)
                # ectrace "$0: $r ; subshell level: $ZSH_SUBSHELL" || true

                if true ; then
                    if isBrish ; then
                        ecerr "CNOTFOUND; Exiting ..."
                        tts-glados1-cached "command not found" || true
                        exit 127
                    else
                        ask "CNOTFOUND; Press enter to proceed, or use C-c or C-d to manually halt the execution." Y
                    fi
                    return $r
                else
                    # zer_handler will be triggered around 5 times on a single CNOTFOUND error, so asking the user here is infeasable.
                    # update: this won't work if only 895 is caught; You need to use '127|895'.
                    #
                    if ask "CNOTFOUND: exit? (bug: this question will be asked multiple times; only the last one matters)" N ; then
                        exit 127
                    else
                        return $r
                    fi
                fi
                ;;
        esac
    fi

    {
        return $r
    } always {
        typeset -g pipestatus=("$ps[@]") # doesn't work
        typeset -g pipestatus_preserved=("$ps[@]")
    }
}

trap zerr_handler ZERR
##
function command_not_found_handler {
    # [[https://unix.stackexchange.com/questions/664089/zsh-halt-when-command-not-found][shell script - zsh: Halt when command not found - Unix & Linux Stack Exchange]]
    ##
    # If no external command is found but a function command_not_found_handler exists  the
    # shell  executes this function with all command line arguments.  The return status of
    # the function becomes the status of the command.  If the function wishes to mimic the
    # behaviour  of  the  shell when the command is not found, it should print the message
    # `command not found: cmd' to standard error and return status  127.   Note  that  the
    # handler  is  executed  in  a  subshell  forked to execute an external command, hence
    # changes to directories, shell parameters, etc. have no effect on the main shell.
    ##
    ectrace_ret=127 ectrace_notrace=y ectrace "command not found: $(gq "$@")"
    # @old (WARNING: execution not necessarily halted)

    # return 127
    return 895 # 895 = 127+256*3 ; This ensures that unrelated 127 exits do not trigger the command not found machinary in zerr
}
##
