##
function editor-open {
    local files=("${editor_open_f[@]}") linenumbers=("${editor_open_l[@]}") no_wait="${editor_open_no_wait}" editor="${editor_open_e:-$EDITOR}"

    if [[ "$editor" =~ '^emacs' ]] ; then
            local opts=()
            if test -n "$no_wait" ; then
                opts+='--no-wait'
                # --no-wait will open stuff in the active frame, instead of opening a new frame
            fi
            local cmd='(progn '
            for i in {1..$#files} ; do
                # (forward-char $col)
                ##
                if (( $#files > 1 )) ; then
                    cmd+="(tab-bar-new-tab) "
                    if (( i == 1 )) ; then
                        cmd+="(tab-bar-close-other-tabs) " # @weirdFeature
                    fi
                fi
                cmd+="(find-file \"${files[$i]}\") (goto-line ${linenumbers[$i]}) "
                ##
                cmd+='(recenter) '
            done
            # lower than this 1.5 delay will not work. More delay might be necessary for edge cases.
            # The first time we use this in a zsh session, the highlight sometimes does not work. Idk why.
            cmd+="(run-at-time 0.15 nil #'+nav-flash-blink-cursor-h) "
            cmd+=')'

            if test -n "$no_wait" ; then
                revaldbg sdbg emc-eval "$cmd" & # this decreases the latency but can sometimes open emacs too soon (before it has jumped to the desired location)
                sdbg emc-focus
            else
                revaldbg sdbg emacsclient "${opts[@]}" -a '' -t -e "$cmd"
            fi
        else
            ##
            # should work with both emacs and vim
            # VSCode: code --goto <file:line[:character]> Open a file at the path on the specified line and character position.--goto file:line[:col]
            # I don't know about opening multiple files on vscode (we can always run the command multiple times)
            ##
            # [[id:dc56c812-14ba-4f42-8484-18456dc9132b][vim/tabs.org]]
            # editor='nvim -p'
            ##
            local cmd="$editor "
            for i in {1..$#files} ; do
                cmd+="+${linenumbers[$i]} $(gq "${files[$i]}") "
            done
            eval "$cmd"
        fi
}
##
