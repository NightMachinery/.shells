function zsh_cursor_color_disable1() {
    typeset -g zsh_cursor_color_disable1=y
}

function h-keymap-updated {
    if test -n "$zsh_cursor_color_disable1" ; then
        zsh_cursor_color_disable1=''
        return 0
    fi

    local old_keymap="${1}" new_keymap="${KEYMAP}"

    if [[ "$new_keymap" == vi* ]] ; then
        color-cursor "#7f7fffffd4d4"
    else
        if isSSH ; then
            # color-cursor '#ffffe4e4e1e1' # mistyrose1
            color-cursor '#ffffe4e4c4c4' # bisque
        else
            color-cursor "#c7ceff"
        fi
    fi
}

zle -N h-keymap-updated
add-zle-hook-widget zle-keymap-select h-keymap-updated

##
# @latency
# going to the next line doesn't call zle-keymap-select; I tried using zle-line-finish, but it conflicted with zsh-autosuggestions.
# precmd runs before the prompt, and so resets the color every single time, and does not let the last command affect the color.
# preexec runs before command execution, and so does not reset the color if no commands were executed, which is unfortunate.

# add-zle-hook-widget zle-line-finish h-keymap-updated

add-zle-hook-widget zle-line-init h-keymap-updated

# precmd_functions+='h-keymap-updated'

# typeset -ag preexec_functions
# preexec_functions+='h-keymap-updated'
