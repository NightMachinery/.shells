##
# Set terminal window and tab/icon title
#
# usage: title short_tab_title [long_window_title]
#
# See: http://www.faqs.org/docs/Linux-mini/Xterm-Title.html#ss3.1
# Fully supports screen, iterm, and probably most modern xterm and rxvt
# (In screen, only short_tab_title is used)
# Limited support for Apple Terminal (Terminal can't set window and tab separately)
function title {
    # forked from OMZ, see https://superuser.com/a/344397/856545 for setting tab and window separately
    emulate -L zsh

    #: No `prompt_subst' and no `print -P' below, deliberately: prompt
    #: expansion *executes* a `$(...)' in the title text, and the `:q' this was
    #: forked with does not stop it -- `:q' backslash-escapes, then `print'
    #: (without -r) strips exactly those backslashes again before the expansion
    #: runs. Titles come from media filenames [agfi:mpv], `$PWD' [agfi:iloop]
    #: and the fuzzy pickers, so that was reachable. `printf' takes its
    #: arguments as data. See =./docs/tmux-tty-title.md=.
    ##
    {
        [[ "$EMACS" == *term* ]] && return

        # if $2 is unset use $1 as default
        # if it is set and empty, leave it as is
        : ${2=$1}

        #: A control character in the title would end the OSC early, letting
        #: the tail through as a fresh escape sequence.
        local tab="${1//[$'\n\r\a\e']/ }" win="${2//[$'\n\r\a\e']/ }"

        #: tmux understands OSC 2 whatever TERM it advertises inward, and
        #: `$TMUX' says so without depending on that TERM. Without this branch
        #: `tmux-direct' matched no arm below and worked only by way of the
        #: terminfo tsl/fsl fallback, while a tmux advertising `screen-256color'
        #: took the `screen*' arm -- whose \ek is a *window rename*, discarded
        #: outright under `allow-rename off'. tmux drops OSC 1, so only OSC 2
        #: is sent. Reaching kitty from here also needs `set-titles on'; see
        #: =./docs/tmux-tty-title.md=. [agfi:isTmux]
        if isTmux ; then
            printf '\e]2;%s\a' "${win}"
            return 0
        fi

        case "$TERM" in
            #: `tmux*' is for a shell reached by ssh *from* a pane: [agfi:ssh]
            #: forwards TERM but not `$TMUX', and the outer tmux is still what
            #: parses the escape. `screen*' below is left alone -- it is
            #: ambiguous between real GNU screen and a tmux set to a `screen*'
            #: default-terminal, and \ek is all real screen understands.
            cygwin|xterm*|putty*|rxvt*|tmux*|ansi)
                printf '\e]2;%s\a' "${win}" # set window name
                printf '\e]1;%s\a' "${tab}" # set tab name
                ;;
            screen*)
                printf '\ek%s\e\\' "${tab}" # set screen hardstatus
                ;;
            *)
                if [[ "$TERM_PROGRAM" == "iTerm.app" ]]; then
                    printf '\e]2;%s\a' "${win}" # set window name
                    printf '\e]1;%s\a' "${tab}" # set tab name
                else
                    # Try to use terminfo to set the title
                    # If the feature is available set title
                    if [[ -n "$terminfo[fsl]" ]] && [[ -n "$terminfo[tsl]" ]]; then
                        echoti tsl
                        printf '%s' "${tab}"
                        echoti fsl
                    fi
                fi
                ;;
        esac
    } >/dev/tty
}
function tty-title {
    if bool "${tty_title_f}" || { isTty && isI } ; then
        local text="$@"

        title "$text" "$text"
    fi
}
##
function terminfo-set-auto {
    # if isTmux ; then
    #     export TERM='screen-256color'
    #     return $?
    # fi

    if isKitty ; then
        export TERM='xterm-kitty' # You need to use kitty-terminfo-install on each new server though
        # Idk why this gets reseted on mosh. It's inherited correctly in SSH ...
    fi

    # export TERM="xterm-256color" # Might do a lot of damage. Added for multi-term.
}
##
