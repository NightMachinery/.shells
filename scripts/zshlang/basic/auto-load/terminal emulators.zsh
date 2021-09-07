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
    setopt prompt_subst

    {
        [[ "$EMACS" == *term* ]] && return

        # if $2 is unset use $1 as default
        # if it is set and empty, leave it as is
        : ${2=$1}

        case "$TERM" in
            cygwin|xterm*|putty*|rxvt*|ansi)
                print -Pn "\e]2;$2:q\a" # set window name
                print -Pn "\e]1;$1:q\a" # set tab name
                ;;
            screen*)
                print -Pn "\ek$1:q\e\\" # set screen hardstatus
                ;;
            *)
                if [[ "$TERM_PROGRAM" == "iTerm.app" ]]; then
                    print -Pn "\e]2;$2:q\a" # set window name
                    print -Pn "\e]1;$1:q\a" # set tab name
                else
                    # Try to use terminfo to set the title
                    # If the feature is available set title
                    if [[ -n "$terminfo[fsl]" ]] && [[ -n "$terminfo[tsl]" ]]; then
                        echoti tsl
                        print -Pn "$1"
                        echoti fsl
                    fi
                fi
                ;;
        esac
    } >/dev/tty
}

function tty-title() {
    isI || return
    local text="$@"

    title "$text" "$text"
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
