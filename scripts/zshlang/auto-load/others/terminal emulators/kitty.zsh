function kitty-remote() {
    : "Invoke with no args to enter the kitty shell"

    if isKitty && ! isTmux ; then
        kitty @ "$@"
    else
        local s i ret=1
        s=("$HOME/tmp/.kitty"*(DN)) # do NOT use '.' glob as these aren't 'files'
        if (( $#s >= 1 )) ; then
            # dead kitties can leave trash sockets behind
            for i in ${s[@]} ; do
                if kitty @ --to unix:${i} "$@" ; then
                    ret=0
                else
                    silent trs "$i" # garbage collecting dead sockets
                    # @futureCron did this delete live sockets as well?
                fi
            done
        fi

        if (( ret != 0 )) ; then
            ecerr "$0: could not send commands to any Kitty instance"
        fi
        return $ret
    fi
}
function kitty-send() {
    local m="${kitty_send_match}"
    in-or-args2 "$@"

    ec "${inargs[@]}" | kitty-remote send-text --match "$m" --stdin
    # https://sw.kovidgoyal.net/kitty/remote-control.html#kitty-send-text
}
function kitty-C-c() {
    kitty-send $'\C-c' #$'\n''reset'
}
@opts-setprefixas kitty-C-c kitty-send
function kitty-esc() {
    local m="${kitty_send_match}"
    # kitty-send $'\^['
    # kitty-send "$(printf '\x1b')"
    printf -- '\x1b\x1b\x1b\x1b\x1b\n' | kitty-remote send-text --match "$m" --stdin
}
@opts-setprefixas kitty-esc kitty-send
##
function kitty-tab-activate() {
    local i="${1:-5}"

    kitty-remote focus-tab --match="id:${i}"
    # https://sw.kovidgoyal.net/kitty/remote-control.html#kitty-focus-tab"
}
##
function kitty-tab-get() {
    local id="${1:-$KITTY_WINDOW_ID}"
    if test -z "$id" ; then
        if isTmux ; then
            ecgray "$0: No 'id' supplied. tmux detected; Ignoring."
            return 1
        else
            assert-args id @RET
        fi
    fi

    kitty-remote ls | jq -r --arg wid "$id" '.[] | select(.is_focused == true) | .tabs[] | select(.windows[0].id == ($wid | tonumber))'
}
function kitty-tab-get-emacs() {
    kitty-remote ls | {
        # jq -r '.[] | select(.is_focused == true) | .tabs[] | select(.windows[0].foreground_processes[0].cmdline[0] | contains("emacs") ) | .id'
        ##
        jq -r '.[] | select(.is_focused == true) | .tabs[] | select(.windows[0].cmdline[]  | (contains("emc-gateway") and (contains("withemc") | not)) ) | .id'
    }
}
##
# if isIReally && isKitty ; then
#     typeset -g kitty_emacs_id="$(kitty-tab-get-emacs | ghead -n 1)"
# fi
function kitty-emacs-focus() {
    local id=5

    ## cached for perf reasons: (the cache needs to invalidated each time 'kemc' is called
    if isDeus || test -z "$kitty_emacs_id" ; then
        id="$(kitty-tab-get-emacs | ghead -n 1)" @TRET
        typeset -g kitty_emacs_id="$id"
    else
        id="$kitty_emacs_id"
    fi
    ##

    if ! kitty-tab-activate "$id" ; then
        if ! isDeus ; then
            deus "$0" "$@"
        fi
    fi
    ## perf:
    # `time (kitty-tab-get-emacs | ghead -n 1)` 0.2s
    # `time2 kitty-emacs-focus` 0.50343608856201172
    # `time2 kitty-tab-activate 5 ` -> 0.165s
    ##
}

function kitty-tab-is-focused() {
    [[ "$(kitty-tab-get | jq -r '.is_focused')" == 'true' ]]
}
##
# redis-defvar kitty_focused
function kitty-is-focused() {
    ##
    [[ "$(frontapp-get)" == 'net.kovidgoyal.kitty' ]]
    ## does not work, is always true, probably tracks the last active kitty window regardless of OS frontapp-getfocus:
    # kitty-remote ls | jq -r '.[] | .is_focused'
    ##
    # [[ "$(kitty_focused_get)" == 1 ]]
    ##
}
##
function kitty-launch-emc() {
    kitty-remote launch '--type=tab' "${commands[zsh]}" -c "fnswap isColor true retry-limited 2 $@ emc-gateway"
    # The retry is to work around the recent emacs/doom issue that kills the starting frame (and sometimes all the frames, when doom themes are used).
    ##
    # This doesn't work, as somehow our config is not loaded. It will work if there is already a server running on EMACS_SOCKET_NAME though
    # kitty @ launch '--type=tab' bash -c 'TERM=xterm-emacs EMACS_SOCKET_NAME=$HOME/tmp/.emacs ALTERNATE_EDITOR= emacsclient -t ; sleep 10'
    ##
}
alias kemc='kitty-launch-emc'

aliasfn withemc1 'EMACS_SOCKET_NAME="$EMACS_ALT1_SOCKET_NAME" emacs_night_server_name="$EMACS_ALT1_SOCKET_NAME"' reval
function kemc1 {
    kitty-launch-emc withemc1
}
##
function kitty-launch-icat() {
    ## use this for troubleshooting:
    # kitty @ launch --type=tab env PATH="$PATH" "$(which wait4user.sh)" "$(which kitty)" +kitten icat "$@"
    # @raceCondition sometimes this does not work, no idea why
    ##
    local f fs=()
    for f in $@ ; do
        fs+="$(grealpath -e "$f")" @TRET
    done

    # @see icat-kitty-single
    revaldbg serrdbg kitty-remote launch --type=overlay env PATH="$PATH" "$(which kitty)" +kitten icat --hold --place "${COLUMNS}x${LINES}@0x0" --scale-up "$fs[@]"
    ##
}
##
function kitty-theme() {
    # the theme names should be stripped of '.conf' before being fed to kitty-theme, and they should not be in abs paths:
    # `kitty-theme --test Zenburn `
    ##
    ensure-dep-kitty-theme

    command kitty-theme -c $NIGHTDIR/configFiles/kitty/kitty_theme_changer.conf.py "$@"
}

function kitty-theme-setup() {
    kitty-theme --setl night-solarized-light

    kitty-theme --setd ayu
}

function kitty-theme-toggle() {
    kitty-theme --toggle --live
}

function kitty-theme-test() {
    local theme q="$(fz-createquery "$@")"

    local dir
    dir=~/.config/kitty/kitty-themes/themes/

    theme="$(fd --extension conf . "$dir" | dir-rmprefix "$dir" | command sd '\.conf$' '' | fz)" @RET

    reval-ec kitty-theme --test "$theme"
}

function kitty-theme-reload() {
    kitty-theme --live
}
##
