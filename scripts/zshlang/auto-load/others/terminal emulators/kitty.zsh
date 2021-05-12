function kitty-remote() {
    : "Invoke with no args to enter the kitty shell"

    if isKitty ; then
        kitty @ "$@"
    else
        local s i
        s=("$HOME/tmp/.kitty"*(DN)) # do NOT use '.' glob as these aren't 'files'
        if (( $#s >= 1 )) ; then
            # dead kitties can leave trash sockets behind
            for i in ${s[@]} ; do
                kitty @ --to unix:${i} "$@"
            done
        fi
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
    assert-args id @RET

    kitty-remote ls | jq -r --arg wid "$id" '.[] | select(.is_focused == true) | .tabs[] | select(.windows[0].id == ($wid | tonumber))'
}
function kitty-tab-get-emacs() {
    kitty-remote ls | jq -r --arg wid "$id" '.[] | select(.is_focused == true) | .tabs[] | select(.windows[0].foreground_processes[0].cmdline[0] | contains("emacs") ) | .id'
}
function kitty-emacs-focus() {
    local id
    id="$(kitty-tab-get-emacs | ghead -n 1)" @TRET
    kitty-tab-activate "$id"
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
    kitty-remote launch '--type=tab' "${commands[zsh]}" -c emc-gateway
    ##
    # This doesn't work, as somehow our config is not loaded. It will work if there is already a server running on EMACS_SOCKET_NAME though
    # kitty @ launch '--type=tab' bash -c 'TERM=xterm-24bits EMACS_SOCKET_NAME=$HOME/tmp/.emacs ALTERNATE_EDITOR= emacsclient -t ; sleep 10'
    ##
}
alias kemc='kitty-launch-emc'
##
function kitty-launch-icat() {
    ##
    # kitty @ launch --type=tab env PATH="$PATH" "$(which wait4user.sh)" "$(which kitty)" +kitten icat "$@"
    # @raceCondition sometimes this does not work, no idea why
    ##
    local f fs=()
    for f in $@ ; do
        fs+="$(grealpath -e "$f")" @TRET
    done

    # @see icat-kitty-single
    revaldbg kitty-remote launch --type=overlay "$(which kitty)" +kitten icat --hold --place "${COLUMNS}x${LINES}@0x0" --scale-up "$fs[@]"
    ##
}
##
function kitty-theme() {
    # the theme names should be stripped of '.conf' before being fed to kitty-theme, and they should not be in abs paths:
    # `kitty-theme --test Zenburn `
    ##
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
