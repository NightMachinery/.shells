function tty-overlay() {
    local y="${tty_overlay_y:-0}" # use 2 to see it after scrolling
    ecn "$(
    tput sc # save cursor
    tput cup "$y" 0 # move cursor to Y X
    # print -n '\r' # redundant
    ecn "$*"
    tput rc # restore cursor
    )" > /dev/tty || {
        ecerr "$0: failed"
        exit
    }
}
##
function tty-statusbar-update() {
    local o
    if isDarwin ; then
        o="$(battery-status-darwin) |> $(date +"%H:%M") $(datej-all) |> $(memory-free-get) $(sysctl -n vm.loadavg)"
    else
        o="Not implemented"
    fi

    # o="$(ecn "$o" | command gay --bi --interpolation 1d --colour 24)" # slow, and potentially distracting

    o+='             '
    tty-overlay "$o"
    # @warn ruins the scrollback buffer
}

function tty-statusbar-update-loop() {
    lo_s=60 awaysh-named tty-statusbar loop tty-statusbar-update
}

function tty-statusbar-update-precmd() {
    # tty_overlay_y=2 tty-statusbar-update "$@"
    # tty_overlay_y=0 inbg tty-statusbar-update "$@"
    # tty_overlay_y=0 awaysh1 tty-statusbar-update "$@"
    # tty_overlay_y=0 awaysh tty-statusbar-update "$@"
    tty_overlay_y=0 tty-statusbar-update "$@" &|
    # to check latency impact, leep pressing =RET=, when it's slow, it would keep going on for a second after you let up)
    # the slowness is probably from other hooks in precmd_functions, as this function runs in the background
}

function tty-statusbar-enable() {
    if isSSH ; then
        return 0
    fi
    ##
    tty-statusbar-update-loop
    ##
    typeset -aUg precmd_functions
    precmd_functions+=tty-statusbar-update-precmd
}
##
