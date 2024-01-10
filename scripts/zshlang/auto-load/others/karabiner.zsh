###
#: Sends =15    SIGTERM=.
#:
#: Add it to the sudoers file:
#: `sudoers-no-pass pkill`
#: evar ALL=(ALL) NOPASSWD: sha256:... /usr/bin/pkill -li karabiner
##
typeset -g KARABINER_RESET_LOG=~/logs/karabiner_reset.log

function h-karabiner-reset {
    log-to "${KARABINER_RESET_LOG}" \
        sudo pkill -li "$@"
}

function karabiner-reset {
    h-karabiner-reset karabiner_console_user_server
}

function karabiner-reset {
    h-karabiner-reset karabiner
}
###
