##
function focus-off {
    if isDarwin ; then
       shortcuts run 'Focus Off'
    else
        @NA
    fi
}

function focus-get {
    if isDarwin ; then
        ##
        #: @bug When we use Siri Shortcuts to change the focus, this program doesn't get the updates.

        # focus_get.jxa
        ##
        local tmp
        tmp="$(gmktemp --suffix='.txt')" @TRET

        shortcuts run 'Get Focus' -o "$tmp" @RET
        #: Uses 'get text from input' in the shortcut to convert the focus output to text.

        cat "$tmp"
        ##
    else
        @NA
    fi
}
##
function focus-do-not-disturb-p {
    local focus
    focus="$(focus-get)" @TRET

    [[ "$focus" == 'Do Not Disturb' ]]
}

function focus-do-not-disturb-on {
    if isDarwin ; then
        shortcuts run 'Focus Set: Do Not Disturb'
    else
        @NA
    fi
}

function focus-do-not-disturb-toggle {
    if focus-do-not-disturb-p ; then
        focus-off
    else
        focus-do-not-disturb-on
    fi
}
##
