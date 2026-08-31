##
#: Whether DND is on is one fact, so both sides of the toggle raise their alert
#: under the same id. The v2 engine stacks a band per alert, so without an id a
#: run of toggles leaves a column of contradictory bands on screen; re-using the
#: id rewrites the existing band instead. The text still differs between on and
#: off, so the change is not treated as a heartbeat and does flash.
#: See [agfi:hs-alert-v2] for the knob, and the "** Gateway" section of
#: hammerspoon/alert/api.lua for the engine side.
typeset -g focus_dnd_alert_id='focus-dnd'
##
function focus-off {
    if isDarwin ; then
       shortcuts run 'Focus Off'

        alert_id="$focus_dnd_alert_id" alert "Do Not Disturb: off"
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

        alert_id="$focus_dnd_alert_id" alert "Do Not Disturb: ON"
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
