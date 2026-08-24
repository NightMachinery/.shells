BTT_HS_NOISES_UID='5DBF0BE7-5822-459A-B450-36E3396124F9'
##
function hammerspoon-reload {
     hammerspoon -c "hs.reload()"
}
alias hsr='hammerspoon-reload'
alias hhh='hammerspoon-reload'
##
function hs-popclickBttToggle() {
    local lis="$(serr hammerspoon -c 'popclickBttToggle()')"
    hs-popclick-btt-refresh
    @opts say '' @ hs-popclick2icon $lis

}
function hs-popclick-btt-refresh() {
    btt-refresh "$BTT_HS_NOISES_UID"
}
function hs-popclickBttGet() {
    local lis
    lis="$(serr hammerspoon -c 'popclickBttGet()')" || {
        ecerr "$0: could not get value from hammerspoon"
        return 1
        ##
        lis=''
    }
    hs-popclick2icon "$lis"
}
@opts-setprefix hs-popclickBttGet hs-popclick2icon
function hs-popclick2icon() {
    local lis="${1}" say="${hs_popclick2icon_say}"

    test -n "$say" && { pgrep -f HS_POPCLICK_HI | inargsf kill-withchildren }
    if [[ "$lis" == true ]] ; then
        ec "🎆"
        test -n "$say" && {
            awaysh-named HS_POPCLICK_HI hearinvisible "$GREENCASE_DIR/LittleMisfortune/23_06_MI_thatsmagicaldontyouthink.flac"
            # fsay "The magic flows"
        }
    elif [[ "$lis" == false ]] ; then
        ec "🌌"
        test -n "$say" && {
            awaysh-named HS_POPCLICK_HI hearinvisible "$GREENCASE_DIR/LittleMisfortune/09_23_MI_itdoesntlookthatmagical.flac"
            # fsay "Sealed forever"
        }
    else
        ec "🥶"
        test -n "$say" && {
            awaysh-named HS_POPCLICK_HI hearinvisible "$GREENCASE_DIR/LittleMisfortune/05_30_MI_yikesforever.flac"
            # fsay "Yikes forever"
        }
    fi
    true
}
##
function gradS-get() {
    hammerspoon -c gradS # locking done via garden sessions
}
##
# gradS=E
# function h_gradS-get() {
#     local lockStr="lock_gradS"
#     local lock="$(redism setnx $lockStr 5)"
#     if [[ "$lock" == 1 ]] ; then
#         gradS="$(hammerspoon -c gradS)"
#         silent redism del "$lockStr"
#     else
#         silent redism expire "$lockStr" 60 # for resilience
#     fi
# }
# function gradS-get() {
#     h_gradS-get
#     ec "$gradS"
# }
##
# function h_gradS-get() {
#     local lockStr="lock_gradS"
#     local lock="$(redism setnx $lockStr 5)"
#     if [[ "$lock" == 1 ]] ; then
#         deus @opts expire 0 od 0 @ eval-memoi hammerspoon -c gradS
#         silent redism del "$lockStr"
#     else
#         silent redism expire "$lockStr" 300 # for resilience
#         @opts expire 0 od 0 @ eval-memoi hammerspoon -c gradS
#     fi
# }
# function gradS-get() {
#     local out="$(h_gradS-get)"
#     if test -z "$out" ; then
#         out='empty'
#     fi
#     ec "$out"
# }
##
# function h_gradS-get() {
#     local lockStr="lock_gradS"
#     local lock="$(redism setnx $lockStr 5)"
#     if [[ "$lock" == 1 ]] ; then
#         hammerspoon -c gradS
#         silent redism del "$lockStr"
#         return 0
#     else
#         silent redism expire "$lockStr" 300 # for resilience
#         return 1
#     fi
# }
# function gradS-get() {
#     retry_sleep=0.1 serr retry h_gradS-get
# }
##
function hs-alert-v1 {
    @darwinOnly

    local msg="$*" dur="${alert_dur:-5}"

    msg="$(ecn $msg | text-wrap 90 | sdlit $'\n' '\n' | sdlit '"' '\"')" @TRET
    sout hammerspoon -c "hs.alert (\"$msg\", ${dur})" # outputs a UUID thingy
    # https://www.hammerspoon.org/docs/hs.alert.html
}
##
# The v2 engine lives in hammerspoon/core/alert-engine.lua: coloured bands that
# stack instead of hiding each other, wrap instead of being cut off, and can
# flash the whole screen first.
#
# Knobs, all overridable per call:
#   alert_dur     seconds on screen (default 5)
#   alert_flash   fullscreen flash before settling, in seconds; 0 skips it
#   alert_pos     top (default), center, bottom
#   alert_id      reusing an id updates that alert in place instead of stacking
#   alert_markup  plain (default) or md
#   alert_color   band colour by name: default, warn/amber, crit, agent, free
#
# `md` is a small markdown subset - **bold**, *italic*, ~~strike~~, and
# [text]{red bold} for colour, which markdown has none of. Anything that does
# not parse renders literally, so a typo shows up rather than vanishing. See
# the "** Markup" section of alert-engine.lua for the whole grammar.
#
# Both new knobs are bare words, so unlike the message they are safe to inline
# in the command string; the colour is resolved by name on the Lua side rather
# than sending an RGB table over ipc.
#
# The message travels in a file rather than in the command string. `hammerspoon
# -c` hangs on payloads of a few hundred characters and takes the ipc port down
# with it until the client is killed, so inlining the text - escaped or
# base64-encoded, it makes no difference - breaks on exactly the long command
# output this is most useful for. Only the path goes over ipc; the Lua side
# reads the file and deletes it.
#
# No text-wrap either: the engine wraps against actual pixels, so it knows the
# screen width and we do not have to guess at 90 columns.
function hs-alert-v2 {
    @darwinOnly

    local msg="$*" dur="${alert_dur:-5}"
    local flash="${alert_flash:-0.2}" pos="${alert_pos:-top}"
    local id="${alert_id:-}"
    local markup="${alert_markup:-plain}" color="${alert_color:-}"

    # Not `local path`: `path` is the array tied to $PATH, so declaring it local
    # and assigning a string to it empties PATH for the rest of the function.
    # Everything after that, `command mktemp` included, is a command-not-found.
    local msgfile
    msgfile="$(command mktemp "${TMPDIR:-/tmp}/hs-alert-v2.XXXXXX")" @TRET
    ecn "$msg" > "$msgfile" @TRET

    local opts="seconds = ${dur}, flashSeconds = ${flash}, position = \"${pos}\""
    opts+=", markup = \"${markup}\""
    if test -n "$id" ; then
        opts+=", id = \"${id}\""
    fi
    if test -n "$color" ; then
        opts+=", color = \"${color}\""
    fi

    reval-dbg sout hammerspoon -c "alertV2FromFile(\"${msgfile}\", { ${opts} })"
}
aliasfn hs-alert hs-alert-v2
aliasfn alert hs-alert
#: Without these, `@opts dur 10 @ hs-alert msg` derives its prefix from the
#: command name and sets =hs_alert_dur=, which nothing reads.
#: [agfi:ensure-var-name] sanitises the dash away instead of erroring, so the
#: knob is silently ignored. `alert` happens to work by accident; pin all three.
@opts-setprefix hs-alert-v2 alert
@opts-setprefix hs-alert alert
@opts-setprefix alert alert
##
function hs-reval-alert {
    local alert_dur="${alert_dur:-1}"

    local out retcode=0
    out="$(reval "$@" 2>&1)" || retcode=$?

    if test -z "$out" ; then
        out="[No Output]"
    fi

    if (( retcode != 0 )) ; then
        out="[Error Code: $retcode]"$'\n\n'"${out}"
    fi

    out="> $(gq "$@")"$'\n\n'"${out}"

    hs-alert "$(ec "$out" | head -n 30)"
}
##
function hs-hyper-z() {
    hammerspoon -c 'eventtap.keyStroke(hyper, 6)'
}
function hs-hyper-x() {
    hammerspoon -c 'eventtap.keyStroke(hyper, 7)'
}
function hs-hyper-m() {
    hammerspoon -c 'eventtap.keyStroke(hyper, 46)'
}
function hs-cmd-v() {
    hammerspoon -c 'eventtap.keyStroke({"cmd"}, 9)'
}
##
function hs-type {
    local input
    input="$(in-or-args "$@")" @RET

    reval-ec hammerspoon -c "hs.eventtap.keyStrokes($(js-quote "$input"))"
}
##
function hs-focus-app {
    local app_name="$1"
    assert-args app_name @RET

    hammerspoon -c "focusApp('${app_name}')"
}

function focus-app {
    if isDarwin ; then
        hs-focus-app "$@"
    else
        @NA
    fi
}
##
