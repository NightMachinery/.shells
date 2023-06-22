BTT_HS_NOISES_UID='5DBF0BE7-5822-459A-B450-36E3396124F9'
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
function alert() {
    local msg="$*" dur="${alert_dur:-5}"

    msg="$(ecn $msg | text-wrap 90 | sdlit $'\n' '\n' | sdlit '"' '\"')" @TRET
    sout hammerspoon -c "hs.alert (\"$msg\", ${dur})" # outputs a UUID thingy
    # https://www.hammerspoon.org/docs/hs.alert.html
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

    reval-ec hammerspoon -c "hs.eventtap.keyStrokes($(gquote-dq "$input"))"
}
##
