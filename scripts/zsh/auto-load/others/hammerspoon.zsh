BTT_HS_NOISES_UID='5DBF0BE7-5822-459A-B450-36E3396124F9'
##
function hs-popclickPlayPause() {
    local lis="$(hammerspoon -c 'popclickPlayPause()')"
    hs-popclick-btt-refresh
    @opts say y @ hs-popclick2icon $lis

}
function hs-popclick-btt-refresh() {
    btt-refresh "$BTT_HS_NOISES_UID"
}
function hs-popclickListening() {
    local lis="$(hammerspoon -c 'popclickListening')"
    hs-popclick2icon $lis
}
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
}
