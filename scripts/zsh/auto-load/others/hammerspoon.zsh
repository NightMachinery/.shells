BTT_HS_NOISES_UID='5DBF0BE7-5822-459A-B450-36E3396124F9'
##
function hs-popclickPlayPause() {
    local lis="$(hammerspoon -c 'popclickPlayPause()')"
    hs-popclick2icon $lis
    hs-popclick-btt-refresh
}
function hs-popclick-btt-refresh() {
    btt-refresh "$BTT_HS_NOISES_UID"
}
function hs-popclickListening() {
    local lis="$(hammerspoon -c 'popclickListening')"
    hs-popclick2icon $lis
}
function hs-popclick2icon() {
    local lis="${1}"

    if [[ "$lis" == true ]] ; then
        ec "🎆"
    elif [[ "$lis" == false ]] ; then
        ec "🌌"
    else
        ec "🥶"
    fi
}
