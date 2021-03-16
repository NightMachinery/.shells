function wallpaper-overlay() {
    local input="$1" overlay_rem="y" overlay_weather="${wallpaper_overlay_weather:-y}"
    local o="${2:-${1:r}_overlay.png}"
    ensure-args input @MRET
    local rem_x="${wallpaper_overlay_rx:-160}"
    local rem_y="${wallpaper_overlay_ry:-60}"
    local rem_s="${wallpaper_overlay_rs:-43}"
    local se_pos="${wallpaper_overlay_se_pos:-+200+70}"
    local weather_pos="${wallpaper_overlay_weather_pos:-+170+0}"
    local weather_s="${wallpaper_overlay_weather_s:-400}"

    local f
    f="$(realpath "$input")" || return $?

    (
        renice-me
        if [[ "$overlay_rem" == y ]] ; then
            local t="$(gmktemp --suffix .png)"
            # https://stackoverflow.com/questions/66629425/pillow-how-to-draw-text-with-the-inverse-color-of-the-underlying-image
            local font="$Font_CourierNew_Symbola" # monospace
            {
                # ec "$(datej-all)"$'\t'"$(weather-short)"
                # @opts key "$(date '+%Y/%m/%d')" @ memoi-eval rem-summary
                # weather-short
                datej_all_mode=1 iwidget-rem
            } | @opts r 255 g 255 b 255 x $rem_x y $rem_y s $rem_s bold 1 font "$font" @ text2img "$t" "$f" && f="$t" || {
                    ecerr "$0: Failed to overlay addons with $?"
                }
        fi
        if [[ "$overlay_weather" == (y|ipad) ]] ; then
            t="$(gmktemp --suffix .png)"
            local loc
            loc="$(serr location-get | prefixer -o , --skip-empty)" || loc='Sabzevar,Iran'
            # https://wttr.in/:help
            gurl "wttr.in/${loc}_transparency=255_mQ0_lang=en.png" > "$t" && {
                #  -channel RGB -negate
                # dark: plus > negate > screen; overlay, lighten, diff, add are very bad
                convert \( "$f" \( -background none -font "$font" -pointsize 30 -fill 'rgba(0,255,0,255)' label:"$(crypto-prices)" \) -gravity southeast -geometry $se_pos -compose plus -composite \) \( "$t" -channel RGB -resize "${weather_s}x" \) -gravity east -geometry $weather_pos -compose plus -composite "$t" && f="$t"
                # box the text: `-bordercolor 'rgba(0,0,255,100)' -border 1`
                # bolden the text: `-stroke 'rgba(0,255,0,255)' -strokewidth 2`
            }
        fi

        rsync "$f" "$o" # ignores if same
    )
}
function wallpaper-overlay-ipad() {
    local f="$1" f_ipad="${2:-$HOME/Downloads/private/ipad.png}"
    f="$(realpath "$f")" || {
        ecerr "$0: Does the input file exist?"
        return 1
    }

    ensure resize4ipad-fill "$f" $f_ipad @MRET
    # if resize4ipad "$f" $f_ipad ; then
    if @opts rx 300 ry 330 rs 43 se_pos '+300+360' weather ipad weather_s 700 weather_pos '+300-0' @ wallpaper-overlay "$f_ipad" "$f_ipad" ; then
        isLocal && { scpeva "$f_ipad" Downloads/private/"${f_ipad:t}" @RET }
        true
    else
        ecerr "$0: resize4ipad exited $?"
        return 1
    fi
}
##
function wallpaper-set-darwin() {
    ensure isDarwin @MRET
    local f="$1"
    f="$(realpath "$f")" || return $?
    ###
    # osascript -e 'tell application "Finder" to set desktop picture to POSIX file "'$f'"' || return $?
    ##
    mcli wallpaper "$f" || return $?
    # Try `killall Dock` if this didn't work.
    ###
}
function wallpaper-set() {
    local f="$1"
    f="$(realpath "$f")" || return $?
    ##
    local ipad=''
    if test -n "$ipad" ; then
        wallpaper-overlay-ipad "$f"
        # errors ignored
    fi
    ##
    local t="$(gmktemp --suffix .png)"
    wallpaper-overlay "$f" "$t" && f="$t"

    wallpaper-set-darwin "$f"
}
##
function wallpaper-auto() {
    wallpaper-auto-bing
}
function wallpaper-auto-bing() {
    ensure-net @MRET

    pushf ~/Pictures/wallpapers/bing
    {
        local dest="$(uuidm).jpg"
        reval-ec aa "$(bing-wallpaper-get)" -o "$dest"
        reval-ec wallpaper-set "$(last-modified)" || notif "$0: setting wallpaper returned $?"
    } always { popf }
}
function wallpaper-auto-ipad() {
    # takes ~9 minutes on eva
    fnswap wallpaper-set wallpaper-overlay-ipad wallpaper-auto
}
##
