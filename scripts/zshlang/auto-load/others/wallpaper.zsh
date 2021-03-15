function wallpaper-set-darwin() {
    local f="$1" overlay_rem="y" overlay_weather=y
    f="$(realpath "$f")" || return $?

    if test -n "$overlay_rem" ; then
        local t="$(gmktemp --suffix .png)"
        # https://stackoverflow.com/questions/66629425/pillow-how-to-draw-text-with-the-inverse-color-of-the-underlying-image
        local font="$Font_CourierNew_Symbola" # monospace
        {
            # ec "$(datej-all)"$'\t'"$(weather-short)"
            # @opts key "$(date '+%Y/%m/%d')" @ memoi-eval rem-summary
            # weather-short
            datej_all_mode=1 iwidget-rem
        } | @opts r 255 g 255 b 255 x 160 y 60 s 43 bold 1 font "$font" @ text2img "$t" "$f" && f="$t" || {
                ecerr "$0: Failed to overlay addons with $?"
            }
    fi
    if test -n "$overlay_weather" ; then
        t="$(gmktemp --suffix .png)"
        local loc='Sabzevar,Iran'
        loc="$(location-get | prefixer -o , --skip-empty)"
        # https://wttr.in/:help
        gurl "wttr.in/${loc}_transparency=255_mQ0_lang=en.png" > "$t" && {
            #  -channel RGB -negate
            # dark: plus > negate > screen; overlay, lighten, diff, add are very bad
            convert \( "$f" \( -background none -font "$font" -pointsize 30 -fill 'rgba(0,255,0,255)' label:"$(crypto-prices)" \) -gravity southeast -geometry +200+70 -compose plus -composite \) \( "$t" -channel RGB -resize 400x \) -gravity east -geometry +170+0 -compose plus -composite "$t" && f="$t"
            # box the text: `-bordercolor 'rgba(0,0,255,100)' -border 1`
            # bolden the text: `-stroke 'rgba(0,255,0,255)' -strokewidth 2`
        }
    fi

    # osascript -e 'tell application "Finder" to set desktop picture to POSIX file "'$f'"' || return $?

    mcli wallpaper "$f" || return $?

    # Try `killall Dock` if this didn't work.
}
function wallpaper-set() {
    # @darwinonly
    wallpaper-set-darwin "$@"
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
        reval-ec wallpaper-set "$(last-created)" || notif "$0: setting wallpaper returned $?"
    } always { popf }
}
##
