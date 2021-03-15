function wallpaper-set-darwin() {
    local f="$1" add_on="y"
    f="$(realpath "$f")" || return $?

    if test -n "$add_on" ; then
        local t="$(gmktemp --suffix .png)"
        # https://stackoverflow.com/questions/66629425/pillow-how-to-draw-text-with-the-inverse-color-of-the-underlying-image
        local font="$NIGHTDIR/resources/fonts/CourierNew_Symbola.ttf" # monospace
        iwidget-rem | @opts r 255 g 255 b 255 x 160 y 60 s 43 bold 1 font "$font" @ text2img "$t" "$f" && f="$t" || {
                ecerr "$0: Failed to overlay addons with $?"
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
    isNet || return 1

    pushf ~/Pictures/wallpapers/bing
    {
        local dest="$(uuidm).jpg"
        reval-ec aa "$(bing-wallpaper-get)" -o "$dest"
        reval-ec wallpaper-set "$(last-created)" || notif "$0: setting wallpaper returned $?"
    } always { popf }
}
##
