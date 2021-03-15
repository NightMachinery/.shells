function screenshot-all() {
    # @darwinonly
    local s="${screenshot_all_s:-5}" file="${1}" silent="${screenshot_all_silent}"

    local opts=(-T0) # -T<seconds> take the picture after a delay of <seconds>, default is 5
    test -n "$silent" && opts+='-x'

    sleep $s
    if test -n "$file" ; then
        screencapture "$file"
    else
        # to clipboard
        screencapture -c
    fi
}
##
function touchbar-screenshot() {
    local img="$(gmktemp --suffix png)"
    screencapture -b "$img" || return 1 # -x for no sound
    local imgp="$(gmktemp --suffix png)"
    convert-pad "$img" "$imgp" || return 2
    pbadd "$imgp"
    bello
}
##
