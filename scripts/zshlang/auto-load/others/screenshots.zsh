##
function screenshot-all {
    if ! isDarwin ; then
        @NA
    fi

    local s="${screenshot_all_s:-5}" file="${1}" silent="${screenshot_all_silent}"

    local opts=(-T0)
    #: -T<seconds> take the picture after a delay of <seconds>

    bool "$silent" && opts+='-x'

    sleep $s
    if test -n "$file" ; then
        revaldbg screencapture "${opts[@]}" -- "$file"
    else
        #: to clipboard
        revaldbg screencapture -c "${opts[@]}" --
    fi
}
##
function touchbar-screenshot {
    if ! isDarwin ; then
        @NA
    fi

    local img="$(gmktemp --suffix png)"
    screencapture -b "$img" || return 1 # -x for no sound
    local imgp="$(gmktemp --suffix png)"
    convert-pad "$img" "$imgp" || return 2
    pbadd "$imgp"
    bello
}
##
