###
## None of these work for me under tmux. icat-it and icat-py might work if the image's original data is sufficiently small. (I could never get them to work.)
aliasfn icat-it ~/.iterm2/imgcat
aliasfn ils-it ~/.iterm2/imgls
## https://github.com/olivere/iterm2-imagetools
function icat-go() {
    : "@warn will uglify images if it has to shrink them"
    isI || return 0
    local h="${icat_go_height:-${icat_go_h:-${icat_h:-1300}}}"

    local opts=()
    [[ "$h" == x ]] || opts+=(-height ${h}px) # This will zoom, too, but that's actually good in most cases!
    "$GOBIN/imgcat" "$opts[@]" "$@"
}
function ils-imgls() {
    if (( $#@ == 0 )) ; then
        set -- ${~imageglob}
    fi
    (( $#@ == 0 )) && return 0

    isI || { exa -a "$@" ; return 0 }
    imgls -height 200px "$@"
}
@opts-setprefix ils-imgls icat
function ils-montage() {
    if (( $#@ == 0 )) ; then
        set -- ${~imageglob}
    fi
    (( $#@ == 0 )) && return 0

    local opts=()
    local label='%f'
    if (( $#@ > 1 )) ; then
        opts+=( -label "$label" )
    fi
    local tmp="$(gmktemp --suffix=.png)"

    #  '1x1<' tells IM to only resize smaller images to the given size. As no image can be smaller that 1 pixel, no image will be resized. The tile size will thus be again the largest dimention of all the images on the page.
    magick montage "$opts[@]" -geometry '1x1<+0+0' "$@" png:- >$tmp | icat_margin='' icat-autoresize
    pbadd "$tmp"
}
@opts-setprefix ils-montage icat
function ils() {
    if (( $#@ == 0 )) ; then
        set -- ${~imageglob}
    fi
    (( $#@ == 0 )) && return 0

    if (( $#@ <= 6 )) ; then
        ils-montage "$@"
    else
        ils-imgls "$@"
    fi
}
@opts-setprefix ils icat
## https://github.com/wookayin/python-imgcat
function icat-py() {
    isI || return 0
    python -m imgcat --height "${icat_h:-30}" "$@" # This will zoom, too, but that's actually good in most cases!
}
##
function h-icat-kitty {
    kitty +kitten icat "$@" < /dev/tty
    #: `echo | h-icat-kitty` prints lots of errors when we omit `/dev/tty`.
}

function icat-kitty-realsize {
    h-icat-kitty "$@"
}

function icat-kitty {
    h-icat-kitty --scale-up "$@"
}

aliasfn islideshow-kitty2 icat-kitty --hold # press a key to go to next image

function icat-kitty-single {
    icat-kitty --clear
    icat-kitty --place "${COLUMNS}x${LINES}@0x0" --scale-up "$@"
    # @see kitty-launch-icat
}

aliasfn icat-kitty2 pixcat fit-screen --enlarge --vertical-margin 60 # accepts dirs, too
aliasfn islideshow-kitty2 icat-kitty2 --hang # press ENTER to go to next image
##
function icat-kitty-fit-width {
  # Get terminal width
  local terminal_width=$(tput cols)

  # Use kitty +kitten icat to display image
  for img in "$@"; do
    # Use ImageMagick to identify the image dimensions
    local img_dimensions=$(magick identify -format "%wx%h" "$img")
    local img_width="$(echo $img_dimensions | cut -d 'x' -f 1)"
    local img_height="$(echo $img_dimensions | cut -d 'x' -f 2)"

    # Calculate scale factor and new height
    local scale_factor="$(echo "$terminal_width / $img_width" | command bc -l)"
    local new_height="$(echo "$img_height * $scale_factor" | command bc)"
    new_height="${new_height%.*}"  # Remove decimal part

    # Display the image
    kitty +kitten icat --place=${terminal_width}x${new_height}@0x0 --scale-up "$img"

    redo2 "$((new_height / 2))" ec
  done
}
##
function icat-realsize() {
    isI || return 0
    if isKitty ; then
        icat-kitty "$@"
    else
        @opts h x @ icat-go "$@"
    fi
}
function icat-autoresize() {
    local margin="${icat_margin:-${icat_m}}"
    (( $#@ == 0 )) && set -- -

    integer margin_height=270
    local sw="$(screen-width)"
    local sh="$(screen-height)"
    local i
    for i in $@ ; do
        local m="$margin"
        if [[ "$i" == - ]] ; then
            i="$(gmktemp --suffix=.png)"
            cat > "$i"
            # needed for gettings dimensions
        fi
        local w_r=$(( sw - 50 )) # width_real
        if test -z "$m" ; then
            ecdbg "$0: automatically determining size ..."
            # needs zsh/mathfunc
            local w="$(img-width "$i")"
            local h="$(img-height "$i")"
            local r=$(( float(w)/h ))
            # re dvar w h r
            local max_w=$(( int((sh - margin_height) * r) ))
            if (( w_r > max_w )) ; then
                # dvar max_w
                w_r=$max_w
            fi
        else
            w_r=$(( sw - m ))
        fi
        ##
        revaldbg magick convert "${${i:e}:-png}":"$i" -resize "$w_r"x png:- | icat-realsize
        ##
    done
}
@opts-setprefix icat-autoresize icat

function icat {
    isI || return 0

    if (( $#@ == 0 )) ; then
        if isInTty ; then
            local images=( ${~imageglob} *.pdf(Nn) )
            if (( ${#images} )) ; then
                icat "$images[@]"
                return $?
            else
                return 0
            fi
        else
            local tmp
            tmp="$(gmktemp)" @TRET
            {
                cat > $tmp @TRET
                icat $tmp </dev/tty # icat-kitty tries to read the stdin if it is not a tty

                gsleep 0.05 #: to allow us to cancel the loop
            } always {
                silent trs-rm "$tmp"
            }
        fi
    fi

    re h_icat "$@"
}

function h_icat {
    local i="$1"

    if bool "${icat_v}" ; then
        ecgray "${i:t}"
    fi

    if [[ "$i" == *.pdf ]] ; then
        icat-pdf "$i"
        return $?
    elif [[ "$i" == *.avif ]] ; then
        # @toFuture/1402 Doesn't Kitty natively support AVIF now?
        ##
        ecgray "$0: converting $(gquote-sq "$i") to PNG ..."

        local tmp
        tmp="$(gmktemp --suffix='.png')" @TRET
        {
            avifdec "$i" "$tmp" @TRET
            h_icat "$tmp"
            return $?
        } always { silent trs-rm "$tmp" }
    fi

    if isKitty ; then
        icat-kitty "$i"
        # icat-kitty2 "$i"

        return $?
    fi

    if test -z "$TMUX" ; then
        icat-autoresize "$i"
    else
        icat-py "$i"
        ecerr "$0: Tmux not supported."
        # @todo use ANSI https://github.com/trashhalo/imgcat
    fi
}
alias ic=icat

aliasfn icat-big icat-kitty
alias icb=icat-big
##
# function icat-labeled() {
#     local i
#     for i in "$@" ; do
#         ecgray $i
#         icat $i
#     done
# }
###
function iterm-boot() {
    # Alt: `lnrp iterm_focus.py ~/Library/Application\ Support/iTerm2/Scripts/AutoLaunch/` uses iterm's own python which doesn't have our Brish
    tmuxnewsh2 iterm_focus reval-notifexit iterm_focus.py
}
##
it2prof() { echo -e "\033]50;SetProfile=$1\a" ; } # Change iterm2 profile. Usage it2prof ProfileName (case sensitive)
##
function iterm-session-active() {
    : "Needs iterm_focus.py running. This gives you the last active session. Use iterm-focus-is to see if iTerm is actually in focus or not."
    # : "Doesn't work with tmux. iTerm's native tmux integration is also very invasive and not suitable for us. Setting ITERM_SESSION_ID manually by ourselves can solve this problem somewhat. It would work fine for ivy, but detaching and reattaching is hard to get right for it if at all."
    # Alt for linux: See focus events http://invisible-island.net/xterm/ctlseqs/ctlseqs.html:
    # These need iTerm to immediately send the focus events when enabled, not waiting for a focus change. I tested it with `unset hi ; echo -ne '\e[?1004h\e[?1004l'; read -r -k 3 -t 0.5 hi ; typeset -p hi`; `hi` remained unset.
    # Alt for linux: xdotool https://unix.stackexchange.com/questions/480052/how-do-i-detect-whether-my-terminal-has-focus-in-the-gui-from-a-shell-script

    redism get iterm_active_session
}

function iterm-session-my() {
    if [[ "$ITERM_SESSION_ID" =~ '[^:]*:(.*)' ]] ; then
        ec "$match[1]"
    else
        return 1
    fi
}

function iterm-session-is-active() {
    [[ "$(iterm-session-active)" == "$(iterm-session-my)" ]]
}

function iterm-focus-get() {
    redism get iterm_focus
}

function iterm-focus-is() {
    [[ "$(iterm-focus-get)" == TERMINAL_WINDOW_BECAME_KEY ]]
}
##
function iterm-tab-activate() {

    local i="${1:-5}"
    i=$(( i - 1 )) @RET # make it zero-based
    ec "tab_activate ${i}" | socat - unix-connect:"$iterm_socket"
    ##
    # cliclick kd:cmd kp:num-${i} ku:cmd # Took ~0.5
    ##
}
##
