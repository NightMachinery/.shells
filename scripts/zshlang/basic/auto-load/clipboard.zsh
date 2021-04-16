function tee-copy() {
    doc "teec; ec-and-copy; tee-copy;
Prints and copies its stdin.
See also: 'etee'."

    > >(pbcopy) | cat
}
aliasfn teec tee-copy
function reval-copy() {
    doc 'revals and also copies the stdout to the clipboard.'

    reval "$@" > >(pbcopy) | cat
}
##
function ec-copy() {
    local i="$*"

    ## Old API
    # reval-copy ec "$i" # copies with a newline at the end
    ##
    pbcopy "$i"
    ec "$i"
}
##
function pbcopy-term() {
    # @alt: it2copy
    local in="${$(in-or-args "$@" ; print -n .)[1,-2]}"

    # OSC 52, supported by kitty, iTerm, and others
    printf "\033]52;c;$(printf "%s" "$in" | base64)\a"
}
function pbcopy() {
    # local in="$(in-or-args "$@")"
    local in="${$(in-or-args "$@" ; print -n .)[1,-2]}"
    dact typ in

    ## buggy
    # if isKitty ; then
    #     ecn "$in" | kitty +kitten clipboard
    #     return $?
    # fi
    ##

    { false && (( $+commands[copyq] )) } && {
        silent copyq copy -- "$in"
    } || {
        (( $+commands[pbcopy] )) && {
            print -nr -- "$in" | command pbcopy
        }
    }
}
function pbpaste() {
    if isKitty ; then
        kitty +kitten clipboard --get-clipboard
        return $?
    fi

    { false && (( $+commands[copyq] )) } && {
        copyq clipboard
    } || {
        (( $+commands[pbpaste] )) && command pbpaste
    }
}
function pbadd() {
    osascript "$NIGHTDIR"'/applescript/path-copy.applescript' "${(f)$(re realpath $@)}" > /dev/null
}
function pbpaste-plus() {
    # GLOBAL out: paste
    unset paste
    paste="$(pbpaste)" || {
        ectrace 'pbpaste failed'
        return 1
    }
    local ppaths=( "${(@f)$(clipboard-to-path.nu)}" )
    test -n "$ppaths[*]" && paste=( $ppaths[@] )
    true
}
##
clipboard-info-darwin() {
  osascript -e "clipboard info" |
  sed -E 's/, /,/g; s/,([0-9]+)/:\1/g' | tr ':,' '\t\n'
}
function pngpaste() {
    # See https://apple.stackexchange.com/a/375353/282215 for getting other types of stuff out of the clipboard
    local name="${1}" extension="${2:-png}" class="${3}"
    test -z "$class" && class='«class PNGf»'
    # ensure-args name @MRET
    ensure isDarwin @MRET

    local stdout=''
    if [[ "$name" == '-' ]] ; then
        name="$(gmktemp --suffix ".${extension}")" @RET
        stdout=y
    fi
    local dir
    dir="$(bottomdir "$name")"
    if test -z "$dir" ; then
        dir="$PWD"
    fi
    dir="$(grealpath "$dir")"
    mkdir -p "$dir" @RET

    name="$(bottomfile "$name")"
    if test -z "${name}" ; then
        name+="$(dateshort | gtr ':' '_' | str2filename)" @RET
    fi

    [[ "$name" =~ '\.'${extension}'$' ]] || name+=".${extension}"

    local f="${dir}/${name}"
    if test -e "$f" ; then
        command rm "$f"
    fi

    revaldbg osascript -e "tell application \"System Events\" to ¬
                  write (the clipboard as ${class}) to ¬
                          (make new file at folder \"${dir}\" with properties ¬
                                  {name:\"${name}\"})" @RET
    if test -n "$stdout" ; then
        cat "$f"
        command rm "$f"
    fi
    ## @alt:
    # https://github.com/jcsalterego/pngpaste/issues/16
    # https://apple.stackexchange.com/questions/418043/macos-saving-images-from-the-clipboard-using-pngpaste-is-faded-and-white
    # pngpaste images (screenshots? pasting from Telegram works fine) look faded and white. Using `montage` first fixes this problem, so use `ils` instead.
    # Use `magick convert png:a1.png -resize 1700x png:- | icat-realsize` to test it with `pngpaste a1.png`
    # things that did not work:
    # - Might be related to `-colorspace`, but I could not fix it.
    # - -define png:color-type=6
    # - png32:-
    ##
}
function jpgpaste() {
    pngpaste "$1" jpg 'JPEG picture'
}
##
