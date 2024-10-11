# alias pc='pbcopy'
function cat-copy {
    local inargs
    in_or_args_newline_p=n in-or-args2 "$@"

    ec "$inargs" #: Yes, we are adding a newline here, to work around some functions which do not output their trailing newline.
    ecn "$inargs" | pbcopy
}
# alias pc='\noglob cat-copy'

function cat-copy-v2 {
    if (( $#@ > 0 )) ; then
        cat "$@"
    else
        in-or-args
    fi | pbcopy
}
alias cf='cat-copy-v2'

function cat-copy-streaming-v1 {
    local temp_file
    temp_file="$(mktemp)" @TRET
    {
        if (( $#@ == 0 )) ; then
            cat
        else
            # arrn "$@"
            ecn "$*"
        fi |
            tee "$temp_file" | cat @RET

        pbcopy < "$temp_file" @RET
    } always {
        silent trs-rm "$temp_file"
    }
}
function cat-copy-streaming {
    if (( $#@ >= 1 )) ; then
        # arrN "$@" |
        ecn "$*" |
            >&1 > >(pbcopy)
    else
        >&1 > >(pbcopy)
    fi
}
alias pc='\noglob cat-copy-streaming'

function cat-copy-as-file {
    local suffix="${1}"

    local tmp
    tmp="$(gmktemp --suffix="$suffix")" @TRET

    cat-copy > "$tmp" @RET

    reval-ec pbadd "$tmp"
}

function cat-rtl-if-tty {
    if isOutTty ; then
        rtl-reshaper-fast
    else
        cat
    fi
}

function cat-rtl-streaming-if-tty {
    if isOutTty ; then
        rtl-reshaper-streaming
    else
        cat
    fi
}

function cat-copy-rtl-if-tty {
    if isOutTty ; then
        cat-copy | rtl-reshaper-streaming
    else
        cat
    fi
}

function cat-copy-if-tty {
    if isOutTty ; then
        cat-copy
    else
        cat
    fi
}

function cat-paste-if-tty {
    ##
    # if isInTty ; then
    #     pbpaste
    # else
    #     cat
    # fi
    ##
    in-or-args "$@"
    ##
}

alias pop='pbpaste'
##
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

function pbcopy {
    ##
    # local in="$(in-or-args "$@")"
    local in="${$(in_or_args_newline_p= in-or-args "$@" ; print -n .)[1,-2]}"
    dact var-show in

    if isLinux ; then
        ##
        # =wl-copy= (Wayland)
        # =xclip -se c -i=
        ##
        # @NA
        return 0
    fi
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
            print -nr -- "$in" | LANG=en_US.UTF-8 command pbcopy
        }
    }
}

function pbpaste {
    # if isKitty ; then
    #     kitty +kitten clipboard --get-clipboard
    #     return $?
    # fi

    { false && (( $+commands[copyq] )) } && {
        copyq clipboard
    } || {
        (( $+commands[pbpaste] )) && command pbpaste
    } | cat-rtl-if-tty
}

function pbpaste-html {
    if isDarwin ; then
        command pbv public.html public.utf8-plain-text
        # https://stackoverflow.com/questions/17217450/how-to-get-html-data-out-of-of-the-os-x-pasteboard-clipboard
    else
        @NA
        # See
        # - https://unix.stackexchange.com/questions/78395/save-html-from-clipboard-as-markdown-text
    fi |
        cat-copy-if-tty
}
alias poph='pbpaste-html'

function pbpaste-html-urlfinal {
    local urli=''
    url="${1:-$(browser-current-url)}" @STRUE

    pbpaste-html |
        html-links-urlfinal "$url" |
        cat-copy-if-tty
}


function pbcopy-html {
    # @alt copy_as_html.swift can set the plain and HTML clipboard simultaneously.
    #
    # Can be pasted in Excel, TextEdit, but surprisingly not in much else.
    # Test with `<b>bold text</b>`.
    #
    # @useme Further tests seem to indicate that I was mistaken in my assumption that pasting rich text is possible in most web apps on Chrome. Gmail and Google Docs work though.
    #
    # - @me https://stackoverflow.com/questions/68937989/macos-copy-html-to-the-clipboard
    # - @me https://github.com/chbrown/macos-pasteboard/issues/8
    # - https://github.com/jkitchin/ox-clip/issues/13
    # - https://assortedarray.com/posts/copy-rich-text-cmd-mac/
    #
    # Less relevant:
    # - https://unix.stackexchange.com/a/84952
    # - https://superuser.com/questions/912712/how-to-send-rich-text-to-the-clipboard-from-command-line
    # - https://stackoverflow.com/questions/6095497/how-can-i-generate-a-rich-text-link-for-pbcopy/6100348#6100348
    # - https://stackoverflow.com/questions/67500279/copy-rich-text-with-image-to-nspasteboard-and-paste-in-word
    #
    ##
    local html="$(cat)"

    # html="<meta charset=\"utf-8\"> $html"
    ec "$html"

    if isDarwin ; then
        local hex
        hex="$(arrN "$html" | hexdump -ve '1/1 "%.2x"')" @TRET
        osascript -e "set the clipboard to {text:\" \", «class HTML»:«data HTML${hex}»}"
    else
        # See https://github.com/jkitchin/ox-clip for other platforms

        ec "$html" | xclip -t text/html
    fi
}

function pbcopy-rtf {
    local rtf
    rtf="$(cat)" @RET

    if isDarwin ; then
        ec "$rtf" |
            command pbcopy -Prefer rtf
        # The  input is  placed  in  the pasteboard as plain text data unless it begins with the Encapsulated PostScript (EPS) file header or the Rich Text Format  (RTF)  file  header,  in which case it is placed in the pasteboard as one of those data types.
        #
        # Using pandoc's RTF output doesn't seem to work (using `-s` with pandoc might fix this), but using html2rtf-textutil works.
        #
        # The output is still only usable where pbcopy-html works.
    else
        @NA
    fi
}


function pbpaste-urls {
    pbpaste-html |
        urls-extract |
        duplicates-clean |
        cat-copy-if-tty
}
alias popu='pbpaste-urls'
##
function pbadd-applescript() {
    # @deprecated
    ##

    osascript "$NIGHTDIR"'/applescript/path-copy.applescript' "${(f)$(re 'grealpath --' $@)}" > /dev/null
}

function pbadd {
    copy_files.swift "${(f@)$(re 'grealpath --' $@)}"
}
alias pa=pbadd

function pbcopy-img-darwin {
    #: Usage: pbcopy-img <path>
    ##
    # pbcopy "$@"
    #: It's useless, the clipboard gets overwritten anyhow.

    local i
    for i in $@ ; do
        i_real="$(realpath "$i")" || i_real="$i"
        clipboard-add "${i_real}"
    done
    ##
    #: =pbcopy_image.m= does not copy as text.
    pbcopy_image.m "$@"
}

function pbcopy-img {
    #: @seeAlso [help:ns-yank-image-at-point-as-image]
    ##
    if isDarwin ; then
        pbcopy-img-darwin "$@"
    else
        @NA
    fi
}

function pbpaste-plus() {
    # GLOBAL out: paste
    unset paste
    paste=( "$(pbpaste)" ) || {
        ectrace 'pbpaste failed'
        return 1
    }

    local ppaths
    if isDarwin ; then
        ppaths=( ${(@f)"$(paste_files.swift)"} )
        ##
        # if isArm ; then
        #     ecgray "$0: getting files from the clipboard are not yet supported on Apple ARM."
        # else
        #     ppaths=( "${(@f)$(clipboard-to-path.nu)}" )
        # fi
        ##
    fi

    test -n "$ppaths[*]" && paste=( $ppaths[@] ) || true
}
##
function clipboard-info-darwin {
  osascript -e "clipboard info" |
  sed -E 's/, /,/g; s/,([0-9]+)/:\1/g' | tr ':,' '\t\n'
}

function pngpaste {
    # See https://apple.stackexchange.com/a/375353/282215 for getting other types of stuff out of the clipboard
    local name="${1}" extension="${2:-png}" class="${3}"
    test -z "$class" && class='«class PNGf»'
    assert isDarwin @RET

    local stdout=''
    if [[ "$name" == '-' ]] ; then
        name="$(gmktemp --suffix ".${extension}")" @TRET
        stdout=y
    fi
    local dir
    dir="$(bottomdir "$name")" @TRET
    if test -z "$dir" ; then
        dir="$PWD"
    fi
    dir="$(grealpath -- "$dir")" @TRET
    mkdir -p "$dir" @TRET

    name="$(bottomfile "$name")"
    if test -z "${name}" ; then
        name+="$(dateshort | gtr ':' '_' | str2filename)" @TRET
    fi

    [[ "$name" =~ '\.'${extension}'$' ]] || name+=".${extension}"

    local f="${dir}/${name}"
    if test -e "$f" ; then
        silent trs-rm "$f" @TRET
    fi

    revaldbg osascript -e "tell application \"System Events\" to ¬
                  write (the clipboard as ${class}) to ¬
                          (make new file at folder \"${dir}\" with properties ¬
                                  {name:\"${name}\"})" @TRET
    if test -n "$stdout" ; then
        cat "$f" @TRET
        silent trs-rm "$f" @STRUE
    else
        if fn-isTop ; then
            ecgray "$0: pasted to $(gq "$f")"

            icat_v=n icat "$f" @STRUE
        fi
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

function jpgpaste {
    pngpaste "$1" jpg 'JPEG picture'
}
##
