
function cat-eol {
    : "copies stdin to stdout, adding a final newline only when one is missing"
    #: A chunk copier, not a line filter: nothing is split, so it is binary
    #: safe, and the writes are unbuffered, so it streams. Measured over two
    #: million lines (2026-09-11) it costs 2.45s against plain `cat''s 2.39s,
    #: where `gawk "{print; fflush()}"' costs 3.09s for the same guarantee and
    #: a bare `awk 1' does not stream at all.
    #:
    #: `binmode', not `perl -CS': the point is to move bytes, not characters.
    #: Encoding layers would have `sysread' hand back characters and mangle a
    #: multi-byte sequence split across a read, and they can arrive from the
    #: environment rather than the command line -- with PERL_UNICODE=SDA set,
    #: this emitted nothing at all before the binmode calls. Raw handles make
    #: UTF-8, a boundary-straddling character and arbitrary binary all pass
    #: through untouched; the newline test only ever looks at the last byte.
    ##
    command perl -e '
binmode(STDIN);
binmode(STDOUT);
my $last;
while ((my $n = sysread(STDIN, my $buf, 65536)) > 0) {
    syswrite(STDOUT, $buf);
    $last = substr($buf, -1);
}
syswrite(STDOUT, "\n") if defined($last) && $last ne "\n";
'
}

function h-cat-copy-fanout {
    : "stdin to both stdout and the clipboard in one pass; <1> filters stdout"
    #: `> >(pbcopy) | ...', not `>&1 > >(pbcopy)'. Both fan the stream out
    #: through zsh's MULTIOS, which forks a helper process to do the copying,
    #: but the second leaves the shell nothing in the foreground to wait on:
    #: the terminal copy can then land after the next prompt (four runs in
    #: five, with a second command following immediately), and in a shell that
    #: exits at once -- `zsh -ic ...' under a pty -- the clipboard write is
    #: lost outright (five runs in five). The pipe gives the shell a reader to
    #: wait on and both problems go away, for ~4ms a call and ~38% on bulk.
    #: `command tee >(pbcopy)' is correct too, and slower per call.
    ##
    > >(pbcopy) | "${1:-cat}"
}

function cat-copy-args {
    : "prints its arguments, else stdin, and copies them; byte-exact"
    #: The engine. Arguments are the text itself, newline separated, as
    #: [agfi:pbcopy] has always taken them; [agfi:cat-copy] is the variant
    #: whose arguments are paths, the way `cat' reads them.
    #:
    #: Through [agfi:in-or-args], so a call with neither arguments nor a pipe
    #: reads the clipboard rather than hanging on a terminal.
    ##
    in_or_args_newline_p=n in-or-args "$@" | h-cat-copy-fanout
}
alias pc='\noglob cat-copy-args'

function cat-copy-args-newline {
    : "cat-copy-args, but stdout is guaranteed to end with a newline"
    #: What a producer using `ecn' wants: the terminal gets its line ending so
    #: the output does not run into the prompt, while the clipboard stays
    #: byte-exact, so a value still pastes into a spreadsheet cell without
    #: spilling into the next one. See [agfi:cat-eol].
    ##
    in_or_args_newline_p=n in-or-args "$@" | h-cat-copy-fanout cat-eol
}

function cat-copy {
    : "prints the contents of its file arguments, else stdin, and copies them"
    #: `cat'-shaped, which is what the name promises: arguments are paths.
    #: [agfi:cat-copy-args] is the one whose arguments are the text.
    #:
    #: Files are printed as they are. A missing final newline is left alone,
    #: unlike [agfi:cat-copy-args-newline]: a file is not a value being
    #: prepared for a paste, and `cat' does not tidy one either.
    ##
    if (( $# )) ; then
        command cat "$@"
    else
        in-or-args
    fi | h-cat-copy-fanout
}
alias cf='cat-copy'

#: The old names.
function cat-copy-streaming {
    : "the old name for [agfi:cat-copy-args]"
    cat-copy-args "$@"
}

function tee-copy {
    : "the old name for [agfi:cat-copy-args]"
    cat-copy-args "$@"
}
aliasfn teec tee-copy

function cat-copy-v2 {
    : "the old name for [agfi:cat-copy]"
    cat-copy "$@"
}

function cat-copy-as-file {
    local suffix="${1}"

    local tmp
    tmp="$(gmktemp --suffix="$suffix")" @TRET

    #: Plain `cat': the clipboard is set from the file below, and copying the
    #: text here first only left it holding the wrong thing for a moment.
    command cat > "$tmp" @RET

    reval-ec pbadd "$tmp"
}
alias caf='cat-copy-as-file'

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

function cat-streaming-copy-rtl-if-tty {
    if isOutTty ; then
        cat-copy-args | rtl-reshaper-streaming
    else
        cat
    fi
}

function cat-copy-rtl-if-tty {
    if isOutTty ; then
        cat-copy-args-newline | rtl-reshaper-streaming
    else
        cat
    fi
}

function cat-copy-if-tty {
    if isOutTty ; then
        #: The newline variant, so a producer using `ecn' does not run into
        #: the prompt. Swap in [agfi:cat-copy-args] if a caller ever needs
        #: stdout as byte-exact as the clipboard already is.
        cat-copy-args-newline
    else
        cat
    fi
}

function cat-copy-streaming-remote {
        if isLocal ; then
            cat-copy-args
        else
            pbcopy-remote
        fi
}

function cat-copy-streaming-remote-if-tty {
    if isOutTty ; then
        cat-copy-streaming-remote
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
    # in-or-args "$@"
    ##
    if isInTty && (( ${#@} == 0 )) ; then
        pbpaste
    else
        cat "$@"
    fi
    ##
}

alias pop='pbpaste'
##
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
    } | cat-copy-rtl-if-tty
}

function pbpaste-html {
    if isDarwin ; then
        command pbv public.html public.utf8-plain-text
        # https://stackoverflow.com/questions/17217450/how-to-get-html-data-out-of-of-the-os-x-pasteboard-clipboard
    elif isLinux ; then
        #: @untested
        xclip -o -selection clipboard -t text/html
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
    #: [agfi:pbcopy-img}
    ##
    @darwinOnly

    assert copy_files.swift "${(f@)$(re 'grealpath --' $@)}" @RET

    ##
    if bool "${pbadd_image_preview_p:-y}" ; then
        icat-maybe "$@"
    fi
    ##
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
    #: @seeAlso
    #: - [help:ns-yank-image-at-point-as-image]
    #: - [agfi:pbadd]
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

    ##
    local paste
    paste="$(pbpaste)" @TRET

    dvar paste
    if test -e "${paste}" ; then
        #: File path in clipboard
        if [[ "${paste:e}" != png ]] ; then
            reval-ecgray magick convert -background transparent "${paste}" "$f" @TRET
        else
            reval-ecgray cp "${paste}" "$f"
        fi
    else
        revaldbg osascript -e "tell application \"System Events\" to ¬
                  write (the clipboard as ${class}) to ¬
                          (make new file at folder \"${dir}\" with properties ¬
                                  {name:\"${name}\"})" @TRET
    fi
    ##

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
