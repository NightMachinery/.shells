alias psd2png=cpsdi
###
function cpsd() {
    local i;
    local counter=1;
    local outs=();
    for i in "${@:2}"; do
        local B=$(basename "$i"); #local D=$(dirname "$i");
        local out="$1/${B%.*}.png"
        convert "$i""[0]" "$out"
        outs[$counter]="$out"
        printf '%s\n' "$out"
        counter=$(($counter + 1))
    done
    pbadd "${(@)outs}"
}
function cpsdi() {
    cpsd "$(dirname "$1")" "$@"
}
function cpsdt() {
    mkdir -p ~/tmp/delme
    cpsd ~/tmp/delme "$@"
}
function psd2telg() {
    tsend ${me:-me} '' -f "$(cpsdt "$@")" --force-document
}
##
function rm-alpha() {
    local B=$(basename "$1"); local D=$(dirname "$1");
    convert "$1" -background "$2" -alpha remove "$D/${B%.*}_$2.png"
}
function alpha2black() { rm-alpha "$1" black }
function alpha2white() { rm-alpha "$1" white }
combine-funcs alpha2bw alpha2black alpha2white

function img-color2color {
    local input="$1" from="${2:-black}" to="${3:-white}"
    local output="${4:-${input:r}_${from}2${to}.${input:e}}"
    local fuzz="${img_color2color_fuzz:-20}"

    reval-ec magick "$input" -fuzz "${fuzz}%" -fill "$to" -opaque "${from}" "${output}"
}

function img-black2white {
    local input="$1" rest=("${@[2,-1]}")

    img-color2color "$input" black white "${rest[@]}"
}
@opts-setprefix img-black2white img_color2color
##
function pbpaste-image {
    #: image-paste, imgpaste; used from night-org.el
    ##
    local dest="$1" ; test -z "$dest" && {
        ecerr "$0: empty dest."
        return 1
    }
    local format="${dest:e:l}"
    pbpaste-plus @RET

    local f="${paste[1]}" #: We do not support multipastes at this point
    if test -e "$f" ; then
        cp "$f" "$dest" @RET
    elif url-match "$f" ; then
        fhMode=curl full-html "$f" "$dest" @RET
        #: aria2 is weaker than curl, and these simple downloads don't benefit from aria2's strengths
    else
        if [[ "${format}" =~ '^jpe?g$' ]] ; then
            jpgpaste "${dest}" @RET
        else
            pngpaste "$dest" @RET
        fi
    fi
}
alias saveas-img='pbpaste-image'
##
function text2img-v1 {
    #: Doesn't work with Persian at all
    : " < text text2img-v1 <img-name>"
    local img="$1"

    test -z "$img" && { ecerr "$0: Empty image destination. Aborting." ; return 1 }

    ##
    #: @duplicateCode/f44064fbab76db98c38d3e18d8c66685
    local font="${text2img_font}"
    if test -z "$font" ; then
        # local font="$NIGHTDIR/resources/fonts/unifont-13.0.06.ttf" # monospace font by GNU that supports most languages, but quite ugly and unreadable.
        local font="Courier New" # See https://github.com/IranOpenFontGroup/Discussions/issues/7 for more Persian monospace fonts
        test -e "$Font_Symbola_CourierNew" && font="$Font_Symbola_CourierNew"
    fi
    ##
    # font='FreeMono'

    # `magick convert -list font` for available fonts, or use absolute paths
    magick convert -page  4000x4000 -font "$font" -pointsize 20 -fill black -background white -trim +repage -bordercolor white  -border 15 text:- png:"${img:r}".png
    ## persian (doesn't work well)
    # ec سلام monster |     convert -page  700x700 -font '/Users/evar/Library/Fonts/B Nazanin Bold_0.ttf' -pointsize 20 -fill black -background white -trim +repage -bordercolor white  -border 15 text:- png:hi.png
    # Use https://stackoverflow.com/questions/23536375/linux-cli-how-to-render-arabic-text-into-bitmap instead
    ##
}
function text2img {
    #: @usage:
    #: `echo Hello World | text2img out.png`
    #:
    # @alt https://xg4.github.io/text2image/ does support Persian seemingly, but not completely?
    # @alt @good https://vincent7128.github.io/text-image/ seems to work as well as text2img.py with Persian, but it can also size the output image automatically.

    ##
    #: @duplicateCode/f44064fbab76db98c38d3e18d8c66685
    local font="${text2img_font}"
    if test -z "$font" ; then
        # local font="$NIGHTDIR/resources/fonts/unifont-13.0.06.ttf" # monospace font by GNU that supports most languages, but quite ugly and unreadable.
        local font="Courier New" # See https://github.com/IranOpenFontGroup/Discussions/issues/7 for more Persian monospace fonts
        test -e "$Font_Symbola_CourierNew" && font="$Font_Symbola_CourierNew"
    fi
    ##

    text2img.py $font "$@"
}

function text-show {
    local tmp="$(gmktemp --suffix .png)"

    text2img $tmp
    icat-realsize $tmp
    rm $tmp
}
@opts-setprefix text-show text2img

function reval-ts {
    reval "$@" | text-show
}
@opts-setprefix reval-ts text2img
##
function 2ico {
    local i="${1}" o="${2:-${1:r}.ico}" s="${png2ico_size:-256}"

    convert -background transparent "$i" -define icon:auto-resize=16,24,32,48,64,72,96,128,256 "$o"
    # convert -resize x${s} -gravity center -crop ${s}x${s}+0+0 "$i" -flatten -colors 256 -background transparent "$o"
}
aliasfn png2ico 2ico
##
function convert-pad {
    jglob
    local i="${1:? Input required}" o="${2:-${1:r}_padded.png}" w="${convert_pad_w:-${convert_pad_s:-1024}}" h="${convert_pad_h:-${convert_pad_s:-1024}}"

    local actualw actualh
    actualw="$(image-width "$i")" || return $?
    actualh="$(image-height "$i")" || return $?

    if (( w < actualw )) ; then
        w=$actualw
    fi
    if (( h < actualh )) ; then
        h=$actualh
    fi
    
    convert "$i" -gravity center -extent ${w}x${h} "$o" || return $?
    if isBorg ; then
        trs "$i"
    fi
}
aliasfn img-fix-telegram convert-pad
##
function jiconpack {
    jej

    unzip2dir $j
    mv **/*.png .
    re convert-pad *.png
}
##
# function img-dimensions {
#     magick identify -format 'width=%wpx;height=%hpx;' "$1" # 2>/dev/null
# }
function img-width {
    magick identify -format '%w' "$1"
}
reify img-width
aliasfn image-width img-width

function img-height {
    magick identify -format '%h' "$1"
}
reify img-height
aliasfn image-height img-height
##
function pad2square {
    # @alt convert-pad
    local input="$1"
    local o="${2:-${1:r}_padded.png}"
    ensure-args input @MRET
    [[ "$o" == *.png ]] || o+='.png'

    convert "$input" \( +clone -rotate 90 +clone -mosaic +level-colors gray -transparent gray \) +swap -gravity center -composite "$o"
}
##
function resize4ipad-fill {
    local input="$1"
    local o="${2:-${1:r}_ipad.png}"
    ensure-args input @MRET
    [[ "$o" == *.png ]] || o+='.png'

    local w=2048 h=2048
    local size="${w}x${h}"
    convert "$input" -resize "${size}^" -gravity center -extent "$size" "$o"
}
function resize4ipad-pad {
    local input="$1"
    local o="${2:-${1:r}_ipad.png}"
    ensure-args input @MRET
    [[ "$o" == *.png ]] || o+='.png'

    convert "$input" -resize 2048x $o
    pad2square "$o" "$o"
}
##
function img-rotate {
    ## usage tips
    # - select the files that need to be rotated in Finder
    # - =pf re 'o d -90 @ img-rotate'=
    ##
    local input="$1" degree="${img_rotate_d:--90}"
    assert test -e "$input" @RET
    local out="${2:-${input:r}_r${degree}.${input:e}}"
    dest-overwrite-p "$out" @RET

    reval-ec magick convert "$input" -rotate "$degree" "$out" @TRET

    if isI && @opts p [ img-rotate ] @ fn-isTop ; then
        icat "$out" || true
    fi
}

aliasfn img-rotate-inplace inplace-io img-rotate
@opts-setprefix img-rotate-inplace img_rotate
##
function img-compress {
    : "@alt @PNG https://pngquant.org/"
    : "impo options: -strip -quality 70"

    local i="$1"
    local o="${2:-${i:r}_compressed.jpg}"
    local colorspace="${img_compress_cs:-sRGB}" # 'Gray'
    local quality="${img_compress_q:-70}" # seems to be from 100
    local max_size="${img_compress_s}" # max size, use suffixes to change the unit
    ensure-array img_compress_opts
    local opts=("${img_compress_opts[@]}")
    local resize="${img_compress_r}"

    if test -n "$max_size" ; then
        [[ "$max_size" == *b ]] || max_size+='kb'
        opts+=(-define jpeg:extent="$max_size")
        # this does not resize the image, so it's limited in what it can do
    else
        opts+=(-quality "$quality")
    fi

    if test -n "$resize" ; then
        [[ "$resize" == *'%' ]] || resize+='%'
        opts+=(-adaptive-resize "${resize}")
    fi


    opts+=(-interlace JPEG) # @idk what use these are

    icat "$i"
    magick identify "$i"
    f-size-labeled "$i"

    reval-ec magick-convert -strip -define jpeg:dct-method=float -sampling-factor 4:2:0 -colorspace "$colorspace" "$opts[@]" "$i" "$o" @TRET
    # sampling-factor 4:2:0: What this does is reduce the chroma channel's resolution to half, without messing with the luminance resolution that your eyes latch onto.

    icat "$o"
    magick identify "$o"
    f-size-labeled "$o"
}
##
function img-background2transparent {
    local i="$1"
    local o="${2:-${i:r}_transparent.png}"
    local fuzz_percent="${img_bg_rm_f:-5}"
    assert-args i @RET

    local x=1 y=1

    magicwand.bash "${x},${y}" -t "$fuzz_percent" -f image -r outside -m overlay -o 0 "$i" "$o"
}
##
function img-to-data-uri {
    local f="$1"
    assert-args f @RET

    local mimetype content
    mimetype="$(file -bN --mime-type "$f")" @TRET

    content="$(gbase64 --wrap=0 < "$f")" @TRET
    #: no line wrapping

    ec "data:${mimetype};base64,${content}" |
        cat-copy-if-tty
    if isOutTty ; then
        icat "$f" || true
    fi
}
##
function img-split-vertical-dumb-v1 {
    #: @alt [[NIGHTDIR:python/img_vertical_split.py]]
    #: @deprecated
    ##
    local input="$1"
    local max_height="${2:-400}"
    local output="${3:-${input:r}_.${input:e}}"
    assert-args input @RET

    local width height
    width="$(identify -format "%w" "$input")" @TRET
    height="$(identify -format "%h" "$input")" @TRET

    #: Calculate the number of chunks needed (returns integer)
    local num_chunks
    num_chunks=$(( (height + max_height - 1) / max_height ))

    #: Split the image into chunks
    local y_offset chunk_output
    for ((i = 0; i < num_chunks; i++)); do
        y_offset=$(( i * max_height ))
        chunk_output="${output:r}${i}.${output:e}"
        assert revaldbg magick convert "$input" -crop "${width}x${max_height}+0+${y_offset}" +repage "$chunk_output" @RET
        #: @GPT4O `+repage`: This resets the virtual canvas information of the image. After cropping an image, the virtual canvas size and offset might still retain original image settings. Using `+repage` ensures that the output image's dimensions match the cropped dimensions without any offsets.

        ec "${chunk_output}"
    done

    ecgray "$0: Image split into $num_chunks chunks."
}

# function img-split-vertical-and-copy {
#     local input="${1:a}" #: normalizes path
#     local max_height="${2:-400}"
#     assert-args input @RET

#     local tmp_dir
#     tmp_dir="$(gmktemp -d)" @RET
#     assert pushf "$tmp_dir" @RET
#     {
#         img-split-vertical "$input" "$max_height" "$tmp_dir/$(basename "$input")"
#     } always {
#         popf
#         # trs "$tmp_dir"
#     }
# }
##
function qview {
    @darwinOnly

    local inargs
    in-or-args3 "$@" @RET

    if (( ${#inargs[@]} == 0 )) ; then
        return 0
    fi

    #: Use argv mode for bulk opens.
    command open -na "qView" --args "${inargs[@]}"
}
##
function finder-img-gallery {
    local finder_img_gallery_open_p="${finder_img_gallery_open_p:-y}"

    local inargs
    in-or-args3 "$@"
    local inputs=( ${inargs[@]} )

    if (( ${#inputs[@]} == 0 )) ; then
        ecerr "finder-img-gallery: no input paths"
        return 1
    fi

    (
        cdtmp @RET
        local out_dir
        out_dir="$PWD"

        local i=0
        local linked_n=0
        local p
        for p in "${inputs[@]}" ; do
            (( i++ ))

            local src="${p}"
            if ! test -e "${src}" ; then
                ecerr "finder-img-gallery: missing: ${src}"
                continue
            fi

            local ext="${src:e}"
            ext="${ext:-img}"

            local name
            name="$(printf '%04d.%s' "$i" "$ext")" @TRET

            reval command ln -s -- "${src}" "${name}" @RET
            (( linked_n++ ))
        done

        if (( linked_n == 0 )) ; then
            ecerr "finder-img-gallery: no existing paths to link"
            return 1
        fi

        if bool "${finder_img_gallery_open_p}" ; then
            command open -- "${out_dir}"
        fi
    )
}
##
