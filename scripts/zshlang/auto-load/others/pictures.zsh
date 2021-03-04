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
function rm-alpha() {
    local B=$(basename "$1"); local D=$(dirname "$1");
    convert "$1" -background "$2" -alpha remove "$D/${B%.*}_$2.png"
}
function alpha2black() { rm-alpha "$1" black }
function alpha2white() { rm-alpha "$1" white }

combine-funcs alpha2bw alpha2black alpha2white
##
function saveas-img() { # image-paste, imgpaste; used from night-org.el
    local dest="$1" ; test -z "$dest" && {
        ecerr "$0: empty dest."
        return 1
    }
    pbpaste-plus

    local f="$paste[1]" # We do not support multipastes at this point
    if test -e "$f" ; then
        cp "$f" "$dest"
    else
        pngpaste "$dest"
    fi
}
##
function text2img-old() {
    # Doesn't work with Persian at all
    : " < text text2img <img-name>"
    local img="$1"

    test -z "$img" && { ecerr "$0: Empty image destination. Aborting." ; return 1 }

    # `convert -list font` for available fonts, or use absolute paths
    convert -page  4000x4000 -font FreeMono -pointsize 20 -fill black -background white -trim +repage -bordercolor white  -border 15 text:- png:"$img".png
    ## persian (doesn't work well)
    # ec سلام monster |     convert -page  700x700 -font '/Users/evar/Library/Fonts/B Nazanin Bold_0.ttf' -pointsize 20 -fill black -background white -trim +repage -bordercolor white  -border 15 text:- png:hi.png
    # Use https://stackoverflow.com/questions/23536375/linux-cli-how-to-render-arabic-text-into-bitmap instead
    ##
}
function text2img() {
    # @alt https://xg4.github.io/text2image/ does support Persian seemingly, but not completely?
    # @alt @good https://vincent7128.github.io/text-image/ seems to work as well as text2img.py with Persian, but it can also size the output image automatically.

    # local font="$NIGHTDIR/resources/fonts/unifont-13.0.06.ttf" # monospace font by GNU that supports most languages, but quite ugly and unreadable.
    local font="Courier New" # See https://github.com/IranOpenFontGroup/Discussions/issues/7 for more Persian monospace fonts
    
    text2img.py $font "$@"
}
function text-show() {
    local tmp="$(gmktemp --suffix .png)"

    text2img $tmp
    icat $tmp
    rm $tmp
}
function ts() {
    # @nameconflict with moreutils' ts which adds timestamps to stdin
    reval "$@" | text-show
}
##
function 2ico() {
    local i="${1}" o="${2:-${1:r}.ico}" s="${png2ico_size:-256}"

    convert -background transparent "$i" -define icon:auto-resize=16,24,32,48,64,72,96,128,256 "$o"
    # convert -resize x${s} -gravity center -crop ${s}x${s}+0+0 "$i" -flatten -colors 256 -background transparent "$o"
}
aliasfn png2ico 2ico
##
function convert-pad() {
    jglob
    local i="${1:? Input required}" o="${2:-${1:r}_padded.png}" w="${convert_pad_w:-${convert_pad_s:-1024}}" h="${convert_pad_h:-${convert_pad_s:-1024}}"

    local actualw=$(identify -format %w "$i")
    local actualh=$(identify -format %h "$i")

    if (( w < actualw )) ; then
        w=$actualw
    fi
    if (( h < actualh )) ; then
        h=$actualh
    fi
    
    convert "$i" -gravity center -extent ${w}x${h} "$o"
}
aliasfn img-fix-telegram convert-pad
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
function jiconpack() {
    jej

    unzip2dir $j
    mv **/*.png .
    re convert-pad *.png
}
##
function img-dimensions() {
  identify -format 'width=%wpx;height=%hpx;' "$1" # 2>/dev/null
}
##
