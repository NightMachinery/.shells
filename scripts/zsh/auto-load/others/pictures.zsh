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
function saveas-img() {
    # used from night-org.el
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
function text2img() {
    : " < text text2img <img-name>"
    local img="$1"

    test -z "$img" || { ecerr "$0: Empty image destination. Aborting." ; return 1 }

    convert -page  4000x4000 -font FreeMono -pointsize 20 -fill black -background white -trim +repage -bordercolor white  -border 15 text:- png:"$img".png
}
