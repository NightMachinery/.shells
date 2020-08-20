function k2pdf() {
    nis k2pdfopt "$@" -dev kv -png -bpc 2 -d -wrap+ -hy- -ws -0.2 -x -odpi "${k2_odpi:-450}" -y -ui-
    # -as
}
function pdf-unencrypt() {
    mdocu "<file>
Uses ghostscript to rewrite the file without encryption." MAGIC
    local in="$1"
    gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile="${in:r}_unencrypted.pdf" -c .setpdfwrite -f "$in"
}
function pdf-cover() {
    convert "pdf:$1[0]" "png:$1:r.png"
}
pdf-count() {
  setopt local_options pipefail
 { pdfinfo "$1" | grep Pages || { ecerr "$0 failed" ; return 1 } } |awk '{print $2}'
}
k2pdf-split() {
    doc usage: pdf k2pdf-options
    local s=0 pc p
    pc="$(pdf-count "$1")" || return 1
    p="${k2_pages:-100}"
    local e i=0
    e=$[s+p]
    until test $s -gt $pc
    do
        k2pdf "$@" -p "$s-$e" -o "%fp$i %b _pages ${s} to ${e}.pdf"
        s=$e
        e=$[s+p]
        i=$[i+1]
    done
}
function pdf-crop-margins () {
    local u="$(uuidgen)"
    command pdf-crop-margins -p 1 "$@" -o "$u"
    # -p is percent retained of margins.
    \rm "${@[-1]}"
    \mv "$u" "${@[-1]}"
}
pdfcrop() {
    jglob
    re pdf-crop-margins "$@"
}
aliasfn pcr pdfcrop
pdfoutline() { jglob ; mutool show "$1" outline }

function pdf-getpages() {
    local f="$1" from="$2" to="$3"
    local o="${4:-${1:r}_$from_${to}.pdf}"
    pdftk A=$f cat A$from-$to output "$o"
}
function jpdfpages() {
    pdf-getpages "$j" "$@"
}
