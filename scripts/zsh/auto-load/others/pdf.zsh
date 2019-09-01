function pdf-unencrypt() {
    mdocu "<file>
Uses ghostscript to rewrite the file without encryption." MAGIC
    local in="$1"
    gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile="${in:r}_unencrypted.pdf" -c .setpdfwrite -f "$in"
}
function pdf-cover() {
    convert "$1[0]" "$1:r.png"
}
pdf-count() {
    pdfinfo "$1" | grep Pages|awk '{print $2}'
}
k2pdf-split() {
    doc usage: pdf k2pdf-options
    local s=0 pc="$(pdf-count "$1")" p="${k2_pages:-100}"
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
