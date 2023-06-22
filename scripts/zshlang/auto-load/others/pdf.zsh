##
function pdf-vspace-rm {
    #: Good for removing unwanted vertical space (newlines) in text extracted from PDFs. This is especially useful when one wants to use Google Translate on these files.
    ##
    cat-paste-if-tty \
        | perl -0777 -pe 's/\s*\x{C}\s*/ /g ; s/(?<!\R)\R(?!\R)/ /g' \
        | cat-copy-if-tty
}
##
function k2pdf() {
    local rtl="${k2pdf_rtl}"

    local opts=()
    if bool $rtl ; then
        opts+='-r'
        # Right-to-left page scans (-r)
    fi

    nis k2pdfopt "$opts[@]" "$@" -dev kv -png -bpc 2 -d -wrap+ -hy- -ws -0.2 -x -odpi "${k2pdf_odpi:-430}" -y -ui-
    # -as
}
##
function pdf-unencrypt() {
    mdocu "<file>
Uses ghostscript to rewrite the file without encryption." MAGIC
    local in="$1"
    gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile="${in:r}_unencrypted.pdf" -c .setpdfwrite -f "$in"
}
##
function pdf-cover-gs() {
    local i="$1" out="${1:r}.png"
    assert-args i @RET

    sout command gs -o "$out" -sDEVICE=pngalpha -dLastPage=1 "$i"
    # See for setting the output resolution:
    # https://stackoverflow.com/questions/10091655/ghostscript-pdf-to-png-output-is-always-595x842-a4
    ## imagemagick sometimes did not work and didn't produce any error messages either
    # convert "pdf:${i}[0]" "png:$out"
    ##
}
function pdf-cover() {
    : "works with EPUBs, too"

    pdf2png "$@" 1
}
function pdf2png-mutool() {
    : "mutool draw [options] file [pages]"
    # pages:  Comma separated list of page numbers and ranges (for example: 1,5,10-15,20-N), where
    #               the character N denotes the last page.  If no pages are specified, then all pages
    #               will be included.

    local i="$1"
    assert-args i @RET
    local out="${pdf2png_o:-${i:r}_%03d.png}"
    [[ "$out" == *.png ]] || out+='.png'

    assert serrdbg command mutool draw -o "$out" -F png "$i" "${@[2,-1]}" @RET
    : '`-r 300` to set dpi'
}
@opts-setprefix pdf2png-mutool pdf2png
aliasfn pdf2png pdf2png-mutool

function icat-pdf() {
    local i="${1}" pages="${icat_p:-1}"
    assert-args i @RET

    local tmp
    tmp="$(gmktemp --suffix .png)" @TRET
    {
        assert @opts o "$tmp" @ pdf2png-mutool -w "$(screen-width)" "$i" "$pages" @RET
        icat-realsize "$tmp"
    } always {
        silent trs-rm "$tmp"
    }
}
@opts-setprefix icat-pdf icat
##
function pdf-count() {
  setopt local_options pipefail
 { pdfinfo "$1" | grep Pages || { ecerr "$0 failed" ; return 1 } } |awk '{print $2}'
}

function k2pdf-split() {
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
##
function pdf-crop-margins-m {
    local i="$1" m="${2:-20}"
    assert-args i @RET
    local o="${pdf_crop_o:-${1:r}_m${m}.${1:e}}"

    reval-ec command pdf-crop-margins -p "$m" "$i" -o "$o"
    #
    #  Pre-crop the document by 5 points on each side before computing the bounding boxes.  Then crop retaining 50% of the computed margins.  This can be useful for difficult documents such as scanned books with page-edge noise or other "features" inside the current margins: =pdf-crop-margins -ap 5 -p 50 doc.pdf=
}
@opts-setprefix pdf-crop-margins-m pdf_crop

function pdf-crop-margins-inplace () {
    local u="$(uuidgen)"
    command pdf-crop-margins -p 1 "$@" -o "$u" # -p percent of margins retained
    # -p is percent retained of margins.
    silent trs-rm "${@[-1]}"
    command mv "$u" "${@[-1]}"
}

function pdfcrop-inplace() {
    jglob
    re pdf-crop-margins-inplace "$@"
}
aliasfn pcr pdfcrop-inplace
##
function pdfoutline {
    jglob

    mutool show "$1" outline
}
##
function pdf-getpages {
    local f="$1" from="$2" to="$3"
    local o="${4:-${1:r}_${from}_${to}.pdf}"
    pdftk A=$f cat A$from-$to output "$o"
}

function jpdfpages {
    pdf-getpages "$j" "$@"
}
##
function pdf-compress-gray {
    : 'ALT: `pdftk in.pdf output out.pdf compress` does not compress as much'

    jglob

    local input="${1}"
    local out="${2:-${input:r}_cg.pdf}"
    local dpi="${pdf_compress_gray_dpi:-150}"

        # -dEmbedAllFonts=true \
        # -dSubsetFonts=true \
    gs  -q -dNOPAUSE -dBATCH -dSAFER \
        -sDEVICE=pdfwrite \
        -dCompatibilityLevel=1.4 \
        -sProcessColorModel=DeviceGray -sColorConversionStrategy=Gray -dOverrideICC \
        -dDownsampleColorImages=true -dDownsampleGrayImages=true \
        -dPDFSETTINGS=/screen \
        -dColorImageDownsampleType=/Bicubic \
        -dColorImageResolution=$dpi \
        -dGrayImageDownsampleType=/Bicubic \
        -dGrayImageResolution=$dpi \
        -dMonoImageDownsampleType=/Bicubic \
        -dMonoImageResolution=$dpi \
        -sOutputFile="$out" "$input"
}
##
function pdf-numberme {
    ensure-dep-pdftk
    ##
    local input output
    input="$1"
    output="${2:-${1:r}_numbered.pdf}"
    [[ "$output" == *.pdf ]] || output+='.pdf'
    assert-args input @RET

    local pagenum
    pagenum=$(pdftk "$input" dump_data | grep "NumberOfPages" | cut -d":" -f2)

    {
        # enscript -L1 --header='||Page $% of $=' --output - < <(for i in $(seq "$pagenum"); do echo; done) | ps2pdf -
        # adds the page numbers to the top right
        ##
        # a page size of 595x842 PostScript points (a.k.a. A4 size), and the font Helvetica in 12 pt size
        local x y
        x=$(( int(595 / 2) - 35 ))
        ##
        y=60 # 0 is the bottom
        ##
        # y=830 # on the top
        ##

        command gs -o -    \
            -sDEVICE=pdfwrite        \
            -g5950x8420              \
            -c "/Helvetica findfont  \
            12 scalefont setfont \
            1 1  ${pagenum} {      \
            /PageNo exch def     \
            ${x} ${y} moveto        \
            (Page ) show         \
            PageNo 3 string cvs  \
            show                 \
            ( of ${pagenum}) show  \
            showpage             \
            } for"
    } | pdftk "$input" multistamp - output $output # will overwrite the output
}
##
function pdf-merge-poppler {
    local in=("${@[1,-2]}") out="${@[-1]}"
    assert-args in out @RET
    ensure-dir "$out" @RET

    reval-ec pdfunite "${in[@]}" "${out}"
}

function pdf-merge-gs {
    local in=("${@[1,-2]}") out="${@[-1]}"
    assert-args in out @RET
    ensure-dir "$out" @RET

    reval-ec command gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -dPDFSETTINGS=/prepress -sOutputFile="$out" "${in[@]}"
    #: -dPDFSETTINGS=/prepress has the effect of rotating pages that are too wide and force annoying horizontal scroll bars.
}

function pdf-merge-pdftk {
    local in=("${@[1,-2]}") out="${@[-1]}"
    assert-args in out @RET
    ensure-dir "$out" @RET

    reval-ec pdftk "${in[@]}" cat output "$out"
}

aliasfn pdf-merge pdf-merge-pdftk
##
function open-zip-pdf {
    local f="$1"

    if [[ "$f" =~ '.*\.pdf$' ]] ; then
        open "$f"
    elif [[ "$f" =~ '.*\.(rar|zip)$' ]] ; then
        local tmp
        tmp="$(gmktemp -d)"
        7z x -o"${tmp}/" "$f"
        re 'reval-ec open' "${tmp}/"*.pdf
    fi
}
##
