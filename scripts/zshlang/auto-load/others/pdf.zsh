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
function pdf-cover-gs {
    local i="$1" out="${1:r}.png"
    assert-args i @RET

    sout command gs -o "$out" -sDEVICE=pngalpha -dLastPage=1 "$i"
    # See for setting the output resolution:
    # https://stackoverflow.com/questions/10091655/ghostscript-pdf-to-png-output-is-always-595x842-a4
    ## imagemagick sometimes did not work and didn't produce any error messages either
    # convert "pdf:${i}[0]" "png:$out"
    ##
}
function pdf-cover {
    : "works with EPUBs, too"

    pdf2png-mutool "$@" 1
}

function pdf2png-mutool {
    : "mutool draw [options] file [pages]"
    # pages:  Comma separated list of page numbers and ranges (for example: 1,5,10-15,20-N), where
    #               the character N denotes the last page.  If no pages are specified, then all pages
    #               will be included.

    local i="$1"
    assert-args i @RET
    local opts=()
    local format="${pdf2png_format:-png}"  # Default to png if not specified
    #: mutool doesn't seem to support jpeg/jpg

    local out="${pdf2png_o:-${i:r}_%03d.${format}}"
    [[ "$out" == *.${format} ]] || out+=".${format}"
    local dpi="${pdf2png_dpi}"
    if test -n "${dpi}" ; then
        opts+=(-r "${dpi}")
    fi
    assert serrdbg command mutool draw -o "$out" "${opts[@]}" -F "$format" "$i" "${@[2,-1]}" @RET
}

@opts-setprefix pdf2png-mutool pdf2png
aliasfn pdf2png pdf2png-mutool

function icat-pdf {
    local i="${1}" pages="${icat_p:-1}"
    assert-args i @RET

    local tmp
    tmp="$(gmktemp --suffix .png)" @TRET
    {
        assert @opts o "$tmp" @ pdf2png-mutool -w "$(screen-width)" "$i" "$pages" @RET

        icat_v=n icat "$tmp"
        # icat-kitty "$tmp"
        # icat-realsize "$tmp"
    } always {
        silent trs-rm "$tmp"
    }
}
@opts-setprefix icat-pdf icat
##
function pdf-count {
  setopt local_options pipefail
  { pdfinfo "$1" | grep Pages || { ecerr "$0 failed" ; return 1 } } |awk '{print $2}'
}
aliasfn pdf-pages-count pdf-count

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
function pdf-getpages-gs {
    local input="${1}"
    local from="${2}"
    local to="${3}"
    local output="${4:-${input:r}_${from}_${to}.pdf}"

    assert command gs -q -dNOPAUSE -dBATCH -dSAFER \
        -sDEVICE=pdfwrite \
        -dFirstPage=${from} \
        -dLastPage=${to} \
        -sOutputFile="${output}" "${input}" @RET

    # command du -h "${output}"
}

function pdf-getpages {
    local f="$1" from="$2" to="$3"
    local o="${4:-${1:r}_${from}_${to}.pdf}"
    pdftk A=$f cat A$from-$to output "$o"
}

function jpdfpages {
    pdf-getpages "$j" "$@"
}
##
function pdf-compress-pdftk {
    local input="${1}"
    local out="${2:-${input:r}_c.pdf}"

    reval-ec pdftk "${input}" output "${out}" compress
}

function pdf-compress {
    jglob

    local input="${1}"
    local out="${2:-${input:r}_compressed.pdf}"
    local dpi="${pdf_compress_dpi:-150}"
    local gray_p="${pdf_compress_gray_p}"
    local opts=(
        # -dEmbedAllFonts=true
        # -dSubsetFonts=true
        # -dDownsampleThreshold=1.0
    )

    if bool "${gray_p}" ; then
        opts+=(
            -sProcessColorModel=DeviceGray
            -sColorConversionStrategy=Gray
            -dOverrideICC
            #: @Claude/3.5-sonnet The -dOverrideICC switch is used to override or ignore ICC (International Color Consortium) profiles in PDF files during processing. When set to true, Ghostscript will ignore embedded ICC profiles and use its default color management instead.
        )
        out="${2:-${input:r}_cg.pdf}"
    else
        opts+=(
            # -dConvertCMYKImagesToRGB=true
            # -dProcessColorModel=/DeviceRGB
        )
    fi

    trs "$out" || true
    assert command gs -q -dNOPAUSE -dBATCH -dSAFER \
        -sDEVICE=pdfwrite \
        -dCompatibilityLevel=1.4 \
        -dDownsampleColorImages=true \
        -dDownsampleGrayImages=true \
        -dDownsampleMonoImages=true \
        -dPDFSETTINGS=/screen \
        -dColorImageDownsampleType=/Bicubic \
        -dColorImageResolution=$dpi \
        -dGrayImageDownsampleType=/Bicubic \
        -dGrayImageResolution=$dpi \
        -dMonoImageDownsampleType=/Bicubic \
        -dMonoImageResolution=$dpi \
        ${opts[@]} \
        -sOutputFile="$out" "$input" @RET

    command du -h "${input}" "${out}"
}
aliasfn pdf-compress-inplace inplace-io pdf-compress
@opts-setprefix pdf-compress-inplace pdf_compress

function pdf-compress-gray {
    pdf_compress_gray_p=y pdf-compress "$@"
}
##
function pdf-compress-qpdf {
  local qpdf_bin="${pdf_compress_qpdf_bin:-qpdf}"

  local optimize_images_p="${pdf_compress_qpdf_optimize_images_p:-y}"
  local keep_inline_images_p="${pdf_compress_qpdf_keep_inline_images_p:-}"
  local oi_min_width="${pdf_compress_qpdf_oi_min_width:-}"
  local oi_min_height="${pdf_compress_qpdf_oi_min_height:-}"
  local oi_min_area="${pdf_compress_qpdf_oi_min_area:-}"

  local input="${1}"
  local out="${2:-${input:r}_compressed.pdf}"

  local qpdf_opts=()

  if bool "${optimize_images_p}" ; then
    qpdf_opts+=(--optimize-images)
  fi

  if bool "${keep_inline_images_p}" ; then
    qpdf_opts+=(--keep-inline-images)
  fi

  if [[ -n "${oi_min_width}" ]] ; then
    qpdf_opts+=(--oi-min-width="${oi_min_width}")
  fi

  if [[ -n "${oi_min_height}" ]] ; then
    qpdf_opts+=(--oi-min-height="${oi_min_height}")
  fi

  if [[ -n "${oi_min_area}" ]] ; then
    qpdf_opts+=(--oi-min-area="${oi_min_area}")
  fi

  trs "${out}" || true

  assert command "${qpdf_bin}" "${qpdf_opts[@]}" --linearize "${input}" "${out}" @RET

  command du -h "${input}" "${out}"
}
##
function pdf-compress-ranged-v1 {
    #: This function splits the PDF files, compresses the specified range, and combines the parts back.
    #: This will lose links between the parts.
    ##
    jglob

    local input="${1}"
    local out="${2:-${input:r}_compressed.pdf}"
    local start_page="${pdf_compress_ranged_start}"
    local end_page="${pdf_compress_ranged_end}"

    #: If no range specified, just use regular pdf-compress
    if [[ -z "${start_page}" && -z "${end_page}" ]]; then
        pdf-compress "${input}" "${out}"
        return $?
    fi

    #: Get the total page count
    local page_count
    page_count="$(pdf-count "${input}")" @TRET

    #: Set defaults for empty values
    start_page="${start_page:-1}"
    end_page="${end_page:-$page_count}"

    #: Create a temporary directory for intermediate files
    local temp_dir
    temp_dir="$(mktemp -d)" @TRET

    local part1="${temp_dir}/part1.pdf"
    local part2="${temp_dir}/part2.pdf"
    local part3="${temp_dir}/part3.pdf"
    local part2_compressed="${temp_dir}/part2_compressed.pdf"

    local merge_files=()

    #: Extract before range (if needed)
    if (( start_page > 1 )); then
        assert pdf-getpages-gs "${input}" 1 $((start_page - 1)) "${part1}" @RET
        merge_files+=("${part1}")
    fi

    #: Extract the range to compress
    assert pdf-getpages-gs "${input}" "${start_page}" "${end_page}" "${part2}" @RET

    #: Compress the range using the existing pdf-compress function
    assert pdf-compress "${part2}" "${part2_compressed}" @RET
    merge_files+=("${part2_compressed}")

    #: Extract after range (if needed)
    if (( end_page < page_count )); then
        assert pdf-getpages-gs "${input}" $((end_page + 1)) "${page_count}" "${part3}" @RET
        merge_files+=("${part3}")
    fi

    #: Now merge all parts into the final output
    trs "$out" || true

    assert command gs -q -dNOPAUSE -dBATCH -dSAFER \
        -sDEVICE=pdfwrite \
        -sOutputFile="${out}" \
        ${merge_files[@]} @RET

    #: Clean up temporary files
    silent trs-rm "${temp_dir}" || true

    command du -h "${input}" "${out}"
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
function pdf-empty-create {
    local dest="${1}"
    local page_count="${2:-50}"

    local page_size="auto"
    # local page_size="a4paper"

    if test -z "${dest}" ; then
        local whiteboard_dir=~/tmp/whiteboards
        mkdir-m "${whiteboard_dir}"

        dest="$(gmktemp --tmpdir="${whiteboard_dir}" --dry-run --suffix .pdf)"
        ecgray "$dest"
    fi
    if [[ "${dest}" != *.pdf ]] ; then
        dest="${dest}.pdf"
    fi

    local temp_dir="$(gmktemp -d)"
    ecgray "$0: temp_dir: ${temp_dir}"

    local temp_tex_file="${temp_dir}/main.tex"

    local geometry_options='left=0mm,right=0mm,top=0mm,bottom=0mm'
    if [[ ${page_size} == 'auto' ]] ; then
        page_size=''
        geometry_options+=', paperwidth=16in, paperheight=9in'
    fi

    if test -n "${page_size}" ; then
        page_size="[${page_size}]"
    fi
    if test -n "${geometry_options}" ; then
        geometry_options="[${geometry_options}]"
    fi

    cat > "$temp_tex_file" <<EOF
\\documentclass${page_size}{article}
\\usepackage${geometry_options}{geometry}
\\usepackage{pdfpages}
\\begin{document}
\\pagestyle{empty}
EOF

    #: Add blank pages
    for ((i=1; i<=page_count; i++)); do
        ec "\\newpage\\null" >> "$temp_tex_file"
    done

    #: End the LaTeX document
    ec "\\end{document}" >> "$temp_tex_file"

    #: Compile the LaTeX document into a PDF
    assert reval-ecgray pdflatex -interaction=batchmode -output-directory "${temp_dir}" "${temp_tex_file}" @RET
    # &> "${temp_dir}/pdflatex.log"

    #: Move the PDF to the desired destination
    reval-ecgray gmv -i "${temp_tex_file:r}.pdf" "$dest"

    #: Clean up temporary files
    if isDebug ; then
        reval-ec emc-open "${temp_tex_file}"
    else
        silent trs-rm "${temp_dir}"
    fi
}
aliasfn pdf-blank-create pdf-empty-create
##
function pdf-ocr {
    local input="${1}"
    local dest="${2:-${input:r}_ocr.pdf}"
    assert-args input @RET

    reval-ec ocrmypdf --redo-ocr "${input}" "${dest}" @RET
}
##
