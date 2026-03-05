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
    local format="${pdf2png_format:-png}"  #: Default to png if not specified
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
# aliasfn pdf2png pdf2png-mutool

function h-pdf-to-img-magick {
    local format="${1}"
    local input="${2}"
    local dest="${3:-${input:r}}"
    dest="${dest:r}.${format}"

    if [[ "$input" == "${dest}" ]] ; then
        ecerr "Error: Input and output paths are the same. Please specify a different output path."
        return 1
    fi


    local dpi="${pdf_to_img_magick_dpi:-${pdf2png_magick_dpi:-600}}"

    if [[ -z "${input}" ]]; then
        ecerr "Error: Input PDF path or page specifier missing."
        ecerr "Usage: pdf2${format}-magick <input_pdf_or_page_specifier> [output_pattern.${format}]"
        ecerr "Example: pdf2${format}-magick mydoc.pdf mydoc.${format}"
        ecerr "Example (specific page): pdf2${format}-magick mydoc.pdf[0] mydoc_page1.${format}"
        ecerr "Note: If 'output_pattern.${format}' is specified for a multi-page PDF without a page specifier in the input,"
        ecerr "ImageMagick will typically create files like 'output_pattern-0.${format}', 'output_pattern-1.${format}', etc."
        return 1
    fi

    reval-ecgray magick convert -density "${dpi}" "${input}" "${dest}" @RET
}

function pdf2png-magick {
    h-pdf-to-img-magick "png" "$@" @RET
}
@opts-setprefix pdf2png-magick pdf_to_img_magick
aliasfn pdf2png pdf2png-magick

function pdf2jpg-magick {
    h-pdf-to-img-magick "jpg" "$@" @RET
}
@opts-setprefix pdf2jpg-magick pdf_to_img_magick
aliasfn pdf2jpg pdf2jpg-magick
##

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
# aliasfn pcr pdfcrop-inplace
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
function h-pdf-rasterize-normalize-pdftoppm-names {
    local out_dir="${1}"
    local ext="${2}"

    local f
    for f in "${out_dir}"/page-*.${ext}(N) ; do
        local base="${f:t}"
        if [[ "${base}" =~ '^page-([0-9]+)\.' ]] ; then
            local n="${match[1]}"
            local padded
            printf -v padded '%04d' "${n}"
            local new="${out_dir}/page_${padded}.${ext}"
            assert command gmv --force -- "${f}" "${new}" @RET
        fi
    done
}

function h-pdf-rasterize-out-count {
    local out_dir="${1}"
    local ext="${2}"

    local outs=("${out_dir}"/page_*.${ext}(N) "${out_dir}"/page-*.${ext}(N))
    ec "${#outs[@]}"
}

function h-pdf-rasterize-progress-render {
    local done="${1}"
    local total="${2}"
    local backend="${3}"

    if ! [[ "${done}" =~ '^[0-9]+$' ]] ; then
        done=0
    fi
    if ! [[ "${total}" =~ '^[0-9]+$' ]] ; then
        total=0
    fi

    if (( total > 0 )) ; then
        (( done > total )) && done="${total}"
        local percent="$(( done * 100 / total ))"
        local bar_width=20
        local filled="$(( done * bar_width / total ))"
        local empty="$(( bar_width - filled ))"
        local bar_filled=""
        local bar_empty=""
        printf -v bar_filled '%*s' "${filled}" ''
        bar_filled="${bar_filled// /#}"
        printf -v bar_empty '%*s' "${empty}" ''
        bar_empty="${bar_empty// /-}"
        ecn $'\r'"pdf-rasterize(${backend}): [${bar_filled}${bar_empty}] ${done}/${total} (${percent}%)" >&2
    else
        ecn $'\r'"pdf-rasterize(${backend}): ${done} page(s)" >&2
    fi
}

function h-pdf-rasterize-progress-watch {
    local pid="${1}"
    local out_dir="${2}"
    local ext="${3}"
    local total="${4}"
    local backend="${5}"

    local done=0
    while command kill -0 "${pid}" >/dev/null 2>&1 ; do
        done="$(h-pdf-rasterize-out-count "${out_dir}" "${ext}")" @TRET
        h-pdf-rasterize-progress-render "${done}" "${total}" "${backend}"
        command sleep 0.2
    done

    done="$(h-pdf-rasterize-out-count "${out_dir}" "${ext}")" @TRET
    h-pdf-rasterize-progress-render "${done}" "${total}" "${backend}"
    ecerr ""
}

function pdf-rasterize {
    local backend="${pdf_rasterize_backend:-auto}"
    local density="${pdf_rasterize_density:-300}"
    local format_raw="${pdf_rasterize_format:-png}"
    local jpg_quality="${pdf_rasterize_jpg_quality:-70}"
    local background="${pdf_rasterize_background:-white}"
    local clean_p="${pdf_rasterize_clean_p:-y}"
    local progress_p="${pdf_rasterize_progress_p:-auto}"

    local input="${1}"
    local out_dir="${2:-${input:r}_rasterized}"
    local format="${format_raw:l}"

    if test -z "${input}" ; then
        ecerr "pdf-rasterize: input not supplied"
        return 1
    fi

    if ! test -f "${input}" ; then
        ecerr "pdf-rasterize: input not found: ${input}"
        return 1
    fi

    assert mkdir-m "${out_dir}" @RET

    if bool "${clean_p}" ; then
        trs-rm "${out_dir}"/page_*.${format}(N) || true
        trs-rm "${out_dir}"/page-*.${format}(N) || true
    fi

    case "${progress_p}" in
        auto)
            if isI ; then
                progress_p=y
            else
                progress_p=n
            fi
            ;;
    esac

    local page_count=0
    if bool "${progress_p}" ; then
        page_count="$(pdf-count "${input}" 2>/dev/null)" || page_count=0
        [[ "${page_count}" =~ '^[0-9]+$' ]] || page_count=0
    fi

    local try_backends=()
    case "${backend}" in
        auto)
            try_backends=(pdftoppm gs magick)
            ;;
        *)
            try_backends=("${backend}")
            ;;
    esac

    local ok_p=n
    local b
    for b in "${try_backends[@]}" ; do
        case "${b}" in
            pdftoppm)
                if command -v pdftoppm >/dev/null 2>&1 ; then
                    local ext="${format}"
                    local flag=""
                    local pdftoppm_jpeg_opts=()
                    case "${format}" in
                        png)
                            flag="-png"
                            ;;
                        jpg|jpeg)
                            ext="jpg"
                            flag="-jpeg"
                            if [[ "${jpg_quality}" =~ '^[0-9]+$' ]] && command pdftoppm -h 2>&1 | command grep -q -- '-jpegopt' ; then
                                pdftoppm_jpeg_opts=( -jpegopt "quality=${jpg_quality}" )
                            fi
                            ;;
                        tif|tiff)
                            ext="tiff"
                            flag="-tiff"
                            ;;
                        *)
                            ecerr "pdf-rasterize: unsupported format for pdftoppm: ${format}"
                            return 1
                            ;;
                    esac

                    if bool "${progress_p}" ; then
                        command pdftoppm -r "${density}" "${flag}" "${pdftoppm_jpeg_opts[@]}" -- "${input}" "${out_dir}/page" &
                        local raster_pid="${!}"
                        h-pdf-rasterize-progress-watch "${raster_pid}" "${out_dir}" "${ext}" "${page_count}" "pdftoppm" @RET
                        wait "${raster_pid}" || return $?
                    else
                        assert command pdftoppm -r "${density}" "${flag}" "${pdftoppm_jpeg_opts[@]}" -- "${input}" "${out_dir}/page" @RET
                    fi

                    h-pdf-rasterize-normalize-pdftoppm-names "${out_dir}" "${ext}" @RET
                    ok_p=y
                    format="${ext}"
                    break
                fi
                ;;
            gs)
                if command -v gs >/dev/null 2>&1 ; then
                    local device=""
                    local ext="${format}"
                    local gs_jpeg_opts=()
                    case "${format}" in
                        png)
                            device="png16m"
                            ;;
                        jpg|jpeg)
                            ext="jpg"
                            device="jpeg"
                            if [[ "${jpg_quality}" =~ '^[0-9]+$' ]] ; then
                                gs_jpeg_opts=( "-dJPEGQ=${jpg_quality}" )
                            fi
                            ;;
                        tif|tiff)
                            ext="tiff"
                            device="tiff24nc"
                            ;;
                        *)
                            ecerr "pdf-rasterize: unsupported format for gs: ${format}"
                            return 1
                            ;;
                    esac

                    if bool "${progress_p}" ; then
                        command gs -dSAFER -dBATCH -dNOPAUSE -dNOPROMPT "-r${density}" "-sDEVICE=${device}" "${gs_jpeg_opts[@]}" "-sOutputFile=${out_dir}/page_%04d.${ext}" -- "${input}" &
                        local raster_pid="${!}"
                        h-pdf-rasterize-progress-watch "${raster_pid}" "${out_dir}" "${ext}" "${page_count}" "gs" @RET
                        wait "${raster_pid}" || return $?
                    else
                        assert command gs -dSAFER -dBATCH -dNOPAUSE -dNOPROMPT "-r${density}" "-sDEVICE=${device}" "${gs_jpeg_opts[@]}" "-sOutputFile=${out_dir}/page_%04d.${ext}" -- "${input}" @RET
                    fi

                    ok_p=y
                    format="${ext}"
                    break
                fi
                ;;
            magick)
                if command -v magick >/dev/null 2>&1 ; then
                    local magick_opts=()
                    if [[ "${format}" == (jpg|jpeg) ]] && [[ "${jpg_quality}" =~ '^[0-9]+$' ]] ; then
                        magick_opts=(-strip -sampling-factor 4:2:0 -quality "${jpg_quality}")
                    fi

                    if bool "${progress_p}" ; then
                        command magick -density "${density}" -background "${background}" -alpha remove -alpha off -scene 1 "${magick_opts[@]}" -- "${input}" "${out_dir}/page_%04d.${format}" &
                        local raster_pid="${!}"
                        h-pdf-rasterize-progress-watch "${raster_pid}" "${out_dir}" "${format}" "${page_count}" "magick" @RET
                        wait "${raster_pid}" || return $?
                    else
                        assert command magick -density "${density}" -background "${background}" -alpha remove -alpha off -scene 1 "${magick_opts[@]}" -- "${input}" "${out_dir}/page_%04d.${format}" @RET
                    fi

                    ok_p=y
                    break
                elif command -v convert >/dev/null 2>&1 ; then
                    local convert_opts=()
                    if [[ "${format}" == (jpg|jpeg) ]] && [[ "${jpg_quality}" =~ '^[0-9]+$' ]] ; then
                        convert_opts=(-strip -sampling-factor 4:2:0 -quality "${jpg_quality}")
                    fi

                    if bool "${progress_p}" ; then
                        command convert -density "${density}" -background "${background}" -alpha remove -alpha off -scene 1 "${convert_opts[@]}" -- "${input}" "${out_dir}/page_%04d.${format}" &
                        local raster_pid="${!}"
                        h-pdf-rasterize-progress-watch "${raster_pid}" "${out_dir}" "${format}" "${page_count}" "convert" @RET
                        wait "${raster_pid}" || return $?
                    else
                        assert command convert -density "${density}" -background "${background}" -alpha remove -alpha off -scene 1 "${convert_opts[@]}" -- "${input}" "${out_dir}/page_%04d.${format}" @RET
                    fi

                    ok_p=y
                    break
                fi
                ;;
            *)
                ecerr "pdf-rasterize: unknown backend: ${b}"
                return 1
                ;;
        esac
    done

    if ! bool "${ok_p}" ; then
        ecerr "pdf-rasterize: no working backend found (tried: ${(j:, :)try_backends})"
        return 1
    fi

    local outs=("${out_dir}"/page_*.${format}(N))
    if (( ${#outs[@]} == 0 )) ; then
        ecerr "pdf-rasterize: no output produced in: ${out_dir}"
        return 1
    fi

    local out
    for out in "${outs[@]}" ; do
        ec "${out}"
    done
}
@opts-setprefix pdf-rasterize pdf_rasterize

function h-pdf-rasterize2word-pdf-size-twips {
    local input="${1}"

    if ! command -v pdfinfo >/dev/null 2>&1 ; then
        return 1
    fi

    local info
    info="$(command pdfinfo -f 1 -l 1 -- "${input}" 2>/dev/null)" || return 1
    test -n "${info}" || return 1

    ec "${info}" | perl -0777 -ne '
if (/^Page size:\s*([0-9.]+)\s*x\s*([0-9.]+)/m) {
    my $w = int(($1 * 20) + 0.5);
    my $h = int(($2 * 20) + 0.5);
    print "$w $h\n";
    exit 0;
}
exit 1;
'
}

function h-pdf-rasterize2word-reference-doc-create {
    local out="${1}"
    local page_width_twips="${2:-12240}"
    local page_height_twips="${3:-15840}"
    local margin_top="${4:-0}"
    local margin_right="${5:-0}"
    local margin_bottom="${6:-0}"
    local margin_left="${7:-0}"
    local margin_header="${8:-0}"
    local margin_footer="${9:-0}"
    local margin_gutter="${10:-0}"

    assert mkdir-m "${out:h}" @RET

    cat <<EOF | command pandoc \
        --from 'markdown+raw_attribute' \
        --to docx \
        --output "${out:A}" \
        - @RET
~~~{=openxml}
<w:p><w:pPr><w:sectPr><w:pgSz w:w="${page_width_twips}" w:h="${page_height_twips}"/><w:pgMar w:top="${margin_top}" w:right="${margin_right}" w:bottom="${margin_bottom}" w:left="${margin_left}" w:header="${margin_header}" w:footer="${margin_footer}" w:gutter="${margin_gutter}"/></w:sectPr></w:pPr></w:p>
~~~
EOF
}

function pdf-rasterize2word {
    local density="${pdf_rasterize2word_density:-${pdf_rasterize_density:-300}}"
    local format_raw="${pdf_rasterize2word_format:-${pdf_rasterize_format:-png}}"
    local jpg_quality="${pdf_rasterize2word_jpg_quality:-${pdf_rasterize_jpg_quality:-70}}"
    local background="${pdf_rasterize2word_background:-${pdf_rasterize_background:-white}}"
    local backend="${pdf_rasterize2word_backend:-${pdf_rasterize_backend:-auto}}"
    local force_page_break_p="${pdf_rasterize2word_force_page_break_p:-n}"
    local keep_images_p="${pdf_rasterize2word_keep_images_p:-n}"
    local images_dir_arg="${pdf_rasterize2word_images_dir:-}"
    local image_width="${pdf_rasterize2word_image_width:-100%}"
    local reference_doc="${pdf_rasterize2word_reference_doc:-}"
    local auto_reference_doc_p="${pdf_rasterize2word_auto_reference_doc_p:-y}"
    local auto_page_size_p="${pdf_rasterize2word_auto_page_size_p:-y}"
    local page_width_twips="${pdf_rasterize2word_page_width_twips:-}"
    local page_height_twips="${pdf_rasterize2word_page_height_twips:-}"
    local margin="${pdf_rasterize2word_margin:-144}"
    local margin_top="${pdf_rasterize2word_margin_top:-${margin}}"
    local margin_right="${pdf_rasterize2word_margin_right:-${margin}}"
    local margin_bottom="${pdf_rasterize2word_margin_bottom:-${margin}}"
    local margin_left="${pdf_rasterize2word_margin_left:-${margin}}"
    local margin_header="${pdf_rasterize2word_margin_header:-0}"
    local margin_footer="${pdf_rasterize2word_margin_footer:-0}"
    local margin_gutter="${pdf_rasterize2word_margin_gutter:-0}"
    ensure-array pdf_rasterize2word_pandoc_opts
    local pandoc_opts=("${pdf_rasterize2word_pandoc_opts[@]}")

    local input="${1}"
    local out="${2:-${input:r}_rasterized.docx}"
    local format="${format_raw:l}"
    [[ "${out}" == *.docx ]] || out+='.docx'

    if test -z "${input}" ; then
        ecerr "pdf-rasterize2word: input not supplied"
        return 1
    fi

    if ! test -f "${input}" ; then
        ecerr "pdf-rasterize2word: input not found: ${input}"
        return 1
    fi

    local reference_doc_tmp=""
    if test -n "${reference_doc}" ; then
        if ! test -f "${reference_doc}" ; then
            ecerr "pdf-rasterize2word: reference doc not found: ${reference_doc}"
            return 1
        fi
    elif bool "${auto_reference_doc_p}" ; then
        if bool "${auto_page_size_p}" && { test -z "${page_width_twips}" || test -z "${page_height_twips}" ; } ; then
            local page_size
            page_size="$(h-pdf-rasterize2word-pdf-size-twips "${input}")" || page_size=""
            if test -n "${page_size}" ; then
                local page_size_parts=("${(z)page_size}")
                page_width_twips="${page_width_twips:-${page_size_parts[1]}}"
                page_height_twips="${page_height_twips:-${page_size_parts[2]}}"
            fi
        fi
        page_width_twips="${page_width_twips:-12240}"
        page_height_twips="${page_height_twips:-15840}"

        reference_doc_tmp="$(gmktemp --suffix '.docx')" @TRET
        h-pdf-rasterize2word-reference-doc-create \
            "${reference_doc_tmp}" \
            "${page_width_twips}" \
            "${page_height_twips}" \
            "${margin_top}" \
            "${margin_right}" \
            "${margin_bottom}" \
            "${margin_left}" \
            "${margin_header}" \
            "${margin_footer}" \
            "${margin_gutter}" @RET
        reference_doc="${reference_doc_tmp}"
    fi

    if test -n "${reference_doc}" ; then
        pandoc_opts+=( --reference-doc "${reference_doc}" )
    fi

    local images_dir=""
    local tmp_dir=""

    if test -n "${images_dir_arg}" ; then
        images_dir="${images_dir_arg}"
        assert mkdir-m "${images_dir}" @RET
    else
        tmp_dir="$(gmktemp -d)" @TRET
        images_dir="${tmp_dir}"
    fi

    assert mkdir-m "${out:h}" @RET

    local pdf_rasterize_density="${density}"
    local pdf_rasterize_format="${format}"
    local pdf_rasterize_jpg_quality="${jpg_quality}"
    local pdf_rasterize_background="${background}"
    local pdf_rasterize_backend="${backend}"

    local rasterized_output
    rasterized_output="$(pdf-rasterize "${input}" "${images_dir}")" @TRET
    local images=("${(@f)rasterized_output}")
    if (( ${#images[@]} == 0 )) ; then
        ecerr "pdf-rasterize2word: no images found in: ${images_dir}"
        if test -n "${reference_doc_tmp}" ; then
            silent trs-rm "${reference_doc_tmp}" || true
        fi
        if test -n "${tmp_dir}" ; then
            silent trs-rm "${tmp_dir}" || true
        fi
        return 1
    fi

    local progress_p=n
    if isI ; then
        progress_p=y
        # ecgray "$0: progress bar activated because stdout is a terminal"
    fi

    local images_n="${#images[@]}"

    local convert_ret=0
    {
        local image
        local image_i=0
        for image in "${images[@]}" ; do
            (( image_i++ ))
            if bool "${progress_p}" ; then
                local percent="$(( image_i * 100 / images_n ))"
                local bar_width=20
                local filled="$(( image_i * bar_width / images_n ))"
                local empty="$(( bar_width - filled ))"
                local bar_filled=""
                local bar_empty=""
                printf -v bar_filled '%*s' "${filled}" ''
                bar_filled="${bar_filled// /#}"
                printf -v bar_empty '%*s' "${empty}" ''
                bar_empty="${bar_empty// /-}"
                ecn $'\r'"pdf-rasterize2word: [${bar_filled}${bar_empty}] ${image_i}/${images_n} (${percent}%)" >&2
            fi

            ec "![](${image:t}){width=${image_width}}"
            if bool "${force_page_break_p}" && [[ "${image}" != "${images[-1]}" ]] ; then
                cat <<'EOF'
```{=openxml}
<w:p><w:r><w:br w:type="page"/></w:r></w:p>
```
EOF
            fi
            ec ''
        done

        if bool "${progress_p}" ; then
            ecerr ""
        fi
    } | indir "${images_dir}" pandoc \
        --from 'markdown+raw_attribute' \
        --to docx \
        "${pandoc_opts[@]}" \
        --output "${out:A}" \
        - || convert_ret="$?"

    if test -n "${tmp_dir}" ; then
        if bool "${keep_images_p}" ; then
            ec "${tmp_dir}"
        else
            silent trs-rm "${tmp_dir}" || true
        fi
    fi

    if test -n "${reference_doc_tmp}" ; then
        silent trs-rm "${reference_doc_tmp}" || true
    fi

    if (( convert_ret != 0 )) ; then
        return "${convert_ret}"
    fi

    ec "${out}"
}
@opts-setprefix pdf-rasterize2word pdf_rasterize2word

function pdf-rasterize2word-compress {
    reval-ec-env \
        pdf_rasterize2word_format=jpg \
        pdf_rasterize2word_density=180 \
        pdf_rasterize2word_jpg_quality=60 \
        pdf-rasterize2word "$@"
}
@opts-setprefix pdf-rasterize2word-compress pdf_rasterize2word_compress
##
