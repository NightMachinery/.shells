##
function tex2pdf {
    #: * @tests
    #: ** `tex2pdf 'C(V,W,I)=\sum_{j=1}^{d^{\prime}}\mathbb{I}_{[\exists i\in V:W_{i j}>0]}I_{j}'`
    ##
    local tex_content
    tex_content="$(in-or-args "$@")" @RET

    local echo_dest_p="${tex2pdf_echo_dest_p}"
    local dest dest_dir
    dest="${tex2pdf_o}"
    if test -z "${dest}" ; then
        dest="$(mktemp-borg --suffix='.pdf')" @TRET

        echo_dest_p="${echo_dest_p:-n}"
    fi
    echo_dest_p="${echo_dest_p:-y}"

    if ! [[ "$dest" =~ '\.pdf$' ]] ; then
        dest+='.pdf'
    fi

    dest_dir="${dest:h}"

    local pkgs=( ${tex2pdf_pkgs[@]} amsmath amssymb )

    local extra_pkgs="${(j:,:)pkgs}"

    local tmp_tex_file tmp_dir
    tmp_dir="$(gmktemp -d)" @TRET
    {
        tmp_tex_file="${tmp_dir}/main.tex"

        cat > "$tmp_tex_file" <<EOF
\documentclass[preview,border=1pt,varwidth]{standalone}
\usepackage{${extra_pkgs}}
\begin{document}
\(
${tex_content}
\)
\end{document}
EOF

        assert sout pdflatex -interaction=batchmode \
            -output-directory "${tmp_dir}" \
            "${tmp_tex_file}" @RET
        #: @GPT4T The pdflatex command does not have a direct flag to suppress the banner. Instead, you can redirect the standard output to /dev/null to hide the banner along with other messages, except for errors which are sent to standard error.

        assert mv "${tmp_tex_file:r}.pdf" "$dest" @RET

        out="${dest}" #: @globalOutput
        if bool "${echo_dest_p}" ; then
            ec "${dest}"
        fi

        if isIReally && ! isBorg ; then
            local icat_v=n
            ##
            revaldbg icat "${dest}" || true
            ##
            # revaldbg icat-kitty-realsize "${dest}" || true
            ##
        fi
    } always {
        silent trs-rm "${tmp_dir}"
    }
}

function tex2png {
    local dpi="${tex2png_dpi:-1500}"
    local echo_dest_p="${tex2png_echo_dest_p}"
    echo_dest_p="${echo_dest_p:-y}"

    local dest
    dest="${tex2png_o:-$(mktemp-borg --suffix='.png')}" @TRET

    local pdf out
    tex2pdf_echo_dest_p=n tex2pdf "$@" @RET
    pdf="${out}"
    dact var-show pdf

    pdf2png_dpi="${dpi}" pdf2png_o="${dest}" assert pdf2png "${pdf}" @RET
    out="${dest}" #: @globalOutput

    if bool "${echo_dest_p}" ; then
        ec "${dest}"
    fi
}

function tex2png-v1 {
    #: @alt [agfi:tex2pdf] [agfi:tex2png]
    #: =tex2png-v1= is somewhat buggy, not recommended.
    ##
    local tex
    tex="$(in-or-args "$@")" @RET
    local dpi="${tex2png_dpi:-1500}"
    local dest
    dest="${tex2png_o:-$(mktemp-borg --suffix='.png')}" @TRET
    local pkgs=( ${tex2png_pkgs[@]} amsmath amssymb )

    pnglatex.bash -b Transparent -P 2 -p "${(j.:.)pkgs}" -d "$dpi" -o "$dest" -f "$tex" @TRET

    if isIReally && ! isBorg ; then
        local icat_v=n
        ##
        revaldbg icat "${dest}" || true
        ##
        # revaldbg icat-kitty-realsize "${dest}" || true
        ##
    fi
}

function tex-preview {
    if isBorg ; then
        tex2png "$@"
    else
        tex2pdf "$@"
    fi

}
alias xt='\noglob silence tex-preview'
#: Even with noglob, =\= still needs escaping
##
function latex-escape {
    in-or-args "$@" |
        perl -pe 's/([\#\$\%\&\~\_\^\\\{\}])/\\$1/g' |
        cat-copy-if-tty
}
aliasfn latex-url-escape latex-escape
##
function pdflatex-m {
    local autodir="${pdflatex_autodir:-n}"
    local fast_p="${pdflatex_fast_p:-n}"
    local bell_p="${pdflatex_bell_p:-y}"
    local bib_mode="${pdflatex_bib_mode:-bibtex_ignore}"
    local var_escaper
    #: [[id:a7185750-2d2e-4b83-af93-94ffdc9fb07e][latex/escaping]]
    var_escaper=(ec)
    # var_escaper=(lisp-quote)
    # var_escaper=(latex-escape)

    local opts=("${@[1,-2]}") f="${@[-1]}"
    if ! [[ "$f" =~ '\.tex$' ]] ; then
        f+='.tex'
    fi
    f_realpath="$(grealpath "$f")" @TRET
    assert-args f @RET
    local f_dir="${f_realpath:h}" name="${pdflatex_name:-${f:r:t}}" name_tmp
    if bool "${autodir}" ; then
        reval-ec pushf "${f_dir}"
    fi
    {
        local cv_p='n'
        if [[ "$name" =~ '^CV.*' || "${name}" == "references" ]] ; then
            ecgray "$0: CV Mode"

            cv_p=y
        fi

        local cv_dir
        cv_dir="${nightNotes}/private/subjects/resume, CV"

        typeset -A tex_vars=()
        if (( ${#latex_vars} > 0 )) ; then
            tex_vars=("${latex_vars[@]}") #: [key var] ...
        fi

        local tex=""
        if ! isDeus ; then
            tex+="\\pdfcompresslevel=0
\\pdfobjcompresslevel=0"$'\n'
        fi

        local nightJobName="${name}"
        local key val vars=(
            HOME
            nightNotes
            nightNotesPrivate
            nightNotesPublic
            nightResourcesPublic
            nightResourcesPrivate
            nightJobName
        )
        for key in ${vars[@]} ; do
            #: Check if parameter exists
            if ! [[ -v $key ]] ; then
                continue
            fi

            val="${(P)key}"
            # re var-show key val

            val="$(reval "${var_escaper[@]}" "${val}")" @TRET
            # var-show val

            tex+="\\newcommand{\\${key}}{${val}}"$'\n'
        done

        tex+="\\newcommand{\\globalBibPath}{$(reval "${var_escaper[@]}" ${nightGlobalBib})}"$'\n' @TRET
        tex+="\\newcommand{\\mypwd}{$(reval "${var_escaper[@]}" ${f_dir})}"$'\n' @TRET

        tex+="\\newcommand{\\CVDir}{${cv_dir}}"$'\n'
        if bool "${cv_p}" ; then
            assert gcp -v "${cv_dir}/"*.bib(.D) . @RET
        fi

        for key in ${(k@)latex_vars} ; do
            val="${latex_vars[$k]}"
            val="$(reval "${var_escaper[@]}" ${val})" @TRET
            tex+="\\newcommand{\\${k}}{${val}}"$'\n'
        done

        local f_escaped
        # f_escaped="$(reval "${var_escaper[@]}" ${f})"
        f_escaped="\"${f}\""
        tex+="\\input{${f}}"$'\n'
        #: path with spaces needs to be quoted for \input

        local tex_f
        tex_f="$(gmktemp --suffix='.tex')" @TRET
        var-show tex_f
        name_tmp="${tex_f:t:r}"
        ec "$tex" > "$tex_f" @RET

        # dact emc "$tex_f"

        opts+=(
            -jobname="${name_tmp}"
            -halt-on-error
        )
        draft_opts=(
            -draftmode
        )

        local success_p=n
        {
            trs *.aux(.DN) @TRET

            #: [[https://tex.stackexchange.com/questions/450863/using-bibtex-with-pdflatex][pdftex - Using BibTex with pdfLaTeX - TeX - LaTeX Stack Exchange]]
            time2 assert reval-ec pdflatex "${draft_opts[@]}" "${opts[@]}" "$tex_f" @RET

            if [[ "${bib_mode}" == bibtex ]] ; then
                time2 reval-ec bibtex *.aux @RET
            elif [[ "${bib_mode}" == bibtex_ignore ]] ; then
                time2 reval-ec bibtex *.aux @STRUE
            elif [[ "${bib_mode}" == n ]] ; then
                ecgray "$0: no bibtex"
            else
                ecerr "$0: unknown bib_mode: ${bib_mode}"
                return 1
            fi

            time2 reval-ec pdflatex "${draft_opts[@]}" "${opts[@]}" "$tex_f" @RET

            if ! bool "${fast_p}" ; then
                time2 reval-ec pdflatex "${draft_opts[@]}" "${opts[@]}" "$tex_f" @RET
                time2 reval-ec pdflatex "${draft_opts[@]}" "${opts[@]}" "$tex_f" @RET
                #: This is needed, otherwise the CVPR's review line numbers don't align properly at the start of the paragraphs.
            fi

            time2 reval-ec pdflatex "${opts[@]}" "$tex_f" @RET
            gmv -v "${name_tmp}.pdf" "${name}.pdf"

            success_p=y

            trs "${name_tmp}"*(.DN) || true
            sioyek-reload || true

        } always {
            if bool "${success_p}" ; then
                if bool "${bell_p}" ; then
                    bell-insaniquarium-sing
                fi

            else
                awaysh fsay 'failed to compile latex to PDF'
            fi
        }
    } always {
        if bool "${autodir}" ; then
            popf
        fi
    }
}
@opts-setprefix pdflatex-m latex

function h-pdflatex-emacs {
    local f="$1"

    local pdflatex_autodir=y
    pdflatex-m -shell-escape -interaction=nonstopmode "$f"
}

function h-pdflatex-emacs-async {
    local log="${HOME}/logs/pdflatex_emacs.ansilog"
    #: @duplicateCode/f96ce923c4daa8a299a2a376062acc30

    assert mkdir-m "${log:h}" @RET

    local f="$1"
    local marker="EMACS_PDFLATEX_${f}"
    kill-marker "${marker}" || true
    #: sometimes the processes are dead before we try to kill them and so this command returns non-zero

    awaysh-bnamed "${marker}" log-to "${log}" h-pdflatex-emacs "$@"
}

function pdflatex-full {
    #: @deprecated Use [agfi:pdflatex-m].
    ##
    local input="$1"
    assert-args input @RET

    reval-ec pdflatex -draftmode "${input}" @RET

    if isDeus ; then
        reval-ec bibtex "${input}" @RET # or biber
        # reval-ec makeindex "${input}".idx @RET # if needed
        # reval-ec makeindex -s style.gls @RET # for glossary if needed

        reval-ec pdflatex -draftmode "${input}" @RET
        #: [[https://tex.stackexchange.com/questions/342464/using-bibtex-and-pdflatex-why-are-three-latex-runs-needed][Using BibTeX and pdfLaTeX: why are three LaTeX runs needed? - TeX - LaTeX Stack Exchange]]
    fi

    reval-ec pdflatex "${input}" @RET
}
##
function bibtex2apa {
    local tmp
    tmp="$(gmktemp --suffix .bib)" @TRET
    cat-paste-if-tty > "$tmp" @RET

    #: add =-q= to disable verbosity
    bibtex2html -nokeys -o - -s apa -nodoc "$tmp"
}
##
function pix2tex-m {
    local preview_mode="${pix2tex_preview_mode:-local}"

    local image extension=png
    image="$(gmktemp --suffix ".${extension}")" @TRET

    assert pngpaste "${image}" @RET
    icat_v=n icat "${image}" || true

    ##
    # reval-ecgray \
        # command pix2tex --show "${image}"
    ##
    local res
    local retcode preview_url latex
    res="$(bb_image_to_latex.dash "${image}")" @TRET

    # ecgray "$res"

    retcode="$(ec "$res" | jq -re '.retcode')" || {
        ecgray "$res"
        ecerr "$0: failed to parse retcode"
        return 1
    }

    if (( ${retcode} == 0 )) ; then
        preview_url="$(ec "$res" | jq -re '.preview_url')" @TRET
        latex="$(ec "$res" | jq -re '.latex')" @TRET

        { ecn "$latex" | cat-copy-if-tty } @TRET

        ##
        if [[ "${preview_mode}" == 'web' ]] ; then
             open "${preview_url}"
        fi

        clipboard-add "${preview_url}" @STRUE
        ecgray "preview_url:"$'\n  '"${preview_url}"$'\n'
        ##

        if true || [[ "${preview_mode}" == 'local' ]] ; then
            local icat_v=n

            ecbold "* Original:"
            # icat-kitty-fit-width "${image}" || true
            icat "${image}" || true

            ecbold "* Ours:"
            ec "${latex}" | {
                tex2pdf
                # tex2png
            }
            #: Our current =tex2png= implementation is buggy:
            #: `C(V,W,I)=\sum_{j=1}^{d^{\prime}}\mathbb{I}_{[\exists i\in V:W_{i j}>0]}I_{j}`
            #: The : in =V:W= does not render for us.
        fi
    else
        return "${retcode}"
    fi
    ##
}
@opts-setprefix pix2tex-m pix2tex

aliasfn ocr-latex pix2tex-m
alias xs='ocr-latex'
##
function cv-upload {
    z CV @RET

    reval-ecgray rsp-safe CV.pdf CV_short.pdf references.pdf "${lilf_user}@${lilf_ip}:Downloads/"
}

function cv-build {
    z CV @RET

    local pdflatex_name="${pdflatex_name:-CV}"

    pdflatex-m "${nightNotesPrivate}/subjects/resume, CV/CV.tex"
    reval-ecgray pbadd "${pdflatex_name}.pdf"
}

function cv-short-build {
    pdflatex_name=CV_short cv-build "$@" @RET
}

function cv-1page-build {
    pdflatex_name=CV_1page cv-build "$@" @RET
}

function cv-consulate-build {
    pdflatex_name=CV_C cv-build "$@" @RET
}

function cv-references-build {
    pdflatex_name=references cv-build "$@" @RET
    command pdf-crop-margins -p4 100 10 100 100 references.pdf -o references_cropped.pdf
    command gmv references_cropped.pdf references.pdf
    pbadd references.pdf
}

function cv-pub-build {
    pdflatex_name=publications cv-build "$@" @RET
    command pdf-crop-margins -p4 100 10 100 100 publications.pdf -o publications_cropped.pdf
    command gmv publications_cropped.pdf publications.pdf
    pbadd publications.pdf
}
##
function bibtidy {
    if (( ${#@} == 0 )) ; then
        cat-paste-if-tty |
            h-bibtidy |
            cat-copy-if-tty
    else
        h-bibtidy "$@"
    fi
}

function h-bibtidy {
    local opts=(
        --v2
        --no-sort
        --duplicates
        --merge='last'
        --trailing-commas
        --encode-urls
        --blank-lines
        --space=2  #: default
    )

    command bibtex-tidy "${opts[@]}" "$@"
}
##
