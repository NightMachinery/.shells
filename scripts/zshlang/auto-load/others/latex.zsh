##
function tex2png {
    local tex
    tex="$(in-or-args "$@")" @RET
    local dpi="${tex2png_dpi:-1500}"
    local dest
    dest="${tex2png_o:-$(mktemp-borg --suffix='.png')}" @TRET
    local pkgs=( ${tex2png_pkgs[@]} amsmath amssymb )

    pnglatex.bash -b Transparent -P 2 -p "${(j.:.)pkgs}" -d "$dpi" -o "$dest" -f "$tex" @TRET

    if isIReally ; then
        icat "$dest" >/dev/tty || true
    fi
}
alias xt='\noglob silence tex2png' #: =\= still needs escaping
##
function latex-url-escape {
    cat-paste-if-tty |
        perl -pe 's/([\#\$\%\&\~\_\^\\\{\}])/\\$1/g' |
        cat-copy-if-tty
}
##
function pdflatex-m {
    local opts=("${@[1,-2]}") f="${@[-1]}"
    if ! [[ "$f" =~ '\.tex$' ]] ; then
        f+='.tex'
    fi
    # f="$(grealpath "$f")" @TRET
    assert-args f @RET
    local name="${f:r:t}" name_tmp

    local cv_p='n'
    if [[ "$name" =~ '^CV.*' ]] ; then
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
        tex+="\pdfcompresslevel=0
\pdfobjcompresslevel=0"$'\n'
    fi

    tex+="\def\mypwd{${f:h}}"$'\n'

    if bool "${cv_p}" ; then
        tex+="\def\CVDir{${cv_dir}}"$'\n'

        assert gcp -v "${cv_dir}/"*.bib . @RET
    fi

    for k in ${(k@)latex_vars} ; do
        tex+="\def\${k}{${latex_vars[$k]}}"$'\n'
    done

    tex+="\input{\"${f}\"}"$'\n'
    #: path with spaces needs to be quoted for \input

    local tex_f
    tex_f="$(gmktemp --suffix='.tex')" @TRET
    name_tmp="${tex_f:t:r}"
    ec "$tex" > "$tex_f" @RET

    # dact emc "$tex_f"

    opts+=(-jobname="${name_tmp}")

    {
        #: [[https://tex.stackexchange.com/questions/450863/using-bibtex-with-pdflatex][pdftex - Using BibTex with pdfLaTeX - TeX - LaTeX Stack Exchange]]
        assert reval-ec pdflatex -draftmode "${opts[@]}" "$tex_f" @RET

        reval-ec bibtex *.aux @RET

        reval-ec pdflatex -draftmode "${opts[@]}" "$tex_f" @RET
        reval-ec pdflatex "${opts[@]}" "$tex_f" @RET
        gmv -v "${name_tmp}.pdf" "${name}.pdf"
        trs "${name_tmp}"*(.D)
    } always {
        bell-insaniquarium-sing
    }
}
@opts-setprefix pdflatex-m latex

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
