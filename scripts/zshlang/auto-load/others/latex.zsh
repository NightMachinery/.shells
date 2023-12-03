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
    assert-args f @RET

    local cv_dir
    cv_dir="${nightNotes}/private/subjects/resume, CV"

    typeset -A tex_vars=()
    if (( ${#latex_vars} > 0 )) ; then
        tex_vars=("${latex_vars[@]}") #: [key var] ...
    fi

    local tex=""

    tex+="\def\mypwd{${f:h}}"
    tex+="\def\CVDir{${cv_dir}}"

    for k in ${(k@)latex_vars} ; do
        tex+="\def\${k}{${latex_vars[$k]}}"
    done

    tex+="\input{\"${f}\"}"
    #: path with spaces needs to be quoted for \input

    local tex_f
    tex_f="$(gmktemp --suffix='.tex')" @TRET
    ec "$tex" > "$tex_f" @RET

    dact emc "$tex_f"

    #: [[https://tex.stackexchange.com/questions/450863/using-bibtex-with-pdflatex][pdftex - Using BibTex with pdfLaTeX - TeX - LaTeX Stack Exchange]]
    assert reval-ec pdflatex "${opts[@]}" "$tex_f" @RET

    assert gcp -v "${cv_dir}/"*.bib . @RET
    assert reval-ec bibtex *.aux @RET

    assert reval-ec pdflatex "${opts[@]}" "$tex_f" @RET
    assert reval-ec pdflatex "${opts[@]}" "$tex_f" @RET
}
@opts-setprefix pdflatex-m latex
##
function bibtex2apa {
    local tmp
    tmp="$(gmktemp --suffix .bib)" @TRET
    cat-paste-if-tty > "$tmp" @RET

    #: add =-q= to disable verbosity
    bibtex2html -nokeys -o - -s apa -nodoc "$tmp"
}
##
