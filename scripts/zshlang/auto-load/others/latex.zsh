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
    typeset -A tex_vars=()
    if (( ${#latex_vars} > 0 )) ; then
        tex_vars=("${latex_vars[@]}") #: [key var] ...
    fi

    local tex=""

    tex+="\def\mypwd{${f:h}}"
    tex+="\def\CVDir{${nightNotes}/private/subjects/resume, CV}"

    for k in ${(k@)latex_vars} ; do
        tex+="\def\${k}{${latex_vars[$k]}}"
    done

    tex+="\input{\"${f}\"}"
    #: path with spaces needs to be quoted for \input

    local tex_f
    tex_f="$(gmktemp --suffix='.tex')" @TRET
    ec "$tex" > "$tex_f" @RET

    dact emc "$tex_f"

    reval-ec pdflatex "${opts[@]}" "$tex_f"
}
@opts-setprefix pdflatex-m latex
##
