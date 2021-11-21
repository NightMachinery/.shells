##
function tex2png {
    local tex
    tex="$(in-or-args "$@")" @RET
    local dpi="${tex2png_dpi:-1500}"
    local dest
    dest="${tex2png_o:-$(mktemp-borg --suffix='.png')}" @TRET
    local pkgs=( ${tex2png_pkgs[@]} amsmath )

    pnglatex.bash -b Transparent -P 2 -p "${(j.:.)pkgs}" -d "$dpi" -o "$dest" -f "$tex" @TRET

    if isIReally ; then
        icat "$dest" >/dev/tty || true
    fi
}
alias xt='\noglob silence tex2png' #: =\= still needs escaping
##
