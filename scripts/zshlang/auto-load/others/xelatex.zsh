##
function xelatex-m {
    local opts=("${@[1,-2]}") name="${@[-1]:-main}"

    local success_p=n
    {
        trs *.{log,xdv,aux,bbl,blg,xdv,ent}(.DN)
        #: =.ent=: endnotes cached files, used in Sharif Seminar

        local draft_opts=(--no-pdf)
        #: [[https://tex.stackexchange.com/questions/219811/draft-mode-in-xelatex][xetex - Draft-mode in XeLaTeX - TeX - LaTeX Stack Exchange]]

        local final_opts=()

        # redo_fail_mode=exit redo2 2
        reval-ecgray xelatex "${draft_opts[@]}" "${opts[@]}" "${name}.tex" @RET

        bibtex "${name}" @RET

        reval-ecgray xelatex "${draft_opts[@]}" "${opts[@]}" "${name}.tex" @RET
        reval-ecgray xelatex "${final_opts[@]}" "${opts[@]}" "${name}.tex" @RET

        success_p=y

        if bool "${xelatex_open_p:-n}" ; then
            open-sioyek "${name}.pdf" || true
        fi

        sioyek-reload || true
    } always {
        if bool "${success_p}" ; then
            bell-insaniquarium-sing
        else
            fsay 'failed to compile latex to PDF'
        fi
    }
}
##
