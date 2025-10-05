##
function xelatex-m {
    local opts=("${@[1,-2]}") name="${@[-1]:-main}"
    local open_p="${xelatex_open_p:-n}"
    local interactive_p="${xelatex_interactive:-n}"
    local build_dictionary_script="${xelatex_build_dictionary_script:-scripts/build_dictionary.py}"

    #: remove .tex from end of name
    name="${name%.tex}"

    if ! bool "${interactive_p}" ; then
        opts+=(
            ##
            # --interaction=nonstopmode
            #: @G2.5 continue compiling as much as possible when it encounters errors, rather than stopping
            ##
            --halt-on-error
        )
    fi

    local success_p=n
    {
        trs **/*.{log,xdv,aux,bbl,blg,xdv,ent,toc}(.DN)
        #: =.ent=: endnotes cached files, used in Sharif Seminar

        local draft_opts=(--no-pdf)
        #: [[https://tex.stackexchange.com/questions/219811/draft-mode-in-xelatex][xetex - Draft-mode in XeLaTeX - TeX - LaTeX Stack Exchange]]

        local final_opts=()

        # redo_fail_mode=exit redo2 2
        reval-ecgray xelatex "${draft_opts[@]}" "${opts[@]}" "${name}.tex" @RET

        if test -n "${build_dictionary_script}" && test -e "${build_dictionary_script}" ; then
            python "${build_dictionary_script}" @RET
        fi

        bibtex "${name}" @RET

        reval-ecgray xelatex "${draft_opts[@]}" "${opts[@]}" "${name}.tex" @RET
        reval-ecgray xelatex "${final_opts[@]}" "${opts[@]}" "${name}.tex" @RET

        success_p=y

        if bool "${open_p}" ; then
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
