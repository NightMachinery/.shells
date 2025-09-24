##
typeset -g msc_thesis_dir=~cod/uni/thesis/thesis-template
##
function thesis-build {
    (
        assert cd "${msc_thesis_dir}" @RET

        mkdir -p build/
        xelatex-m thesis.tex
    )
}

##
function thesis-cx {
    (
        assert cd "${msc_thesis_dir}" @RET

        cat  PE/thesis_prompt_v1.2.md
        cx chapters/*.tex thesis.tex styles/**/*(.D)  figs/cover.tex figs/readme.txt
    ) | cat-copy-if-tty @RET
}
##
