##
typeset -g msc_thesis_dir=~cod/uni/thesis_worktrees

function thesis-dir-get {
    local worktree="${1:-thesis}"

    ec "${msc_thesis_dir}/${worktree}/thesis-template"
}

function z-thesis {
    local worktree="${1:-thesis}" dir
    dir="$(thesis-dir-get "${worktree}")" @RET
    cd "${dir}" @RET
}
alias zt=z-thesis
#: @todo disable me
##
function thesis-build {
    local worktree="${1:-thesis}" dir
    dir="$(thesis-dir-get "${worktree}")" @RET

    (
        assert cd "${dir}" @RET

        ecgray "$0: building in: ${dir}"

        mkdir -p build/
        xelatex-m thesis.tex @RET
        if [[ "${worktree}" != "thesis" ]] ; then
            local dest="thesis_${worktree}.pdf"
            assert command mv thesis.pdf "${dest}"  @RET

            ecgray "built: ${dest}"
        fi
    )
}

function h-thesis-build-emacs {
    local log=~/logs/thesis_build.log

    silent trs "$log" || true

    xelatex_open_p=n thesis-build >& "${log}"

    return 0
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
