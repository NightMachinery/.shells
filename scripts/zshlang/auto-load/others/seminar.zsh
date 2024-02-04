##
function seminar-compile {
    (
        z seminar_final
        reval-ec xelatex-m seminar_mehri
    )
}

function h-seminar-compile-emacs {
    local log=~/logs/seminar_compile.log

    silent trs "$log" || true

    xelatex_open_p=n seminar-compile >& "${log}"

    return 0
}
##
function agr-replace-latex-command {
    local cmd="$1"

    reval-ec-env agr_regex=y from='(?i)\\'"${cmd:l}" to='\\'"${cmd}" agr
}
##
