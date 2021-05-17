export hugo_base_dir=~/code/hugo/notes-hugo
typeset -g hugo_content_dir="$hugo_base_dir/content"

function hugo-sections-indices-create() {
    assert test -d "$hugo_content_dir" @RET

    assert pushf "$hugo_content_dir/posts" @RET
    {
        local d o
        for d in "${(@f)$(fd -uu --type d .)}" ; do
            o="$d/_index.md"

            ec "+++
title = \"${d:t}\"
type = \"index\"
draft = false
+++

" > $o @TRET

            ecgray "$0: created $(gq "$o")"
        done
    } always { popf }
}
