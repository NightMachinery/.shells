##
function ipynb2md {
    : "interface is different from ipynb2org!"

    reval-ec jupyter nbconvert "$@" --to markdown
}
##
function orgbabel-src-python-reset {
    local output_clear_p="${ipynb_output_clear_p:-y}"

    local p1=''
    if bool "$output_clear_p" ; then
        p1='(#\+begin_example\s*$)|'
        #: When the output has been cleared, all source blocks are translated as example blocks.
    fi

    cat-paste-if-tty |
    perl -lpe 's{^'${p1}'(#\+begin_src\s+(?:jupyter-)?python\b.*)}{#+begin_src jupyter-python :kernel py_base :session /jpy:127.0.0.1#6035:orgk1/ :async yes :exports both}g' |
    cat-copy-if-tty

    #: If converting from ipynb, just clear the cell outputs first instead. (This way you won't get extraneous 'caption' image blocks.)
    # perl -lpe 's{^#\+begin_example\s*$}{#+RESULTS:\n$&}g' |
}
##
function ipynb2org {
    local input="$1"
    assert-args input @RET
    local output_clear_p="${ipynb_output_clear_p:-y}"

    local dest="${2}"
    if test -z "$dest"; then
        dest="${input:r}.org"
        if test -e "$dest" ; then
            if ask "$0: overwrite destination: $dest" n ; then
                trs "$dest"
            else
                return 1
            fi
        fi
    fi

    ecgray "$0: WARNING This conversion is not lossless. In particular, it seems to drop backslashes sometimes (in Python strings?)."

    local opts=()

    if bool "${output_clear_p}" ; then
        opts+=(--ClearOutputPreprocessor.enabled=True)
    fi

    local tmp
    tmp="$(mktemp)" @TRET
    {
        assert revaldbg jupyter nbconvert "$input" --stdout "${opts[@]}" --to markdown > "$tmp" @RET
        { cat "$tmp" | md2org | orgbabel-src-python-reset > "$dest" } @TRET

        realpath "$dest"
    } always { silent trs-rm "$tmp" }
}

function emc-ipynb {
    local f="$1"

    local dest
    dest="$(gmktemp --suffix=.org)" @TRET

    ipynb_output_clear_p="${ipynb_output_clear_p:-n}" ipynb2org "$f" "$dest"

    emc_nowait2_colorize_p='non-modified' reval-ec emc-nowait2 "$dest"
}
##
