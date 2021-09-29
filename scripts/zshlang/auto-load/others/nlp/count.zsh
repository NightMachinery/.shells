function count-words-spacy {
    local f garden="${count_words_garden:-$(jupytergarden-p)}"
    if (( ${#@} >= 1 )) ; then
        f="$1"
        f="$(grealpath "$f")" @TRET
    else
        f="$(gmktemp)" @TRET
        cat > "$f" @TRET
    fi

    if bool "$garden" ; then
        reval-ec @opts kernel_name 'python3' session 'spacy_1' json_output y quote y @ \
            jg-eval \
            =(ec '__name__ = "__garden__"'$'\n'
            cat "${commands[count_words.py]}"
            ec $'\n'"main($(gquote-dq "$f"))")
    else
        reval-ec count_words.py "$f"
    fi
}
@opts-setprefix count-words-spacy count_words

function count-words-wc {
    # almost as accurate as count-words-spacy
    ##
    gwc -w
}

aliasfn count-words count-words-wc

function count-words-humanfriendly {
     count-words | numfmt-humanfriendly
}
##
function html-get-reading-estimate() {
    local est
    est="$(cat "$1" | readtime.js)"
    ec "$(ec $est|jqm .humanizedDuration) ($(ec $est|jqm .totalWords) words)"
}
##
