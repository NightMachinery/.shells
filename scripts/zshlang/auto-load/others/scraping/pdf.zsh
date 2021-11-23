##
function html-get-pdf {
    local urls=( $@ )
    assert-args urls @RET
    local engine=("${html_get_pdf_e[@]:-full-html2}")
    local math_zoom="${html_get_pdf_math_zoom}"
    if test -z "$math_zoom" ; then
        math_zoom='0.5'
    fi
    local math_zoom_inv="$(( 1.0/math_zoom ))0" #: zsh outputs integer floats ala =1.= which CSS does not accept, so we are explicitly adding a zero
    local zoom="${html_get_pdf_zoom:-330%}"
    if [[ "$zoom" =~ '^\d+$' ]] ; then
        zoom+='%'
    fi

    local url
    for url in ${urls[@]}; do
        url="$(urlfinalg_print="${urlfinalg_print:-y}" urlfinalg "$url")" @TRET

        reval-env "$engine[@]" \
            "$url" | sanitize-css-external || {
            local r=$?
            ecerr "$0: Failed for URL $(gq "$url")"
            return $r
        }
    done

    cat << EOF

<head>
    <style>
     pre {
         white-space: pre-wrap;
         word-break: break-all;
     }

    .MathJax, .mjx-math, .mwe-math-element {
        /* font-size: ${math_size} !important; */
        zoom: ${math_zoom} !important;
    }

    table, img, .MathJax, .MathJax *, .MathJax_Display, .MathJax_Preview, .mjx-chtml, .mjx-math, .mjx-math *, .mwe-math-element, .mwe-math-element * {
        /*
        Warning: Do not set max-width for all elements; it constrained the text elements to a ~60% share of the screen width. (Perhaps the screen width is not set correctly in puppeteer? It rendered correctly when I opened it in Chrome.).
        Issue: doesn't seem to work for =table=s.
        Update: This does not shrink its children, so it seems useless? But it seemed to work for Wikipedia pages before we absolutified its links and thus switched to their rendered images instead of MathML.
        Update: good for =img= tags at least
        See also: https://stackoverflow.com/questions/70059431/css-how-do-i-make-an-element-scale-itself-so-that-it-doesnt-overflow-its-max-w
        */
        max-width: 100vw !important;
        text-align: left !important;
    }

    img {
        max-width: 100vw !important;
        text-align: left !important;
    }

    table {
          margin: 0 !important;
          width: 100vw !important;
    }
    /* This did not work, as the elements took less width than their actual content. (@idk why.) It also breaks some equations.
    table * {
          width: fit-content !important;
    }
    */

    body {
         font-size: ${zoom} !important;
    }

    /** arxiv-vanity */
    .ltx_align_right {
        text-align: right !important;

        /* zoom: ${math_zoom} !important; */
    }

    .ltx_Math {
        padding-right: 1%;
        /* These elements can have the wrong width. I don't know how to fix this, so I am just adding some padding as a margin of error.
        @userConfig/tradeoff @toFuture/1402 The padding makes the majority of cases ugly though, and the cases it helps with (long inline equations) might not common enough to be worth it.
        */

        zoom: ${math_zoom_inv}; /* Canceling out the math zoom for these (presumably) inline equations. */
    }
    /** */
    </style>
</head>
EOF
}
##
function html2pdf {
    : "Usage: html2pdf my.html > my.pdf"

    local url="$1"
    assert-args url @RET
    url="$(file-unix2uri-rp "$url")" @TRET

    html2pdf.js "$url"
}
##
function web2pdf {
    local urls=( $@ )
    assert-args urls @RET

    local title
    title="${web2pdf_title:-$(url-title "${urls[1]}" | str2filename)}" @TRET
    local dest
    dest="${web2pdf_o}"
    ensure-dir "$dest"
    if test -z "$dest" || test -d "$dest" ; then
        dest+="${title:-untitled}.pdf"
    fi
    local dest_html="${dest:r}.html"
    local html_get_pdf_math_zoom="${html_get_pdf_math_zoom:-${web2pdf_mz}}"
    local html_get_pdf_zoom="${html_get_pdf_zoom:-${web2pdf_z}}"

    html-get-pdf "${urls[@]}" > "$dest_html" @TRET
    html2pdf "$dest_html" > "$dest" @TRET

    realpath "$dest" @TRET

    if kindle-p ; then
        2ko "$dest" >&2 @TRET
    fi
}
alias w2p='\noglob web2pdf'
@opts-setprefix w2p web2pdf
##
function w2p-readmoz {
    html_get_pdf_e=(readmoz-nosanitize) \
    html_get_pdf_zoom="${html_get_pdf_zoom}" \
        html_get_pdf_math_zoom="${html_get_pdf_math_zoom}" \
        web2pdf "$@"
}
noglobfn w2p-readmoz
@opts-setprefix w2p-readmoz web2pdf

function w2p-wiki {
    : "AKA web2pdf-wikipedia"
    : "@warn Wikipedia renders its MathML tags as images on the server-side, so if you like to render them locally, you have to neuter theose =<img>= tags' URLs."

    html_get_pdf_zoom="${html_get_pdf_zoom}" \
        html_get_pdf_math_zoom="${html_get_pdf_math_zoom:-1}" \
        w2p-readmoz "$@"
}
noglobfn w2p-wiki
@opts-setprefix w2p-wiki web2pdf
##
