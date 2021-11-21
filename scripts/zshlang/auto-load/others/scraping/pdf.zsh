##
function html-get-pdf {
    local urls=( $@ )
    assert-args urls @RET
    local engine=("${html_get_pdf_e[@]:-full-html2}")

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
        font-size: ${full_html_math_zoom:-60}% !important;
    }
    img, .mjx-chtml, .MathJax, .mjx-math, .mwe-math-element, .MathJax *, .mjx-math *, .mwe-math-element * {
        /*
        We can even set this for everything, and we'll only break stuff with even lower max-widths
        Update: This does not shrink its children, so it seems useless? But it seemed to work for Wikipedia pages before we absolutified its links and thus switched to their rendered images instead of MathML.
        Update: good for =img= tags at least
        */
        max-width: 90vw !important;
        text-align: left !important;
    }

    body {
         font-size: ${full_html_zoom:-330}% !important;
    }
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
    html-get-pdf "${urls[@]}" > "$dest_html" @TRET
    html2pdf "$dest_html" > "$dest" @TRET

    realpath "$dest" @TRET

    if kindle-p ; then
        2ko "$dest" >&2 @TRET
    fi
}
alias w2p='\noglob web2pdf'
##
function w2p-readmoz {
    html_get_pdf_e=(readmoz-nosanitize) \
    full_html_zoom="${full_html_zoom}" \
        full_html_math_zoom="${full_html_math_zoom}" \
        web2pdf "$@"
}
noglobfn w2p-readmoz

function w2p-wiki {
    : "AKA web2pdf-wikipedia"
    : "@warn Wikipedia renders its MathML tags as images on the server-side, so if you like to render them locally, you have to neuter theose =<img>= tags' URLs."

    full_html_zoom="${full_html_zoom}" \
        full_html_math_zoom="${full_html_math_zoom}" \
        w2p-readmoz "$@"
}
noglobfn w2p-wiki
##
