## Google Docs
function gdoc-url {
    #: * gives the export URL for the given Google Docs URLs
    #: * @exampls
    #: ** 'https://docs.google.com/presentation/d/1fV_fGIFyUgXNU70xmIt6pb6ek6wSp-O1_esuXR2pomI/edit#slide=id.p'
    ##
    local -x format="${gdoc_f:-pdf}"

    in-or-args "$@" |
        perl -nle  'BEGIN { use v5.30 } ; (m{/(?<type>document|presentation)(?:/.*)?/d/(?<id>[^/]*)/} && print "https://docs.google.com/$+{type}/u/0/export?format=$ENV{format}&id=$+{id}") || say STDERR "gdoc-url: could not parse URL: $_"' |
        cat-copy-if-tty
}

function gdoc-epub {
    in-or-args "$@" |
        gdoc_f='epub' gdoc-url |
        inargsf revaldbg aab
}

function gdoc-pdf {
    in-or-args "$@" |
        gdoc_f='pdf' gdoc-url |
        inargsf revaldbg aa2tlg-book
}
##
