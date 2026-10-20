##
function h-org-paper-tools-dep {
    go-local-dep org-paper-metadata "${NIGHTDIR}/golang/org-paper-metadata" @RET
}

function h-org-paper-metadata-run {
    h-org-paper-tools-dep @RET
    #: Credentials cross through the environment, never argv or a file.
    local -x SemanticScholar_API_key="${SemanticScholar_API_key}"
    local -x deepseek_api_key="${deepseek_api_key}"
    local -x semantic_scholar_api_p="${semantic_scholar_api_p}"
    local -x semantic_scholar_scraper="${semantic_scholar_scraper:-auto}"
    local -x fhMode="${fhMode}"
    local line
    command org-paper-metadata "$@" 2> >(while IFS= read -r line ; do ecgray "${line}" ; done)
}

function h-semantic-scholar-page-url {
    h-org-paper-metadata-run --resolve "${1}"
}

function semantic-scholar-to-json-api {
    local input="${1:-$(pbpaste)}"
    assert-args input @RET
    input="$(semantic-scholar-url-get "${input}")" @RET
    h-org-paper-metadata-run --format json --scraper api "${input}"
}

function semantic-scholar-to-json {
    local input="${1:-$(pbpaste)}"
    assert-args input @RET
    h-org-paper-metadata-run --format json "${input}"
}

function semantic-scholar-to-json-scraping {
    semantic_scholar_api_p=n semantic-scholar-to-json "$@"
}

function semantic-scholar-to-org {
    setopt local_options pipe_fail
    local inargs
    in-or-args3 "$@" @RET
    h-org-paper-metadata-run --format org "${inargs[@]}" | cat-copy-if-tty
}
aliasfn ssorg semantic-scholar-to-org
##
function arxiv-to-org {
    local inargs
    in-or-args3 "$@" @RET

    local url
    for url in ${inargs[@]} ; do
        url="$(arxiv-url-get "$url")" @TRET

        local json
        json="$(revaldbg arxiv-json "$url")" @TRET

        ec "$json" |
            revaldbg paper_to_org.py "$url"
    done | cat-copy-if-tty
}
##
function semantic-scholar-dl-from-org {
    # if should-proxy-p ; then
    #     pxa89-local
    # fi

    local tlg_dest="${ssdl_tlg_dest:-$tlg_ch_books}"
    local tlg_p="${ssdl_tlg_p:-y}"

    local dir="${ss_dl_dir}"
    if test -z "$dir" ; then
        if isBorg ; then
            dir="$(gmktemp -d)" @TRET
        else
            dir="${paper_dir}"
            if ! test -d "$dir" ; then
                dir="."
            fi
        fi
    fi

    local org
    org="$(cat-paste-if-tty)" @TRET

    local urls
    if urls="$(ec "$org" | rget '^\s*:openAccessPdf:\s+(.*?)\s*$')" ; then
    elif urls="$(ec "$org" | urls-extract | rg '(?:\.pdf$|^https?://dl.acm.org/doi/pdf/)')" ; then
    elif urls="$(ec "$org" | urls-extract | rget '^https://api.semanticscholar.org/arXiv:([^/]+)$' | head -n 1)" ; then
        urls="https://export.arxiv.org/pdf/${urls}.pdf"
    else
        urls="$(ec "$org" | rget '^\s*:pdf_urls:\s+(.*?)\s*$')" @TRET
    fi
    urls="$(ec "${urls}" | arxiv-exportify)" @TRET
    urls="$(ec "${urls}" | ugrep -i -v '^https?://ieeexplore.ieee.org/stamp/stamp.jsp')" @TRET
    urls=(${(@f)urls})

    local url
    url="${urls[1]}"
    if test -z "$url" ; then
        ecerr "$0: no suitable URLs found!"
    fi

    local name
    name="$(
    ec "$org" |
    org-link-extract-title |
    arxiv-exportify |
    ghead -n 1
    )" @TRET

    local dest
    dest="${name}"
    [[ "$dest" == *.pdf ]] || dest+=".pdf"
    dest="$(ec "$dest" | str2filename-ascii)"

    # reval-ec retry aa-gateway "$url" -o "${dest}" @RET
    # reval-ec retry wget "$url" -O "${dest}" @RET
    #: [agfi:h-arxiv-pdf-dl] is a no-op for non-arXiv URLs, so it needs no test
    #: here: arXiv papers get the =arxiv.org= fallback, everything else gets a
    #: single URL. It replaces curl's =--create-dirs=, hence the =mkdir-m=.
    mkdir-m "$dir" @RET
    dest="${dir}/${dest}"
    h-arxiv-pdf-dl "$url" "${dest}" @RET

    if bool "${tlg_p}" ; then
        local lock_id="$0"
        retry_sleep=15 assert lock-aquire-redis-retry "${lock_id}" $((30*60)) @RET
        setopt localtraps
        trap "" $exit_traps[@]
        {
            #: The subshell allows us to exit via C-c, which we have otherwise disabled via the trap above.
            (
                {
                    ec "$org" |
                        perl -ple 's/\@(?:toread\S*|tosee\S*|CR\b)\s?//g' |
                        org2tlg "$tlg_dest"
                } || {
                    ecerr "$0: failed to send the description to Telegram"
                    return 1
                }

                reval-env-ec tlg_dest="$tlg_dest" retry_sleep=10 retry-limited 10 tsendf-book "$dest"
            )
        } always {
            lock-release-redis "${lock_id}"
            trap - $exit_traps[@]
        }
    fi
}
aliasfn ssdl semantic-scholar-dl-from-org
@opts-setprefix semantic-scholar-dl-from-org ssdl

function semantic-scholar-get-and-dl {
    # if should-proxy-p ; then
    #     pxa89-local
    # fi

    local inargs=() ret=0
    in-or-args3 "$@" @RET

    local url
    for url in ${inargs[@]} ; do
        { semantic-scholar-to-org "$url" |
              semantic-scholar-dl-from-org } || {
            ret=$?
            ecerr "$0: URL failed: ${url}"
        }
    done

    return ${ret}
}
aliasfn jss semantic-scholar-get-and-dl
##
function semantic-scholar-id-get {
    in-or-args "$@" |
        perl -lne '(m|^(?::paperId:\s*)?([^/]{40})$| || m|(?:https://www.semanticscholar.org/paper/\|https://www.connectedpapers.com/main/)(?:(?:[^/]+)/)?([^/]{40})(?:/\|$)| ) && print $1' |
        cat-copy-if-tty
}

function connectedpapers-from-ss-id {
    local inargs
    inargs=("${(@f)$(in-or-args "$@" | semantic-scholar-id-get)}") @TRET

    for ss_id in ${(@)inargs} ; do
        ec "https://www.connectedpapers.com/main/${ss_id}"
    done |
        cat-copy-if-tty
}
##
