function twitter2threadreader {
    in-or-args "$@" |
        perl -ple 's|^https?://(?:twitter\.com\|threadreaderapp\.com)/[^/]+/(?:status\|thread)/(\d+).*$|https://threadreaderapp.com/thread/${1}.html|g' |
        cat-copy-if-tty
}

function twitter2org {
    local urls=($@)

    urls=(${(@f)"$(twitter2threadreader "${urls[@]}")"})

    local deusvult="${deusvult}"
    local url html
    for url in "${urls[@]}" ; do
        ecgray ''
        while true ; do
            html="$(revaldbg reval-memoi full-html2 "${url}")" @TRET
            thread_info="$(ec "$html" |
            pup '.thread-info')" || true
            if test -z "$thread_info" ; then
                deusvult=y

                ecngray $'\r'"$0: sleeping ..."
                sleep 1
                ecngray $'\r'"$0: retrying ..."

                continue
            else
                ecgray ''
            fi

            ecn "* [[$(org-escape-link "$url")]] by "
            ec "$html" |
                pup '.twitter_name' |
                html2plain |
                head -n1

            ec "$thread_info" |
                html2plain

            ec "$html" |
                pup '[data-controller="thread"]' |
                html2org

            break
        done
    done | cat-copy-if-tty
}
