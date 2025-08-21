##
function nini-md {
    local topic_id="${1}" out
    if test -e "${topic_id}"; then
        ecgray "$0: input is a file, just converting to markdown"

        out="${topic_id}"
    else
        if [[ "${topic_id}" =~ 'ninisite\.com/discussion/topic/([0-9]+)' ]]; then
            topic_id="${match[1]}"
        elif [[ "${topic_id}" =~ '^([0-9]+)$' ]]; then
            #: already a topic id
        else
            ecerr "$0: could not parse topic id from: '${topic_id}'"
            return 1
        fi

        out="ninisite_${topic_id}.org"

        assert reval-ec get-the-nini --no-org-readable-line-breaks-p -o "${out}" -- "${topic_id}" @RET
    fi

    local out_md="${out:r}.md"
    cat "${out}" | pandoc_convert_trim_extra=n org2md > "${out_md}" @RET
    ec "${out_md}"
}
##
