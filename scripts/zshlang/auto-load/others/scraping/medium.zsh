function medium-to-scribe {
    local inargs
    in-or-args2 "$@"

    ec "$inargs" |
        perl -pe \
            's|^https?://(?:.*\.)*(?<!link\.)medium\.com(/.*)?$|https://scribe.rip${1}|g' |
        prefixer --skip-empty |
        cat-copy-if-tty
}
