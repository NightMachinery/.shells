##
function hash-files {
    local engine="${hash_file_engine:-gsha512sum}"

    local temp_file
    temp_file="$(mktemp)" @TRET
    {
        for f in "$@"; do
            if test -d "$f"; then
                hash-dir "$f" >> "$temp_file" @RET
            else
                assert hash-file "$f" >> "$temp_file" @RET
            fi
        done

        #: Sort the hashes to ensure consistent order
        < "$temp_file" gsort |
            reval "${engine}" |
            awk '{print $1}' | #: This awk command is harmless for a well-behaved hash engine that returns a single hash, but it's useful for fixing the traditional hash commands.
            cat-copy-if-tty
    } always {
        silent trs-rm "${temp_file}"
    }
}

function hash-dir {
    : "hashes non-hidden files in the directory (recursive)"

    local d="$1"
    if ! test -d "$d" ; then
        if test -e "$d" ; then
            hash-file "$d"
            return $?
        else
            ecerr "$0: nonexistent: $(gquote-dq "$d")"
            return 1
        fi
    fi

    #: Iterate over all non-hidden files and directories in the directory (recursive)
    local files=("${d}"/*(N))
    hash-files "${files[@]}"
}
##
