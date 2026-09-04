permute-case() {
    eval "printf '%s\0' $(echo "$@" | gsed 's/./{\U&,\L&}/g')"
}
##
function NUL2NL {
    tr '\0' '\n'
}

function NL2NUL {
    tr '\n' '\0'
}

function NUL2RS {
    tr '\0' '\36'
}

function RS2NUL {
    local trim_trailing_whitespace_p="${unseal_trim_trailing_whitespace_p:-n}"

    if bool "${trim_trailing_whitespace_p}" ; then
        #: `\s*\36` trims the newline [agfi:seal] writes before each separator.
        #: `\s+\z` does the same for the last record, which has no separator
        #: after it and so was never reached by the first substitution.
        #: The rest drops empty records: a leading separator (which [agfi:seal]
        #: writes into an existing-but-empty attic), or a blank line left by a
        #: hand edit.
        perl -0777 -pe 's/\s*\36/\0/g; s/\s+\z//; s/\0+/\0/g; s/\A\0+//; s/\0+\z//'
    else
        ##
        # tr '\36' '\0'
        ##
        perl -0777 -pe 's/\36/\0/g; s/\0(?:\s*\0)+/\0/g; s/\A\0+//; s/\0+\z//'
    fi
}
##
