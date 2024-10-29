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
        perl -0777 -pe 's/\s*\36/\0/g'
    else
        ##
        # perl -0777 -pe 's/\36/\0/g'
        ##
        tr '\36' '\0'
    fi
}
##
