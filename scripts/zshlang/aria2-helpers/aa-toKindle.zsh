#!/usr/bin/env zsh
local mark="$aaMark" # used to mark completion of this script
{
    local full="$3"
    local num="$2"
    [[ "$num" == '1' ]] || {
        ecerr "$num (> 1) files supplied to aa-tooKindle. Aborting."
        return 1
    }

    p2k "$full"
    # \rm -f "$full"
    # \rm -f "${full:r}.mobi"

} always { test -z "$mark" || touch "$mark" }
