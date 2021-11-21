#!/usr/bin/env zsh
local mark="$aaMark" # used to mark completion of this script
local engine="${aa_helper_e:-p2k}"
{
    local full="$3"
    local num="$2"
    [[ "$num" == '1' ]] || {
        ecerr "$num (> 1) files supplied to aa-helper-gen. Aborting."
        return 1
    }

    reval-ec "$engine" "$full"
} always { test -z "$mark" || touch "$mark" }
