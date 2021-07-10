#!/usr/bin/env zsh
local mark="$aaMark" # used to mark completion of this script, tmux kills it otherwise
{
    local full="$3"
    local num="$2"
    [[ "$num" == '1' ]] || {
        ecerr "$num files supplied to aa-2drive. Aborting."
        return 1
    }
    local relative="${$(realpath-relchild "$(pwd)" "$full"):h}"
    ecdbg pwd is "$(pwd)"
    [[ "$relative" == '.' ]] && relative=''
    rgeval rcr copy "$full" "rabbit0:aa/$relative" && {
        command rm "$full"
        echo "Uploaded and deleted $full" >> aa.log | cat
    } || { ecerr "Uploading $full failed" }

} always { test -z "$mark" || touch "$mark" }
