##
function claude {
    local -x EDITOR=nvim
    local -x VISUAL="${EDITOR}"
    #: not sure if EDITOR is actually used

    $proxyenv command claude "$@"
}
##
function claude-vcsh-commit {
    local target_dir="${1:-$NIGHTDIR}"

    (
        cd "$target_dir" @RET
    
        claude "${NIGHTDIR}/prompt/vcsh-commit.md"
    )
}

function claude-night-sh {
    (
        cd "$NIGHTDIR" @RET
        
        claude "${NIGHTDIR}/prompt/night-sh.md"
    )
}
##
