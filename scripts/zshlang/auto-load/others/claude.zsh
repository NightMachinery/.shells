##
function claude {
    local -x EDITOR=nvim
    local -x VISUAL="${EDITOR}"
    #: not sure if EDITOR is actually used

    tty-title "🍼${PWD:t}"

    $proxyenv command claude "$@"
}
##
function claude-autocommit {
     reval-ec claude --verbose -p 'git-committer' --allowedTools 'Bash(git:*)'

     reval-ecgray glola 5
}
##
function claude-vcsh-commit {
    local target_dir="${1:-$NIGHTDIR}"

    (
        cd "$target_dir" @RET
    
        claude -p "Read '${NIGHTDIR}/prompt/vcsh-commit.md' and start committing changes." --verbose --allowedTools 'Bash(vcsh night.sh:*)'

        reval-ecgray vcn-with glola 5
    )
}

function claude-night-sh {
    (
        cd "$NIGHTDIR" @RET
        
        claude "${NIGHTDIR}/prompt/night-sh.md"
    )
}
##
