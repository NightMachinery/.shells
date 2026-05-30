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
     reval-ec claude-m --verbose -p 'git-committer' --allowedTools 'Bash(git:*)'

     ecgray
     reval-ecgray glola 5
}
##
function claude-vcsh-commit {
    local target_dir="${1:-$NIGHTDIR}"
    local engine=("${claude_commit_engine[@]:-claude-m}")

    (
        cd "$target_dir" @RET
    
        reval-ecgray "${engine[@]}" -p "Read '${NIGHTDIR}/AGENTS.md' and '${NIGHTDIR}/PE/vcsh-commit.md' and start committing changes." --verbose --allowedTools 'Bash(vcsh night.sh:*)'

        ecgray
        reval-ecgray vcn-with glola 5
    )
}

function claude-night-sh {
    (
        cd "$NIGHTDIR" @RET
        
        claude-m "${NIGHTDIR}/prompt/night-sh.md"
    )
}
##
function claude-pioneer {
    local -x ANTHROPIC_AUTH_TOKEN="${pioneer_api_key}"
    local -x ANTHROPIC_BASE_URL="https://api.pioneer.ai/"

    claude "$@"
}

aliasfn claude-m claude-pioneer
##
