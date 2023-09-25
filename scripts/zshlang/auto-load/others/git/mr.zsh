##
typeset -g git_tracked_local=()

if isMB2 ; then
    git_tracked_local+=(
        ~cod/python/PyNight

        ~cod/orgmode-styles

        ~base/_Books/papers
    )
fi

function gmr {
    local cmd=("$@")

    local d
    for d in ${git_tracked_local[@]} ; do
        ecngray $'\n'"* "
        ecbold "$(path-abbrev ${d})$(colorfg "$gray[@]"): $(colorfg 0 0 0)$(gquote ${cmd[@]})"

        indir-exists "$d" "${cmd[@]}"
    done
}

function vcsh-all {
    local cmd=("$@")

    local repo vcsh_repos
    vcsh_repos=(${(@f)"$(vcsh list)"})

    for repo in ${vcsh_repos[@]} ; do
        ecngray $'\n'"* "
        ecbold "@vcsh ${repo}$(colorfg "$gray[@]"): $(colorfg 0 0 0)$(gquote ${cmd[@]})"

        indir-exists "${HOME}" vc-with "$repo" "${cmd[@]}"
    done
}

function gmrss {
    gmr gss

    vcsh-all gss -uno
}
##
function mr-status {
    local dir="${1:-$HOME}"

    reval-ec indir-exists "$dir" mr status -uno |& sd 'mr status:.*\n\n+' '' | pager-if-tty
}
alias mrs=mr-status
##
