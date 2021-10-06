##
function mr-status() {
    local dir="${1:-$HOME}"

    reval-ec indir-exists "$dir" mr status -uno |& sd 'mr status:.*\n\n+' '' | pager-if-tty
}
alias mrs=mr-status
##
