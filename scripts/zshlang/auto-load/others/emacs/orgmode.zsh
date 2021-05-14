##
function org-watch() {
    local fd_in file

    fswatch --exclude='.' --include='\.org$' "$nightNotes" | {
        # protect our stdin:
        exec {fd_in}<&0
        exec </dev/null
        while read -d $'\n' -r file <&${fd_in} ; do
            ##
            # this won't work till the upstream bug of hardlink detection gets fixed:
            # dbg org-update-files "$file"
            ##
            org-update-files-all
        done
    } always {
        exec {fd_in}<&-
    }
}

function org-update-files() {
    local cmd
    cmd="(night/update-files $(emc-quote "$@"))"

    time2 revaldbg emc-eval "$cmd"
}
function org-update-files-all() {
    ##
    # fd --extension org --type f . "$nightNotes" | emc_eval_in=y emc-eval '(apply night/update-files lines)'
    ##
    fd --extension org --type f . "$nightNotes" | inargsf org-update-files
    ##
    # org-update-files "$nightNotes"/**/*.org
    ##
}
##
