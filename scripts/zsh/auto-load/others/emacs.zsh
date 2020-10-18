##
function doom-sync() {
    rust-setup
    doom sync
}
##
function emcpe() {
    local fz_opts=( $fz_opts[@] -1 )
    # ffkill -SIGUSR2 \'emacs \'daemon
    ffkill -SIGUSR2 emacs daemon
    emacsclient -e '(setq debug-on-quit nil)'
}
function emn() {
    emc -e '(helm-man-woman "")' # can't input to helm using its arg. why?
    #"(woman \"$*\")"
}
## did not work
# function emc-openclose() {
#     local f="$1"
#     emc -e "(progn (find-file ${f:q}) (save-buffers-kill-terminal)"
#     # { sleep 0.1 ; emc -e '(save-buffers-kill-terminal)' } &
#     # emc "$f"
#     reset
# }
# reify emc-openclose
# function emacs-import-vfiles() {
#     init-vfiles yes
#     emc-openclose "${vfiles[@]}"
# }
##
function emacs-vfiles() {
    : "Paste the result into the recentf list of emacs. You'll need to have set the max limit appropriately, as I have done."

    init-vfiles yes
    local i res=()
    for i in "$vfiles[@]" ; do
        res+="\"$(<<<$i sd "$HOME" '~')\""
    done
    ec-copy "${(u@F)res}"
}
