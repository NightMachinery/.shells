##
function emc-sudo() {
    : "See also doom--sudo-file-path"

    local f
    f="$(grealpath -e ${1} | gtr '"' '\"')" @TRET
    revaldbg emcnw -e '(find-file "/sudo::'$f'")'
}
##
function doom-sync() {
    rust-setup
    doom sync
}
##
function emcpe() {
    bella_zsh_disable1=y
    local fz_opts=( $fz_opts[@] -1 )
    # ffkill -SIGUSR2 \'emacs \'daemon
    ffkill -SIGUSR2 emacs daemon
    emacsclient -e '(setq debug-on-quit nil)'
}
function emn() {
    bella_zsh_disable1=y
    emc-gateway -e '(helm-man-woman "")' # can't input to helm using its arg. why?
    #"(woman \"$*\")"
}
## did not work
# function emc-openclose() {
#     local f="$1"
#     emc-gateway -e "(progn (find-file ${f:q}) (save-buffers-kill-terminal)"
#     # { sleep 0.1 ; emc-gateway -e '(save-buffers-kill-terminal)' } &
#     # emc-gateway "$f"
#     reset
# }
# reify emc-openclose
# function emacs-import-vfiles() {
#     init-vfiles yes
#     emc-openclose "${vfiles[@]}"
# }
##
function emacs-vfiles() {
    : "Paste the result into the recentf list of emacs (For me at: ~/.emacs.d.doom/.local/cache/recentf). You'll need to have set the max limit appropriately, as I have done."

    init-vfiles yes
    local i res=()
    for i in "$vfiles[@]" ; do
        res+="\"$(<<<$i sd "$HOME" '~')\""
    done
    ec-copy "${(u@F)res}"
}
##
aliasfn emc-open bicon-emc
alias emc='emc-open'
function emc-gateway() {
    bella_zsh_disable1=y
    tty-title emacs
    local my_term="$TERM"
    if isKitty || isiTerm ; then
        my_term='xterm-24bits'
    fi
    TERM="$my_term" $proxyenv emacsclient -t "$@"
}
function emc-eval() {
    # https://emacs.stackexchange.com/questions/28665/print-unquoted-output-to-stdout-from-emacsclient?noredirect=1&lq=1
    revaldbg emacs --batch --eval "(progn
     (require 'server)
     (princ
       (format \"%s\\n\"
         (server-eval-at \"server\" '(with-current-buffer (window-buffer (selected-window))
                                         "${*}")
            )
         )
       )
     )"
}
aliasfnq emc-buffer-file-name emc-eval "(buffer-file-name)"
function emc-sourceme() {
    local f
    f="$(emc-buffer-file-name)" @RET
    if [[ "$f" == *.(zsh|bash) ]] ; then
        NIGHT_NO_EXPENSIVE='' reval-ec source "$f"
    else
        ecerr "$0: file doesn't seem suitable: $f"
    fi
}
function emc-focus() {
    if isSSH ; then
        return 0
    fi

    if isKitty ; then
        kitty-emacs-focus
    else
        terminal-activate-tab 5
    fi
    bella_zsh_disable1=y
}
function emcnw() {
    emc-gateway --no-wait "$@"
    emc-focus
}
ialias emcg="emacsclient -c"
##
