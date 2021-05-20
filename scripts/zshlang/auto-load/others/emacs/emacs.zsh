##
# export EMACS_SOCKET_NAME=/tmp/sockets/.emacs
export EMACS_SOCKET_NAME="${HOME}/tmp/.emacs-servers/server"
##
function emc-sudo() {
    : "See also doom--sudo-file-path"

    local f
    f="$(grealpath -e ${1} | gtr '"' '\"')" @TRET
    revaldbg emcnw -e '(find-file "/sudo::'$f'")'
}
##
function doom-sync() {
    : "-u updates as well"

    rust-setup
    doom sync "$@"
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
    fnswap isI true tty-title emacs
    local my_term="$TERM"
    if isKitty || isiTerm ; then
        my_term='xterm-24bits'
    fi
    TERM="$my_term" $proxyenv emacsclient -t "$@"
}
function emc-eval() {
    # https://emacs.stackexchange.com/questions/28665/print-unquoted-output-to-stdout-from-emacsclient?noredirect=1&lq=1

    local cmd stdin="${emc_eval_in}"

    cmd="(progn
     (require 'server)
     (princ
       (format \"%s\\n\"
         (server-eval-at (concat (getenv \"EMACS_SOCKET_NAME\")) '(with-current-buffer (window-buffer (selected-window))
                                         "${*}")
            )
         )
       )
     )"

    if bool "$stdin" ; then
        ecgray "$0: stdin does NOT work."

        cmd="
  (let ((lines '())
        this-read)
    (while (setq this-read (ignore-errors
                             (read-from-minibuffer \"\")))
      (setq lines (cons this-read lines)))

      ${cmd}
     )"
    fi

    revaldbg emacs --batch --eval "$cmd"

    ## tests:
    # `fd --extension org --type f . "$nightNotes" | emc_eval_in=y dbg emc-eval '(z arrN (identity lines))'`
    # so the stdin module doesn't work :( I guess lines is not passed to the emacs  server?
    ##
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
function emc-nowait() {
    emc-gateway --no-wait "$@"
    emc-focus
}
alias emcnw='emc-nowait'

ialias emcg="emacsclient -c"
##
function emc-in() {
    local s="$1"

    local t
    t="$(gmktemp --suffix "$s")" @TRET
    cat > "$t" @TRET
    emc-open "$t"
    emc-colorize
}
function emc-colorize() {
    emc-eval "(when (equalp major-mode 'fundamental-mode) (xterm-color-colorize-buffer) (set-buffer-modified-p nil) (read-only-mode))"
    # xterm-color-colorize-buffer eats the ANSI codes, so if we save the file those codes will be LOST
    #
    # this whole command is a raceCondition but it should be harmless
}
##
function icat-emc() {
    icat "$(emc-buffer-file-name)"
}
alias icc='icat-emc'
##
function emc-quote() {
    local i res=''
    for i in "$@" ; do
        res+=" \"$(ecn "$i" | sdlit '\' '\\' | sdlit '"' '\"')\" " @TRET
    done

    ecn "$res"
    ## perf:
    # `time2 emc-quote "${(@f)$(fd --extension org --type f . "$nightNotes")}"` -> 18.4s
    # @todo0 rewrite this in a @perf lang
    # @alt if we add --add-postfix to prefixer, we can simply use a pipeline to do this and avoid zsh. It should be fast enough.
    # NO @alt use emc-eval with stdin
    ##
}
##
function emc-nowait2() {
    local f="$1"
    assert-args f @RET
    
    emc-eval "(find-file $(emc-quote "$f"))"
    # throws useless error 'Invalid read syntax: "#"', but works anyway

    emc-focus
}
##
function emc-less() {
    local fs=( $@ ) jq_force="$emc_less_jq"
    if (( $#@ == 0 )) ; then
        fs+=/dev/stdin
    fi

    local f
    for f in $fs[@] ; do
        if [[ "$f" == *.json ]] || bool "$jq_force" ; then
            cat "$f" | jq .
        else
            cat "$f"
        fi
    done | emc-in
}
alias el="emc-less"

function emc-less-jq() {
    @opts jq y @ emc-less "$@"
}
alias elj="emc-less-jq"
##
function file-uri2unix() {
    local f="$1"
    # assert-args f @RET

    if [[ "$f" =~ 'file://[^/]*(/.*)' ]] ; then
        ec "$match[1]" | url-decode.py
    else
        ec "$f"
    fi
}
function trimr-hash() {
    local inargs
    in-or-args2 "$@"

    arrN "${inargs[@]}" | command sd '([^#]*)#.*' '$1'
}
##
function emc-html-viewer() {
    local f="$1"
    assert-args f @RET

    f="$(file-uri2unix "$f")" @TRET
    local tmp="$(gmktemp --suffix .org)"
    assert html2org "$f" > $tmp @RET
    emc "$tmp"
}
##
##
