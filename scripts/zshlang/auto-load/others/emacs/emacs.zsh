##
function emc-sudo() {
    : "See also doom--sudo-file-path"

    local f
    f="$1"

    revaldbg emc-eval '(doom/sudo-find-file '"$(emc-quote "$f")"')' @RET
    ##
    # f="$(grealpath -e -- ${f} | gtr '"' '\"')" @TRET
    # revaldbg emcnw -e '(find-file "/sudo::'$f'")'
}
##
function doom-sync {
    #: -u: Update all installed packages after syncing.
    #: -U: Don't update any packages.
    #: -!, --force: Suppress prompts by auto-accepting their consequences.

    rust-setup
    doom sync -U "$@"
}
##
function emcpe {
    local opts=()
    if (( ${#@} >= 1 )) ; then
        opts=("$@")
    else
        opts=(-SIGUSR2)
    fi

    bella_zsh_disable1
    local fz_opts=( $fz_opts[@] -1 )
    # ffkill -SIGUSR2 \'emacs \'daemon
    if emc-gui-p ; then
        ffkill "${opts[@]}" 'Emacs.app/Contents/MacOS/Emacs' '!daemon' '!batch' '!zsh' '!alt' '!-nw'
    else
        ffkill "${opts[@]}" emacs daemon '!alt'
    fi

    # emacsclient -e '(setq debug-on-quit nil)'
    emc-eval '(setq debug-on-quit nil)'
}
alias pe='emcpe'
alias pe2='redo2 10 reval-timeout 1 emcpe'

function emc-kill {
    ffkill -9 emacs daemon '!alt' '!emacsclient'
}
alias pek='emc-kill'
##
function emn() {
    bella_zsh_disable1
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
function emacs-vfiles {
    : "Paste the result into the recentf list of emacs (For me at: ~/.emacs.d/.local/cache/recentf). You'll need to have set the max limit appropriately, as I have done."

    init-vfiles yes
    local i res=()
    for i in "$vfiles[@]" ; do
        res+="\"$(<<<$i sd "$HOME" '~')\""
    done
    ec-copy "${(u@F)res}"
}
##
function emc-open {
    #: @seeAlso [agfi:bicon-emc]
    ##
    emc-nowait2 "$@"
}

alias emc='emc-open'
alias emcg='withemcgui emc-open'

function emc-gateway {
    bella_zsh_disable1

    retry ensure-redis @RET

    local emc_engine="${emc_gateway_engine}"
    if test -z "${emc_engine}" ; then
        emc_engine=(emacsclient -t)
    fi

    local title=emacs
    if test -n "$emacs_night_server_name" ; then
        title="${emacs_night_server_name:t}"
        if [[ "$title" == 'server_alt1' ]] ; then
            title="IRC"
        fi

    fi
    if test -z "$title" ; then
        title='emc'
    fi
    if test -z "${emacs_night_server_name}" && test -n "${EMACS_SOCKET_NAME}" ; then
        local -x emacs_night_server_name="${EMACS_SOCKET_NAME}"
    fi

    tty_title_f=y tty-title "$title"

    local my_term="$TERM"
    #: xterm-emacs is our 24-bit entry (setup/terminfo-24bit.src). The real
    #: condition is "does this terminal do truecolor"; kitty and iTerm were only
    #: standing in for it. Spelling it out also covers Termux, which reports an
    #: honest xterm-256color and would otherwise lose the entry.
    if true-color-p ; then
        my_term='xterm-emacs'
    fi

    reval-ec-env \
        TERM="$my_term" \
        LOGNAME="$(whoami)" \
        KITTY_WINDOW_ID="${KITTY_WINDOW_ID}" \
        $proxyenv \
        reval "${emc_engine[@]}" "$@"
}

function emc-mobile {
    #: Shares the ordinary daemon by default.
    #:
    #: Emacs 29.2 -- and master, checked against the source -- leaks minibuffer
    #: depth when one terminal tries to prompt while another holds a minibuffer
    #: read: read_minibuf increments minibuf_level, then
    #: temporarily_switch_to_single_kboard can signal "Terminal N is locked,
    #: cannot read from it" before read_minibuf_unwind, the only thing that
    #: decrements it, is registered. recursion-depth then only climbs and no
    #: Lisp call repairs it, so the daemon must be restarted.
    #: @see ~/scripts/docs/emacs-minibuffer-wedge.md
    #:
    #: A separate daemon would bound what that costs, since mobile ssh sessions
    #: are the ones that drop and reconnect. It is off by default because
    #: restarting is cheap, and the split is not free: separate buffers,
    #: registers and session state, plus a full Doom startup on first launch.
    #: Set =emc_mobile_own_daemon_p=y= if a wedge ever costs more than a
    #: restart is worth.
    ##
    if bool "${emc_mobile_own_daemon_p}" ; then
        local socket="${EMACS_MOBILE_SOCKET_NAME:-${EMACS_SOCKET_NAME:h}/server_mobile}"
        local -x EMACS_SOCKET_NAME="${socket}"
        local -x emacs_night_server_name="${socket}"
        #: Empty ALTERNATE_EDITOR makes emacsclient start the daemon when this
        #: socket has none yet; the first launch pays a full Doom startup.
        local -x ALTERNATE_EDITOR=""
    fi

    TERM=xterm-emacs emc-gateway --frame-parameters '((night/mobile . t))' "$@"
}

function emc-mobile-tmux {
    : "runs [agfi:emc-mobile] in a tmux session named emacs-mobile, attaching to it if it already exists"
    #: WHY tmux, when mosh already survives a roaming connection: with
    #: `emacsclient -t' the frame's controlling tty IS the ssh session, so a
    #: dropped link leaves the daemon holding a frame on a terminal that no
    #: longer exists -- the class of state behind
    #: =docs/emacs-minibuffer-wedge.md=. Hosting the frame in a tmux pane puts
    #: a tty in front of it that outlives the network, so the connection can
    #: come and go under a frame that stays valid, and the same frame can be
    #: picked up again from another device.
    #:
    #: Quitting the frame lets the pane exit. Because =~/.tmux.conf= sets
    #: `remain-on-exit on' globally the session then lingers holding a DEAD
    #: pane, which [agfi:tmux-alive-p] reports as not alive, so the next call
    #: tears it down and opens a fresh Emacs rather than attaching you to a
    #: corpse. Verified, since the alternative -- `duplicate session' from
    #: [agfi:tmuxnew] -- is what happens if that teardown ever stops working.
    #:
    #: A *failing* launch is the case worth keeping a pane for: a session that
    #: died instantly would otherwise surface only as `no tmux session named:
    #: ...' from [agfi:tmux-session-goto], with the real error already gone
    #: with the session.
    #:
    #: End it by quitting the frame from inside Emacs (`SPC q f'), not with
    #: `tmux kill-session': killing the session leaves the daemon holding a
    #: live frame on a pty that no longer exists, which is the very state this
    #: function exists to avoid. Recover from one with
    #: =(delete-frame F t)= via [agfi:emc-eval].
    #:
    #: @usage emc-mobile-tmux            #: attach, creating the session if needed
    #: @usage emc-mobile-tmux file.org   #: file args are honoured only on creation
    ##
    ensure-cmd tmux @RET

    local session="${emc_mobile_tmux_session:-emacs-mobile}"

    #: [agfi:tmux-ensure-attach] ignores the command when the session is
    #: already alive, so file arguments would otherwise vanish silently.
    if (( $# )) && tmux-alive-p "${session}" ; then
        ecerr "$0: ${session} is already running; NOT opening: $*"
        ecerr "$0: open them from inside Emacs, or quit the frame first."
    fi

    #: Only a launch that fails FAST keeps its pane. Emacs quitting normally
    #: must let the pane exit, or the next call would attach to a stale shell
    #: instead of opening a frame. And `tmux kill-session' on a running
    #: session counts as a failure too, so without the elapsed-time test that
    #: shell would survive its own destroyed pty as an orphan -- measured,
    #: not hypothetical.
    local grace="${emc_mobile_tmux_grace_seconds:-10}"

    #: `emc-mobile' is a zsh function, and tmux execs a multi-argument
    #: shell-command directly rather than through a shell, so it has to be
    #: wrapped -- the same shape [agfi:tma-z] and [agfi:tmuxnewsh] use.
    tmux-ensure-attach "${session}" \
        zsh -c "start=\$SECONDS ; FORCE_INTERACTIVE=y emc-mobile $(gq "$@") ; rc=\$? ; (( rc == 0 || SECONDS - start >= ${grace} )) && exit \$rc ; ecerr 'emc-mobile-tmux: emc-mobile failed in under ${grace}s; keeping this pane so the error stays readable' ; exec zsh"
}

function emc-open-no-server {
    emc_gateway_engine=(emacs) emc-gateway "$@"
}
aliasfn emc-open-no-server-tui emc-open-no-server -nw

function emc-eval {
    # https://emacs.stackexchange.com/questions/28665/print-unquoted-output-to-stdout-from-emacsclient?noredirect=1&lq=1

    local cmd stdin="${emc_eval_in}"

    local -x LANG=en_US.UTF-8
    #: To avoid this warning:
    #: `LANG=en_US@calendar=persian.UTF-8 cannot be used, using en_US.UTF-8 instead.`

    cmd="(progn
     (require 'server)
     (let* (
       (server-name (concat (getenv \"EMACS_SOCKET_NAME\")))
       ;; My HEAD emacs server-eval-at is ignoring its server input and using the server-name variable instead. This should work with both the buggy and the correct emacs versions.
      )
     (princ
       (format \"%s\\n\"
         (server-eval-at server-name '(with-current-buffer (window-buffer (selected-window))
                                         "${*}")
     )
         )
)
))"

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

function emc-sourceme {
    local f
    f="$(emc-buffer-file-name)" @RET
    if source-suitable-p "$f" ; then
        NIGHT_NO_EXPENSIVE='' reval-ec source "$f"
    else
        ecerr "$0: file doesn't seem suitable: $f"
    fi
}

function emc-gui-p {
    [[ "$EMACS_SOCKET_NAME" == "$EMACS_GUI_SOCKET_NAME" ]]
}

function emc-focus {
    if isSSH ; then
        return 0
    fi

    if emc-gui-p ; then
        emc-focus-gui
        return $?
    fi

    if isKitty ; then
        kitty-emacs-focus
    else
        terminal-activate-tab 5
    fi
    bella_zsh_disable1
}

function emc-focus-gui {
    if isDarwin ; then
        # if emc-gui-p ; then
        #     #: called by a GUI emacs instance, so emacs is most probably still in focus
        #     #: This is a hack because =frontapp-get= has stopped working.

        #     return 0
        # fi

        if [[ "$(frontapp-get)" =~ '(?i).*emacs.*' ]] ; then
            #: already in focus

            return 0
        else
            reval-ec hammerspoon -c "toggleFocus(emacsAppName)"
            return $?
        fi
    else
        return 1
    fi
}

function emc-nowait {
    @deprecated # emc-nowait2

    emc-gateway --no-wait "$@"
    emc-focus
}
alias emcnw='emc-nowait2'

# ialias emcg="emacsclient -c"
##
function emc-in {
    local s="${1:-.log}"

    local t
    t="$(gmktemp --suffix "$s")" @TRET
    cat > "$t" @TRET

    # test -s "$t" @TRET #: Ensures the input is not empty.

    emc-open "$t"
    # emc-colorize
}

function emc-colorize {
    emc-eval "(when t ;; (equalp major-mode 'fundamental-mode)
 (xterm-color-colorize-buffer) (set-buffer-modified-p nil) (read-only-mode))"
    # xterm-color-colorize-buffer eats the ANSI codes, so if we save the file those codes will be LOST
    #
    # this whole command is a raceCondition but it should be harmless
}
##
function icat-emc {
    icat "$(emc-buffer-file-name)"
}
# alias ice='icat-emc'
##
function lisp-quote {
    ##
    in-or-args "$@" |
        lisp-quote.lisp |
        cat-copy-if-tty
    ##
    # local i res=''
    # for i in "$@" ; do
    #     res+=" \"$(ecn "$i" | sdlit '\' '\\' | sdlit '"' '\"')\" " || {
    #         ectrace "i: $(gq "$i")"
    #         return 1
    #     }
    # done

    # ecn "$res"
    ## perf:
    # `time2 emc-quote "${(@f)$(fd --extension org --type f . "$nightNotes")}"`
    # v1 -> 18.4s
    # v2 (CL) -> 0.099549055099487305s
    ##
}
aliasfn emc-quote lisp-quote

function lisp-quote-all {
    cat-paste-if-tty | in-or-args "$@" | lisp-quote-all.lisp | cat-copy-if-tty
}

function lisp-quote-safe {
    # slower than `lisp-quote', but preserves newlines in arguments
    ##
    local arg
    for arg in "$@" ; do
        ecn "$arg" | lisp-quote-all.lisp
    done | cat-copy-if-tty
}
aliasfn emc-quote-safe lisp-quote-safe
##
function emc-nowait2 {
    local f=("$@") cmd="${emc_nowait2_cmd:-find-file}"
    assert-args f @RET
    local colorize_p="${emc_nowait2_colorize_p}"
    local other_commands=""

    if bool "${colorize_p}"; then
        other_commands+='(xterm-color-colorize-buffer)'

        if [[ "${colorize_p}" == 'non-modified' ]] ; then
            other_commands+='(set-buffer-modified-p nil)'
        elif [[ "${colorize_p}" == 'read-only' ]] ; then
            other_commands+='(set-buffer-modified-p nil) (read-only-mode)'
        fi
    fi

    ## @redundant
    # local tmp
    # tmp="$(serr grealpath -e -- "$f")" && f="$tmp" || true # can be, e.g., an scp path
    ##
    
    #: `vlf-application' is bound because its default, `ask', means "prompt when
    #: the file is over `large-file-warning-threshold'" --- 50MB in night-config.el
    #: --- and this reaches emacs through `server-eval-at', with nobody there to
    #: answer. The prompt then blocks for ever rather than failing: one 108MB org
    #: file sat on it for three hours, looking exactly like a hung conversion.
    #: `dont-ask' uses vlf for those files instead, which is the only practical
    #: way to read one anyway --- plain `find-file' on 108MB of org is not
    #: something emacs finishes either.
    #:
    #: night-vlf.el sets this too. Bound here as well because that is the config
    #: of one machine and this is the code path: a host whose doom config
    #: predates it, or an emacs started before it, must not hang.
    revaldbg emc-eval "(let ((default-directory $(emc-quote "$PWD")) (vlf-application 'dont-ask)) (mapc #'${cmd} '($(emc-quote "${f[@]}"))) ${other_commands} t)"
    # throws useless error 'Invalid read syntax: "#"', but works anyway

    emc-focus
}

function emc-literally {
    #: This is useful for opening big files.
    ##
    @opts cmd find-file-literally @ emc-nowait2 "$@"
}
aliasfn emc-big-files emc-literally
##
function emc-less {
    local fs=( $@ ) jq_force="$emc_less_jq" parser="$emc_less_parser" suffix="${emc_less_s}"
    if (( $#@ == 0 )) ; then
        if isInTty ; then
            local tmp
            tmp="$(mktemp)" @TRET
            cat-paste-if-tty > "$tmp" @RET
            fs+="$tmp"
        else
            fs+=/dev/stdin
        fi
    fi

    local f
    for f in $fs[@] ; do
        if [[ "$f" == *.json ]] || bool "$jq_force" ; then
            cat "$f" | jq .
        else
            cat "$f"
        fi
    done | sponge | {
        if test -n "$parser" ; then
            # assert prettier --parser "$parser"
            assert unibeautify -l "$parser" -f
        else
            cat
        fi
    } | emc-in "$suffix"
}
alias el="emc-less"

function emc-less-jq() {
    @opts jq y s .json @ emc-less "$@"
}
alias elj="emc-less-jq"


function emc-less-org {
    @opts s .org @ emc-less "$@"
}
alias elo="emc-less-org"

function emc-less-md {
    @opts s .md @ emc-less "$@"
}
alias elmd="emc-less-md"

function emc-less-html() {
    # @opts parser html s .html @ emc-less "$@"
    @opts parser HTML s .html @ emc-less "$@"
}
alias elh="emc-less-html"
##
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
function undo-fu-cache-clear {
    #: @alt [help:undo-fu-clear-all] (in the latest version)
    ##
    trs ~/.emacs.d/.local/cache/undo-fu-session/*(DN)
}
##
