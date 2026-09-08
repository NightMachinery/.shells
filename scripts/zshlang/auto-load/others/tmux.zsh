##
function tmux-client-terminal-get {
    #: This dynamically returns the TERM of the terminal that the current session is attached to.
    #: I.e., if you attach to the same session using two different clients, it will return two different values.
    ##
    tmux display-message -p '#{client_termname}' 2>/dev/null
}

function tmux-client-termtype-get {
    #: The terminal's own answer to tmux's XTVERSION query, e.g. `kitty(0.48.2)`.
    #: Unlike [agfi:tmux-client-terminal-get], TERM cannot spoof this.
    #: Empty on tmux <3.3 (the format does not exist) and on terminals that stay
    #: silent, which is why the callers must treat empty as "not that terminal".
    ##
    tmux display-message -p '#{client_termtype}' 2>/dev/null
}

function tmux-client-termtype-supported-p {
    #: Memoized: [agfi:isKitty] sits on the hot path of every [agfi:colorfg] call
    #: through [agfi:true-color-p], so this must not fork on each invocation.
    ##
    if test -n "${tmux_client_termtype_supported_p}" ; then
        bool "${tmux_client_termtype_supported_p}"
        return $?
    fi

    local v ver='' res=n
    #: `tmux -V` prints e.g. `tmux 3.6a` or `tmux next-3.7`.
    v="$(tmux -V 2>/dev/null)"
    if [[ "$v" =~ '([0-9]+\.[0-9]+)' ]] ; then
        ver="$match[1]"
    fi
    if test -n "$ver" && is-at-least 3.3 "$ver" ; then
        res=y
    fi

    typeset -g tmux_client_termtype_supported_p="$res"
    bool "$res"
}
##

function tmux-alive-p {
    local session="${1}"
    assert-args session @RET

    local tmux_target="=${session}"
    local pane_dead_values

    if ! tmux has-session -t "${tmux_target}" &>/dev/null ; then
        return 1
    fi

    pane_dead_values=("${(@f)$(tmux list-panes -t "${tmux_target}" -F '#{pane_dead}')}" ) @RET

    local pane_dead
    for pane_dead in "${pane_dead_values[@]}" ; do
        if [[ "${pane_dead}" == "0" ]] ; then
            #: at least one pane is not dead
            return 0
        fi
    done

    return 1
}

function tmuxnew-ensure {
    local session="${1}"

    if tmux-alive-p "${session}" ; then
        ecgray "$0: tmux session ${session} already exists"

        return 0
    else
        tmuxnew "$@"
    fi
}

function tmux-ensure-attach {
    local session="${1}"
    assert-args session @RET
    shift
    local command=("${@:-zsh}")

    tmuxnew-ensure "${session}" "${command[@]}"
    tmux attach -t "${session}"
}
alias tma='tmux-ensure-attach'

function tma-z {
    local name="${*}"
    assert-args name @RET

    tmux-ensure-attach "${name}" zsh -c "cd ~/ && FORCE_INTERACTIVE=${TMA_Z_FORCE_INTERACTIVE:-y} z $(gq ${name%-*}) && ZSH_PWD=MAGIC_KEEP_CURRENT exec zsh"
    #: `%-*`: remove last dash and everything after it
}
##
function tmuxnewsh {
    #: [agfi:tmuxnewsh2]
    ##

    local env="${tmuxnewshenv[*]}"
    local proxy_forward_p="${tmuxnewsh_proxy_forward_p:-n}"
    if bool "$proxy_forward_p" ; then
        env=(
            proxy_disabled="${proxy_disabled}"
            all_proxy="${all_proxy}"
            ALL_PROXY="${ALL_PROXY}"
            http_proxy="${http_proxy}"
            https_proxy="${https_proxy}"
            HTTP_PROXY="${HTTP_PROXY}"
            HTTPS_PROXY="${HTTPS_PROXY}"
            "${env}"
        )
    fi

    local tmux_pwd="$PWD"
    if isBorg ; then
        tmux_pwd="${HOME}/tmp"
        mkdir-m "${tmux_pwd}"

        #: In Borg, the current PWD will be deleted after command execution. This can be hazardous to the processes running in tmux.
    fi

    revaldbg tmuxnew "$1" "$(gq zsh -c "cd $(gq ${tmux_pwd}) && FORCE_INTERACTIVE=y ${env[*]} $(gq "${@[2,-1]}")")"
}

function tmuxnewsh2 {
    #: Supports simple env vars automatically
    ##

    local name="$1"
    shift
    local env=()
    local i
    for i in "$@" ; do
        if [[ "$i" =~ '^([^=]*)=(.*)$' ]] ; then
            env+="$match[1]=$(gq "$match[2]")"
            shift
        else
            break
        fi
    done
    ## debug:
    # arger "$@"
    # ec "$env[*]"
    # typ env
    ##

    tmuxnewshenv="${tmuxnewshenv[*]} $env[*]" tmuxnewsh "$name" "$@"
}
aliasfn tsh tmuxnewsh2

function tmuxdaemon() {
    local key="$tdKey"
    local cmd="$(gq "$@")"
    local name="$cmd ___ ${tdkey}"
    name="$name $(md5m "$name")"
    name="$(<<<$name str2tmuxname)"
    # perhaps we should ask the user to confirm if a duplicate session with this name already exists?
    silent tmux kill-session -t "$name"  && ecerr "Killed already existing session '$name' to run the new command '$cmd'" # is killing it redundant?
    tmuxnewsh2 "$name" "$@" && ec "Created session '$name'"
}
aliasfn tshd tmuxdaemon
##
function tmuxzombie-ls() {
    tmux list-panes -a -F "#{pane_dead} #{pane_id} #{session_name}" \
        > >(gawk '/^1/ { print $2 }') \
        > >(perl -ne 's/^1\s+\S+\s+(.*)$/$1/ && print' >&2)
}
aliasfn tzls tmuxzombie-ls

function tmuxzombie-kill {
    local fd1
    {
        exec {fd1}>&1 # to output to the original stdout
        tmuxzombie-ls >&$fd1 | inargsf re 'tmux kill-pane -t'
    } always { exec {fd1}>&- }
}
aliasfn tzkill tmuxzombie-kill

function tmuxzombie() {
    # kills the pane of a session, thus turning it to a "dead pane"
    tmux list-panes  -s -F '#{pane_pid}' -t "$1" | inargsf serr kill
}
##
function str2tmuxname() {
    # this might be too restrictive
    in-or-args "$@" |
        gtr -cd ' [a-zA-Z0-9]_-'
}

function str2filename {
    #: This function might be too restrictive ...
    ##
    in-or-args "$@" |
        gtr '/' '_' |
        gtr -d ':?/\\~!@#$%^&*|<>\000'$'\n\t' | #: `+` is okay
        trimsed |
        cat-copy-if-tty
}

function str2filename-ascii {
    str2filename "$@" |
        utf8-to-ascii
}
##
function tmux-capture {
    local target="${1}"
    assert-args target @RET

    local limit="${2}" #: empty limit seems to mean return everything

    revaldbg tmux capture-pane -p -S -"$limit" -t "$target"
}

aliasfn tcgar tmux-capture BrishGarden
##
alias t.hv='tmux new-session \; split-window -h \; split-window -v \; attach'

function ivy-tty-title-get {
    #: The label the ivy tab carries. Override with `ivy_tty_title'.
    ##
    local tty_title="${ivy_tty_title}"
    if test -z "$tty_title" ; then
        # tty_title="🪴"
        #: doesn't display correctly
        #:  - [jalali:1404/07/06/01:33]

        # tty_title="🌿"

        tty_title="🌳"

        # tty_title="ivy"
    fi

    ec "${tty_title}"
}

function ivy-tmux-title-set {
    #: `set-titles-string' is a *session* option, so this pins ivy's tab label
    #: without touching any other session. It is needed because the global
    #: string follows the active pane, and ivy's panes are mpv and friends --
    #: which would leave the tab reading whatever is playing rather than the
    #: label [agfi:ivy] sets with [agfi:tty-title] just before attaching.
    #: See =./docs/tmux-tty-title.md=.
    #:
    #: Set at session creation, so it covers every later attach and not just
    #: the one [agfi:ivy] performs itself.
    ##
    if ! silent tmux has-session -t '=ivy' ; then
        ecerr "$0: no ivy session"
        return 1
    fi

    #: `#' opens a format substitution in a tmux option; `##' is a literal one.
    local tty_title="$(ivy-tty-title-get)"

    #: No `=' exact-match prefix here, unlike the =has-session= above:
    #: =set-option= rejects it outright (`no such session: =ivy'). The bare
    #: name is safe anyway, because the exact check has already run and tmux
    #: resolves an exact session name before trying it as a prefix.
    tmux set-option -t 'ivy' set-titles-string "${tty_title//\#/##}"
}

function ivy {
    ###
    local tty_title="$(ivy-tty-title-get)"

    tty-title "${tty_title}"
    ###
    ## ivy acts as the terminal emulator's startup hook, as well
    phoenix-reload
    ##
    if ! whitespace-is "$(pgrep tmux)" ; then
        if ! ask "$0: tmux seems to be running already; Proceed?" N ; then
            ivy-tmux-title-set
            tmux attach -t ivy
            return 0
        fi
    fi

    various-darwin.zsh

    ivy-convenience

    ivy-self
    #: attaches and blocks, I think
}

function ivy-self {
    local -x DISABLE_DEFER=y

    tmux kill-session -t "ivy" &> /dev/null
    tmux new-session -s ivy -d 'zsh'
    ivy-tmux-title-set

    #: We add a space before our commands to avoid cluttering the shell history.
    tmux send-keys " hear-start-server "$'\n'
    # tmux send-keys " muc "
    #: 'mu' could also download, but it needs to be updated

    tmux split-window -h  'zsh'
    tmux send-keys " lunas "$'\n'
    tmux split-window -v 'zsh'
    # tmux split-window -v 'salice.py'
    tmux select-pane -t 0
    comment order matters. Select a pane before attaching.
    comment '-2            Force tmux to assume the terminal supports 256 colours.'
    tmux -2 attach-session -d
}

function ivy-convenience {
    local i

    # for i in {1..3} ; do
    #     tmuxnewsh2 "zii$i" mosh zii@51.178.215.202 -- /home/linuxbrew/.linuxbrew/bin/zsh
    # done

    for i in {1..2} ; do
        tmuxnewsh2 "eva$i" mosh ${lilf_user}@82.102.11.148 -- zsh
    done

    for i in {1..1} ; do
        tmuxnew "julia_repl$i" env TERM="$TERM" PATH="$PATH" julia
    done


    for i in {1..1} ; do
        tmuxnew "ipython_repl$i" ipython
    done
    ##
    # tmuxnew ipython_p310 conda run --no-capture-output --live-stream -n p310 ipython
    ##
}
##
function tmux-attach {
    local session="$1"
    assert-args session @RET

    tty-title "${session}"
    tmux a -t "${session}"
}
##
