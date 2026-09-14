# -*- mode: sh; sh-shell: zsh; -*-
### tmux session helpers and the `z'-aware launchers, shared between the local
### stack and minimal remote setups.
###
### Extracted from =zshlang/auto-load/others/tmux.zsh=, which kept the only
### copy while =setup/minimal_proxy/.shared.sh= carried a second, drifted one:
### its `tmux-ensure-attach' still used `tmux attach -t', which refuses to nest
### and is exactly what [agfi:tmux-session-goto] exists to avoid.
###
### Dependencies: =zshlang/basic/basic.plugin.zsh=.

function tmux-session-id {
    : "prints the tmux session id of the session named <1>; an id passes through"
    #: A session *name* is not a safe tmux target. A leading sigil declares a
    #: target type -- `$' session, `@' window, `%' pane -- and `=' (exact name
    #: match) disarms that only for session-typed targets: `list-panes'
    #: resolves a *window* even under `-s', and would want a trailing `:' too.
    #: An id is unambiguous in every target position, and it survives a rename
    #: landing mid-operation, which the agent autoname hooks make routine.
    #: See =docs/tmux-session-rename.md=.
    ##
    local session="${1}"
    assert-args session @RET

    if [[ "${session}" =~ '^\$[0-9]+$' ]] ; then
        ec "${session}"
        return 0
    fi

    local line
    for line in "${(@f)$(command tmux list-sessions -F '#{session_id}'$'\t''#{session_name}' 2>/dev/null)}" ; do
        if [[ "${line#*$'\t'}" == "${session}" ]] ; then
            ec "${line%%$'\t'*}"
            return 0
        fi
    done

    ecerr "$0: no tmux session named: ${session}"
    return 1
}

function tmux-session-name-of {
    : "prints the session name behind a tmux target (a session id, a pane, ...)"
    local target="${1}"
    assert-args target @RET

    command tmux display-message -p -t "${target}" '#{session_name}'
}

function tmux-session-goto {
    : "goes to a tmux session: attaches from outside tmux, switches the client from inside"
    #: `attach-session' refuses to nest, so a bare `tmux a -t' run from inside
    #: tmux only ever prints "sessions should be nested with care, unset $TMUX
    #: to force". The verb for a client that already exists is
    #: `switch-client', which resolves that client from `$TMUX'. A shell with
    #: no client at all -- an agent's, a `run-shell' -- has nothing to switch
    #: and says so, which is still a better answer than the nesting refusal.
    ##
    local session="${1}"
    assert-args session @RET

    local target
    target="$(tmux-session-id "${session}")" @RET

    if isTmux ; then
        command tmux switch-client -t "${target}"
    else
        command tmux attach-session -t "${target}"
    fi
}

function tmux-alive-p {
    local session="${1}"
    assert-args session @RET

    local tmux_target
    #: Resolving the id doubles as the existence check.
    tmux_target="$(tmux-session-id "${session}" 2>/dev/null)" || return 1

    local pane_dead_values
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
    tmux-session-goto "${session}"
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
    #: `isBorg' lives in =conditions-personal.zsh=, which no plugin loads; a
    #: plugin-only shell is by definition not Borg.
    if (( ${+functions[isBorg]} )) && isBorg ; then
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
