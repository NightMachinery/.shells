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

#: Where [agfi:h-tmux-z-tag] reads its words from. Resolved at source time, so
#: the plugin works from any cwd and from a plugin manager's own checkout.
typeset -g tmux_z_words_pretty="${${(%):-%x}:A:h}/words-pretty.txt"
typeset -ga tmux_z_words_pretty_cache=()

#: The tmux session user option carrying a session's tmux-z identity. The
#: session *name* cannot carry it: the agent autoname hooks rename an agent's
#: session at its first prompt (=docs/tmux-session-rename.md=), so
#: `codex-t scripts@main' would otherwise start a second agent in the same
#: directory every time. =agent-tmux.zsh= keeps `@agent_session' for the same
#: reason.
typeset -g tmux_z_key_option='@tz_key'

function h-tmux-z-tag {
    : "sets REPLY to a short, memorable two-word tag for an untagged session"
    #: `$RANDOM', not [agfi:passgen-words]: a session tag is a label to retype,
    #: not a secret. passgen-words draws from =/usr/share/dict/words= (yielding
    #: `paryphodrome-Songo') and costs four forks plus two scans of a
    #: 235k-line file per launch. This forks nothing.
    #:
    #: It returns through REPLY, and must not be called as `$(h-tmux-z-tag)':
    #: `$RANDOM' consumed inside a subshell does not advance the parent's
    #: state, so every call in a retry loop would hand back the same tag. That
    #: is exactly the collision the caller's loop exists to avoid.
    ##
    if (( ${#tmux_z_words_pretty_cache} == 0 )) ; then
        test -e "${tmux_z_words_pretty}" || {
            ecerr "$0: wordlist not found: ${tmux_z_words_pretty}"
            return 1
        }
        tmux_z_words_pretty_cache=( "${(@f)$(<"${tmux_z_words_pretty}")}" )
    fi

    local n="${#tmux_z_words_pretty_cache}"
    (( n >= 2 )) || {
        ecerr "$0: wordlist is too small: ${tmux_z_words_pretty}"
        return 1
    }

    typeset -g REPLY="${tmux_z_words_pretty_cache[RANDOM % n + 1]}-${tmux_z_words_pretty_cache[RANDOM % n + 1]}"
}

function h-tmux-session-by-key {
    : "<key>: prints the id of the tmux session carrying that tmux-z key"
    local key="${1}"
    assert-args key @RET

    local line
    for line in "${(@f)$(command tmux list-sessions -F '#{session_id}'$'\t'"#{${tmux_z_key_option}}" 2>/dev/null)}" ; do
        if [[ "${line#*$'\t'}" == "${key}" ]] ; then
            ec "${line%%$'\t'*}"
            return 0
        fi
    done

    return 1
}

function h-tmux-z-name-sanitize {
    : "makes a tmux-z session name acceptable to tmux"
    #: tmux rejects `.' and `:' (they are target syntax) and reads a *leading*
    #: `@' as window-id syntax. An `@' in the middle is an ordinary character.
    #: [agfi:str2tmuxname] must not be used here: it deletes `@' outright.
    ##
    local name="${1//[.:]/-}"
    ec "${name#@}"
}

function tmuxnewsh2-attach-z {
    : "<query>[@<tag>] [VAR=val ...] [cmd ...]: run a command in a tmux session in the directory z finds, then attach"
    #: Without a tag this always makes a *new* session, named with a generated
    #: one, so two launches in the same directory never collide; the tag it
    #: prints is what you type to come back. With a tag it attaches to the
    #: existing session if there is one.
    ##
    local force_i="${tmuxnewsh2_attach_z_force_interactive:-y}"
    local spec="${1}"
    assert-args spec @RET
    shift

    #: Split on the *last* `@', so `a@b@c' is query `a@b', tag `c'.
    local query="${spec%@*}" tag='' ensure_p=y
    if [[ "${spec}" == *@* ]] ; then
        tag="${spec##*@}"
    fi
    if test -z "${tag}" ; then
        #: A bare `scripts', and a trailing `scripts@', both count as untagged.
        query="${spec%@}"
        ensure_p=n
    fi
    assert-args query @RET

    local -a wanted=( "$@" )
    if (( ${#wanted} == 0 )) ; then
        #: Reproduces the old `tma-z': an interactive shell that stays in the
        #: directory the session was opened in.
        wanted=( ZSH_PWD=MAGIC_KEEP_CURRENT zsh )
    fi

    h-tmuxnewsh2-argv-split "${wanted[@]}" @RET
    local cmd_token="${tmuxnewsh2_split_cmd[1]:-zsh}"

    local key='' target=''
    if bool "${ensure_p}" ; then
        key="${cmd_token}"$'\t'"${query}"$'\t'"${tag}"

        if target="$(h-tmux-session-by-key "${key}")" && tmux-alive-p "${target}" ; then
            ecgray "$0: attaching to existing session, nothing was run"

            tmux-session-goto "${target}"
            return $?
        fi
    else
        #: Regenerate on collision: [agfi:tmuxnew] kills the processes of a
        #: same-named session, so a tag that happens to be taken would destroy
        #: live work.
        local i
        for i in {1..8} ; do
            h-tmux-z-tag @RET
            tag="${REPLY}"
            key="${cmd_token}"$'\t'"${query}"$'\t'"${tag}"
            if ! h-tmux-session-by-key "${key}" >/dev/null 2>&1 ; then
                break
            fi
            key=''
        done
        test -n "${key}" || {
            ecerr "$0: could not find a free tag after 8 tries"
            return 1
        }
    fi

    local name
    name="$(h-tmux-z-name-sanitize "${cmd_token} ${query}@${tag}")" @TRET

    #: Resolve the directory here rather than inside the session: a failed or
    #: cancelled pick then aborts before anything is created, instead of
    #: leaving a session that dies with its error message unread.
    #:
    #: The `cd' is load-bearing. `z' is [agfi:ffz] locally, whose corpus
    #: includes the current directory's subtree, but zoxide on the minimal
    #: servers, which never matches the current directory at all. Reading $PWD
    #: back out of a subshell is the one form both spell the same way.
    local dir
    dir="$(cd "${HOME}" && FORCE_INTERACTIVE="${force_i}" z "${query}" >/dev/null && print -r -- "${PWD}")" @TRET
    assert test -d "${dir}" @RET

    tmuxnewsh_pwd="${dir}" tmuxnewsh2 "${name}" "${wanted[@]}" @RET

    target="$(tmux-session-id "${name}")" @TRET
    command tmux set-option -t "${target}" "${tmux_z_key_option}" "${key}" @RET
    ecgray "$0: ${name}"

    tmux-session-goto "${target}"
}

function h-tmuxnewsh2-attach-z-with-cmd {
    : "<cmd> <query>[@<tag>] [args ...]: [agfi:tmuxnewsh2-attach-z] with the command first"
    #: Argument reordering only, so `aliasfn' can bind a command while the
    #: caller still writes the session spec first: `codex-t scripts --resume'.
    ##
    local cmd="${1}" spec="${2}"
    assert-args cmd spec @RET
    shift 2

    tmuxnewsh2-attach-z "${spec}" "${cmd}" "$@"
}

aliasfn tz tmuxnewsh2-attach-z
aliasfn tma-z tmuxnewsh2-attach-z
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

    #: `tmuxnewsh_pwd' is what lets a caller put the session somewhere other
    #: than its own cwd; [agfi:tmuxnewsh2-attach-z] uses it to hand over the
    #: directory `z' resolved.
    local tmux_pwd="${tmuxnewsh_pwd:-$PWD}"
    #: An explicit directory wins over the Borg override, which only exists to
    #: keep a session out of a cwd that is about to be deleted.
    #: `isBorg' lives in =conditions-personal.zsh=, which no plugin loads; a
    #: plugin-only shell is by definition not Borg.
    if test -z "${tmuxnewsh_pwd}" && (( ${+functions[isBorg]} )) && isBorg ; then
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

    h-tmuxnewsh2-argv-split "$@"
    ## debug:
    # arger "${tmuxnewsh2_split_cmd[@]}"
    # ec "${tmuxnewsh2_split_env[*]}"
    ##

    tmuxnewshenv="${tmuxnewshenv[*]} ${tmuxnewsh2_split_env[*]}" \
        tmuxnewsh "$name" "${tmuxnewsh2_split_cmd[@]}"
}
aliasfn tsh tmuxnewsh2

function h-tmuxnewsh2-argv-split {
    : "splits leading VAR=val arguments off an argv; sets tmuxnewsh2_split_env and tmuxnewsh2_split_cmd"
    #: Shared by [agfi:tmuxnewsh2] and [agfi:tmuxnewsh2-attach-z], which needs
    #: the same split to learn the command token for the session name. Zsh has
    #: no namerefs, so the results come back in globals, per =PE/Zsh.org=.
    ##
    typeset -ga tmuxnewsh2_split_env=() tmuxnewsh2_split_cmd=()

    local i
    for i in "$@" ; do
        if [[ "$i" =~ '^([^=]*)=(.*)$' ]] ; then
            tmuxnewsh2_split_env+="$match[1]=$(gq "$match[2]")"
            shift
        else
            break
        fi
    done

    tmuxnewsh2_split_cmd=( "$@" )
}
