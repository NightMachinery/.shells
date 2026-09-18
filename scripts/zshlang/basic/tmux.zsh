## Minimal tmux helpers shared by public plugins and local tmux helpers.
function h-tmux-pane-of-pid {
    : "prints the unique live tmux pane owning <pid>; 1 outside tmux, 2 unsafe/unknown"
    #: Environment markers are hints, not ownership. Tool shells and clean
    #: shell restarts can lose them; nested launches can inherit stale ones.
    #: Walk through pty proxies to the pane shell, not the agent's inner tty.
    #: Like TmuxOf in golang/agent_session/internal/proc/proc.go, match ancestry
    #: against pane_pid. Keep this small shell caller independent of a build.
    #: Uses the socket selected by TMUX (or tmux's default); if that socket
    #: cannot account for a tmux ancestor, fail closed rather than guessing.
    ##
    emulate -L zsh
    local pid="${1}" hint="${TMUX_PANE:-}" tmux_env="${TMUX:-}"
    local row comm parent panes pane pane_pid dead found='' under_tmux=''
    local -a fields
    local -A ancestors
    local -i depth=0
    if [[ "${pid}" != <-> ]] || (( pid <= 1 )) ; then
        print -ru2 -- "$0: a live process PID is required"
        return 2
    fi

    while (( pid > 1 )) ; do
        if (( depth++ >= 64 )) || [[ -n "${ancestors[$pid]:-}" ]] ; then
            print -ru2 -- "$0: incomplete or cyclic process ancestry"
            return 2
        fi
        ancestors[$pid]=1
        row="$(command ps -o ppid=,comm= -p "${pid}" 2>/dev/null)" || {
            print -ru2 -- "$0: cannot inspect process ${pid}"
            return 2
        }
        fields=( ${=row} )
        parent="${fields[1]}"
        comm="${(j: :)fields[2,-1]}"
        if [[ "${parent}" != <-> || -z "${comm}" ]] || (( parent < 1 )) ; then
            print -ru2 -- "$0: incomplete process ancestry at ${pid}"
            return 2
        fi
        [[ "${comm:t}" == tmux || "${comm}" == 'tmux: server' ]] && under_tmux=y
        pid="${parent}"
    done

    if ! panes="$(command tmux list-panes -a -F $'#{pane_id}\t#{pane_pid}\t#{pane_dead}' 2>/dev/null)" ; then
        if [[ -n "${under_tmux}${hint}${tmux_env}" ]] ; then
            print -ru2 -- "$0: cannot query the owning tmux server; check TMUX/socket access"
            return 2
        fi
        return 1
    fi
    for row in "${(@f)panes}" ; do
        fields=( "${(@ps:\t:)row}" )
        pane="${fields[1]}" pane_pid="${fields[2]}" dead="${fields[3]}"
        [[ "${pane}" == %<-> && "${pane_pid}" == <-> && "${dead}" == 0 ]] || continue
        [[ -n "${ancestors[$pane_pid]:-}" ]] || continue
        if [[ -n "${found}" && "${found}" != "${pane}" ]] ; then
            print -ru2 -- "$0: multiple panes match the process ancestry"
            return 2
        fi
        found="${pane}"
    done
    if [[ -n "${found}" ]] ; then
        if [[ -n "${hint}" && "${hint}" != "${found}" ]] ; then
            print -ru2 -- "$0: TMUX_PANE (${hint}) conflicts with owning pane ${found}"
            return 2
        fi
        print -r -- "${found}"
        return 0
    fi
    if [[ -n "${under_tmux}${hint}${tmux_env}" ]] ; then
        print -ru2 -- "$0: tmux context exists but no live owning pane was found"
        return 2
    fi
    return 1
}


function h-tmux-env-repair {
    : "rebuilds this shell's TMUX/TMUX_PANE from the pane that really owns <pid> (default $$); 0 in tmux, 1 outside it, 2 unknown"
    #: The environment markers are all most callers have, and they go wrong in
    #: both directions. A clean restart drops them -- `zsh-restart' execs
    #: `env -i' over the pane's own root process, so the entire pane loses tmux
    #: for good -- and a nested or relaunched process can carry a pane id
    #: belonging somewhere else entirely. Either way `isTmux' lies, and
    #: [agfi:tmux-session-rename-current] then either refuses to run or renames
    #: the wrong session.
    #:
    #: So ask tmux rather than the environment: [agfi:h-tmux-pane-of-pid] finds
    #: the owning pane from the process ancestry, and the pane is enough to
    #: rebuild both variables byte for byte as tmux itself writes them.
    #:
    #: This *mutates and exports into the calling shell* on purpose: it only
    #: ever writes values that are true of this process, so no caller can be
    #: left worse off than the stale or missing values it replaces.
    ##
    emulate -L zsh
    local pid="${1:-$$}"
    local pane spec
    local -i rc=0
    local -a fields

    #: Resolved with TMUX_PANE blanked, because a *stale* one is one of the two
    #: cases this exists to fix and `h-tmux-pane-of-pid' fails closed on the
    #: conflict. The command substitution is a subshell, so the blanking cannot
    #: leak back out. TMUX is left alone deliberately: it selects the server
    #: socket, and it is the best hint available for which server to ask.
    pane="$(TMUX_PANE='' h-tmux-pane-of-pid "${pid}")" || rc=$?

    if (( rc == 1 )) ; then
        #: Proven to be in no pane at all, so anything still set is a leftover
        #: from some other pane -- exactly the value that would rename it.
        unset TMUX TMUX_PANE
        return 1
    fi
    (( rc == 0 )) || return "${rc}"

    spec="$(command tmux display-message -p -t "${pane}" $'#{socket_path}\t#{pid}\t#{session_id}' 2>/dev/null)" || spec=''
    fields=( "${(@ps:\t:)spec}" )
    if (( ${#fields} != 3 )) || test -z "${fields[1]}" ; then
        print -ru2 -- "$0: found pane ${pane} but could not rebuild TMUX from it"
        return 2
    fi

    #: `#{session_id}' prints `$445'; the variable carries the bare number.
    typeset -gx TMUX="${fields[1]},${fields[2]},${fields[3]#\$}"
    typeset -gx TMUX_PANE="${pane}"
    return 0
}

function h-tmux-here-p {
    : "true when this shell sits in a live tmux pane, repairing TMUX/TMUX_PANE first"
    #: The guard to use in place of a bare [agfi:isTmux] wherever the answer
    #: decides what happens to a *named* tmux object -- renaming a session,
    #: setting an option on one. `isTmux' reads one environment variable, and
    #: that variable is wrong often enough to matter in both directions: absent
    #: after a clean shell restart, so the helper refuses inside a real pane;
    #: or left over from a pane this process no longer lives in, so the helper
    #: cheerfully acts on somebody else's session. Neither shows up as an
    #: error, which is what makes them expensive.
    #:
    #: So settle the question against tmux before answering it, with
    #: [agfi:h-tmux-env-repair]. Quiet on purpose: callers print their own
    #: refusal, and the repair's diagnostics are about a conflict it has
    #: already resolved. It costs one ancestry walk plus one `list-panes'
    #: against the server, which is why this belongs in the handful of helpers
    #: that rename things and not in `isTmux' itself.
    ##
    emulate -L zsh
    h-tmux-env-repair >/dev/null 2>&1 || true
    test -n "${TMUX:-}" && test -n "${TMUX_PANE:-}"
}

#: @duplicateCode/0c8b9d0226cdfb4f5bc0a9ea735089df
function tmuxnew {
    #: @todo0 integrate =str2tmuxname=
    ##

    #: The rich kill needs `kill-withchildren' and the pane listers, which live
    #: in auto-load and are not part of any plugin. Without them, fall back to
    #: tmux's own kill rather than failing: a plugin-only load still has to be
    #: able to replace a session.
    if (( ${+functions[tmux-session-processes-kill]} )) ; then
        tmux-session-processes-kill "$1"
    else
        command tmux kill-session -t "$1" &> /dev/null || true
    fi

    command tmux new -d -s "$@"
}
