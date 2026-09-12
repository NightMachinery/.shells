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

#: @duplicateCode/0c8b9d0226cdfb4f5bc0a9ea735089df
function tmuxnew {
    #: @todo0 integrate =str2tmuxname=
    ##

    # command tmux kill-session -t "$1" &> /dev/null || true
    tmux-session-processes-kill "$1"

    command tmux new -d -s "$@"
}
