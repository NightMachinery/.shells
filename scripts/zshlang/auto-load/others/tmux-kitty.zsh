### tmux, kitty, and which process a pane's macOS permissions come from
#: A tmux server, and every pane it will ever run, inherits the TCC
#: "responsible process" of whichever client happened to start it (see
#: [agfi:darwin-responsible-get]). Started from a mosh or ssh session, all of
#: it answers to the remote-shell daemon, and one declined prompt breaks
#: osascript for the server's whole life. So:
#:
#: - [agfi:tmux-server-ensure] has kitty start the server when the caller is
#:   not kitty's; [agfi:tmuxnew] calls it.
#: - [agfi:tmux-server-doctor] says whom the running server answers to.
#: - [agfi:tmux2kitty] moves one job out of a server that is not kitty's.
#:
#: See =docs/tmux-kitty-tcc.md=.
##
function h-darwin-kitty-exe-p {
    : "usage: h-darwin-kitty-exe-p <path>; true when <path> is kitty's own executable"
    [[ "${1}" == */kitty.app/Contents/MacOS/kitty ]]
}

function h-tmux-server-socket {
    : "sets REPLY to the socket a bare tmux command would talk to"
    #: tmux's own rule: the server named by $TMUX inside tmux, else
    #: $TMUX_TMPDIR (default /tmp) plus tmux-UID/default. REPLY rather than
    #: stdout, so that [agfi:tmux-server-live-p] forks nothing.
    ##
    if test -n "${TMUX}" ; then
        REPLY="${TMUX%%,*}"
    else
        REPLY="${TMUX_TMPDIR:-/tmp}/tmux-${UID}/default"
    fi
}

function tmux-server-live-p {
    : "true when a tmux server accepts connections where a bare tmux command would look"
    #: A connect(2) through zsh/net/socket rather than a tmux command: no fork,
    #: and it can never start a server by accident. The socket *file* proves
    #: nothing, since it outlives a crashed server.
    ##
    if ! zmodload zsh/net/socket 2>/dev/null ; then
        #: Does not start a server either, and succeeds on an empty one.
        command tmux list-sessions &>/dev/null
        return $?
    fi

    h-tmux-server-socket
    zsocket "${REPLY}" 2>/dev/null || return 1

    local fd="${REPLY}"
    exec {fd}>&-
}

function tmux-server-responsible-get {
    : "prints '<pid> <executable>' of the process TCC holds responsible for the running tmux server"
    local server
    server="$(command tmux display-message -p '#{pid}' 2>/dev/null)" || return 1
    [[ "${server}" == <-> ]] || return 1

    darwin-responsible-get "${server}"
}

function tmux-server-kitty-p {
    : "true when the running tmux server answers to kitty; returns 2 when it cannot tell"
    #: 2 lets a caller that only warns stay quiet about "no server" or "not
    #: macOS" instead of crying wolf.
    ##
    local resp
    resp="$(tmux-server-responsible-get)" || return 2

    h-darwin-kitty-exe-p "${resp#* }"
}

function tmux-server-doctor {
    : "says which process the running tmux server answers to, and what to do when it is not kitty"
    if ! tmux-server-live-p ; then
        ecgray "$0: no tmux server is running"
        return 0
    fi

    local resp
    if ! resp="$(tmux-server-responsible-get)" ; then
        ecerr "$0: cannot tell which process the tmux server answers to"
        return 2
    fi

    if h-darwin-kitty-exe-p "${resp#* }" ; then
        ecgray "$0: the tmux server answers to kitty (pid ${resp%% *})"
        return 0
    fi

    ecerr "$0: the tmux server answers to ${resp#* } (pid ${resp%% *}), not kitty."
    ecerr "  Every pane's permission prompts are asked in that name."
    ecerr "  Move one job out now: tmux2kitty <session>."
    ecerr "  Repair: restart the server from a plain kitty window, outside tmux; see docs/tmux-kitty-tcc.md."
    return 1
}

function tmux-server-ensure {
    : "makes sure a tmux server is running, and has kitty start it when this shell is not kitty's"
    #: Call before anything that may start the server, i.e. `new-session'.
    #: With a server already up it is one connect(2) and no fork, so
    #: [agfi:tmuxnew] can afford it on every call.
    #:
    #: Never fails the caller. When kitty cannot start the server it says why,
    #: and the caller starts it the old way, from wherever it is: a server
    #: with the wrong attribution beats no tmux from the phone while the
    #: Mac's GUI is down.
    ##
    local timeout="${tmux_server_ensure_timeout:-5}"

    isDarwin || return 0
    tmux-server-live-p && return 0

    local self
    if self="$(darwin-responsible-get)" && h-darwin-kitty-exe-p "${self#* }" ; then
        #: Already kitty's: let the caller start it, exactly as boot always has.
        return 0
    fi
    local self_exe="${${self#* }:-an unknown process}"

    local sock
    if ! sock="$(kitty-socket-get)" ; then
        ecerr "$0: no tmux server, and kitty cannot start one; starting it from here, so its panes will answer to ${self_exe}"
        return 0
    fi

    h-tmux-server-socket
    local server_socket="${REPLY}"

    #: kitty's environment has no TMUX, so the server it starts looks for its
    #: socket under TMUX_TMPDIR. Forward that, or name the socket outright
    #: when ours came from $TMUX.
    local -a tmux_args=() env_opts=()
    if test -n "${TMUX}" ; then
        tmux_args+=( -S "${server_socket}" )
    elif test -n "${TMUX_TMPDIR}" ; then
        env_opts+=( --env "TMUX_TMPDIR=${TMUX_TMPDIR}" )
    fi
    #: `exit-empty off' keeps the still-empty server up until the caller's own
    #: `new-session' lands. It also means this server outlives its last
    #: session.
    tmux_args+=( start-server ';' set-option -s exit-empty off )

    ecgray "$0: asking kitty to start the tmux server, so that its panes answer to kitty and not to ${self_exe}"
    #: Through `zsh -c', as =session.kitty= starts `ivy' at boot, so that the
    #: server's global environment comes from =.zshenv= rather than kitty's
    #: bare Launch Services one.
    if ! kitty @ --to "${sock}" launch --type=background --cwd="${HOME}" "${env_opts[@]}" \
            -- "${commands[zsh]}" -c "command tmux $(gq "${tmux_args[@]}")" >/dev/null ; then
        ecerr "$0: kitty refused to launch the server; starting it from here, answering to ${self_exe}"
        return 0
    fi

    zmodload zsh/zselect 2>/dev/null
    local i
    for (( i = 0 ; i < timeout * 20 ; i++ )) ; do
        tmux-server-live-p && break
        zselect -t 5 #: centiseconds
    done

    if ! tmux-server-live-p ; then
        ecerr "$0: no tmux server from kitty after ${timeout}s; starting it from here, answering to ${self_exe}"
        return 0
    fi

    #: Verify rather than trust: another client may have raced us to it from
    #: the wrong place.
    tmux-server-kitty-p
    if (( $? == 1 )) ; then
        tmux-server-doctor || true
    fi
    return 0
}
##
#: * tmux2kitty: move one job out of tmux and into a kitty tab
#: The job re-runs as kitty's child, so its permission prompts are asked in
#: kitty's name. Its kitty window carries the user variable `tmux2kitty=<id>',
#: which, unlike a title, the program cannot overwrite; everything below
#: finds the window by it.
typeset -g tmux2kitty_state_dir_default="${XDG_STATE_HOME:-${HOME}/.local/state}/tmux2kitty"

function h-tmux2kitty-id {
    : "sets REPLY to the id tmux2kitty files <name> under: safe as a kitty match regex and as a file name"
    #: kitty match expressions split on spaces and read the value as a Python
    #: regex, and session names carry spaces, slashes and dots.
    ##
    REPLY="${1//[^A-Za-z0-9_@-]/_}"
}

function h-tmux2kitty-marker {
    : "sets REPLY to the marker file recording that <id> runs in kitty"
    local dir="${tmux2kitty_state_dir:-${tmux2kitty_state_dir_default}}"

    REPLY="${dir}/${1}"
}

function h-tmux2kitty-windows {
    : "usage: h-tmux2kitty-windows <socket>; prints one '<window id> TAB <id> TAB <pid> TAB <name>' line per moved job"
    local sock="${1}"
    assert-args sock @RET
    ensure-cmd jq @RET

    kitty @ --to "${sock}" ls | command jq -r '
        .[].tabs[].windows[]
        | select(.user_vars.tmux2kitty != null)
        | [.id, .user_vars.tmux2kitty, .pid, (.user_vars.tmux2kitty_name // "")]
        | @tsv'
}

function h-tmux2kitty-kill-tree {
    : "usage: h-tmux2kitty-kill-tree <pid>; TERM <pid> and its descendants, wait for them, KILL what is left"
    #: Descendants too: [agfi:kill-withchildren] semantics, but on one snapshot
    #: that the wait below can then check. A job that holds a port, like the
    #: garden, cannot be restarted until every process of it is gone.
    ##
    local timeout="${tmux2kitty_timeout:-10}"
    local pid="${1}"
    assert-args pid @RET

    local -a pids
    pids=( "${pid}" ${(f)"$(ps-grandchildren "${pid}")"} )
    pids=( ${pids:#} )

    if (( ${pids[(Ie)$$]} )) ; then
        ecerr "$0: refusing: this shell (pid $$) is part of what would be killed"
        return 1
    fi

    kill -TERM "${pids[@]}" 2>/dev/null

    zmodload zsh/zselect 2>/dev/null
    local i line
    local -a alive fields
    for (( i = 0 ; i < timeout * 20 ; i++ )) ; do
        #: Not `kill -0': it succeeds on a zombie, and kitty's `--hold' wrapper
        #: stays one until its window closes, which comes after this returns.
        alive=()
        for line in ${(f)"$(command ps -o pid=,stat= -p "${(j:,:)pids}" 2>/dev/null)"} ; do
            fields=( ${=line} )
            [[ "${fields[2]}" == Z* ]] || alive+=( "${fields[1]}" )
        done
        (( ${#alive} )) || return 0
        zselect -t 5 #: centiseconds
    done

    ecerr "$0: still alive after ${timeout}s, sending KILL: ${alive[*]}"
    kill -KILL "${alive[@]}" 2>/dev/null
    return 0
}

function h-tmux2kitty-interactive-p {
    : "usage: h-tmux2kitty-interactive-p <command...>; true when that command ends in an interactive shell, not a job"
    #: [agfi:tmuxnewsh] wraps even an interactive session in `zsh -c', as
    #: `zsh -c "cd DIR && ... zsh"', so peel `<shell> -c <string>' layers
    #: and look at what finally runs. Moving such a session would kill
    #: whatever you were doing in it and leave a fresh shell in kitty.
    ##
    local -a cmdv=( "$@" ) shells=( zsh bash dash sh fish )
    local depth

    for (( depth = 0 ; depth < 4 ; depth++ )) ; do
        (( ${#cmdv} )) || return 0
        (( ${shells[(Ie)${cmdv[1]:t}]} )) || return 1
        [[ "${cmdv[2]}" == -*c ]] || return 0 #: a shell with no script: interactive

        cmdv=( ${(Q)${(z)cmdv[3]}} )
        (( ${#cmdv} )) || return 0
        #: A compound `cd DIR && VAR=x cmd': what runs is its last word.
        (( ${shells[(Ie)${cmdv[-1]:t}]} )) && return 0
        (( ${shells[(Ie)${cmdv[1]:t}]} )) || return 1
    done
    return 1
}

function tmux2kitty {
    : "usage: tmux2kitty <tmux target>; kills that pane and re-runs its start command in a new kitty tab"
    #: For a job in a tmux server whose macOS permissions come from the wrong
    #: process (see [agfi:tmux-server-doctor]); nothing else in the server is
    #: touched. Inspect it later with [agfi:tmux2kitty-ls],
    #: [agfi:tmux2kitty-text] (works over mosh) and [agfi:tmux2kitty-focus].
    #: Starting the same session through [agfi:tmuxnew] again, e.g. by
    #: re-running its launcher, stops the kitty copy first.
    #:
    #: The target is any tmux pane target: `BrishGarden', `%28'. What re-runs
    #: is the pane's `pane_start_command' in its `pane_start_path', under
    #: `zsh -c' so that it gets the environment =.zshenv= builds, not the
    #: tmux server's.
    #:
    #: It refuses a pane that runs an interactive shell, as there is no job to
    #: re-run, unless `tmux2kitty_force_p' is set.
    ##
    local type="${tmux2kitty_type:-tab}"
    local force_p="${tmux2kitty_force_p}"

    local target="${1}"
    assert-args target @RET
    #: `display-message' prints empty fields for `=name' but takes `=name:'.
    if [[ "${target}" == "="* && "${target}" != *:* ]] ; then
        target+=':'
    fi

    local info
    info="$(command tmux display-message -p -t "${target}" \
        '#{pane_id}'$'\t''#{pane_pid}'$'\t''#{session_name}'$'\t''#{session_windows}'$'\t''#{window_panes}'$'\t''#{window_index}.#{pane_index}'$'\t''#{pane_start_path}'$'\t''#{pane_start_command}')" @TRET

    local -a f=( "${(@ps:\t:)info}" )
    local pane="${f[1]}" pid="${f[2]}" session="${f[3]}" cwd="${f[7]}" start="${f[8]}"
    if [[ "${pane}" != %<-> ]] || [[ "${pid}" != <-> ]] ; then
        ecerr "$0: no such pane: ${1}"
        return 1
    fi

    #: The name the job is known by from now on; a whole session when the pane
    #: is all there is to it, which is what lets [agfi:tmuxnew] reclaim it.
    local name="${session}"
    if (( f[4] != 1 || f[5] != 1 )) ; then
        name="${session}:${f[6]}"
    fi
    h-tmux2kitty-id "${name}"
    local id="${REPLY}"

    #: tmux prints the start command in its own quoting; zsh's lexer takes it
    #: off, as in =agent-session.zsh=. One word is a string tmux ran through
    #: `default-shell -c'; several are an argv it executed directly.
    #: Limitation: tmux escapes a literal newline or tab as `\n', `\t', and
    #: those come back as two characters.
    local -a words cmdv
    words=( ${(z)start} )
    if (( ${#words} == 0 )) ; then
        ecerr "$0: ${name} was started with no command, i.e. as an interactive shell; nothing to re-run"
        return 1
    elif (( ${#words} == 1 )) ; then
        local default_shell
        default_shell="$(command tmux show-options -gv default-shell 2>/dev/null)"
        cmdv=( "${default_shell:-/bin/sh}" -c "${(Q)words[1]}" )
    else
        cmdv=( "${(@Q)words}" )
    fi

    if ! bool "${force_p}" && h-tmux2kitty-interactive-p "${cmdv[@]}" ; then
        ecerr "$0: ${name} runs an interactive shell, not a job: moving it would kill what runs in it and leave a fresh shell. Set tmux2kitty_force_p=y to do it anyway."
        return 1
    fi

    #: Everything that can fail before the kill, so a failure leaves the job
    #: running where it was.
    local sock
    sock="$(kitty-socket-get)" @RET
    kitty @ --to "${sock}" ls >/dev/null @RET

    #: A held window from an earlier move of the same job.
    tmux2kitty-stop "${name}" || true

    ecgray "$0: stopping ${name} (pane ${pane}, pid ${pid})"
    h-tmux2kitty-kill-tree "${pid}" @RET
    #: A dead pane lingers under `remain-on-exit'.
    command tmux kill-pane -t "${pane}" &>/dev/null || true

    local -a opts=( --type="${type}" --cwd="${cwd:-${HOME}}"
        --var "tmux2kitty=${id}" --var "tmux2kitty_name=${name}" )
    if [[ "${type}" != (background|clipboard|primary) ]] ; then
        #: `--hold', so that a crash leaves its traceback readable rather than
        #: closing the window.
        opts+=( --keep-focus --hold --title "tmux2kitty: ${name}" )
    fi
    if [[ "${type}" == tab ]] ; then
        opts+=( --tab-title "tmux2kitty: ${name}" )
    fi

    local win
    if ! win="$(kitty @ --to "${sock}" launch "${opts[@]}" \
            -- "${commands[zsh]}" -c 'exec "$@"' tmux2kitty "${cmdv[@]}")" ; then
        ecerr "$0: kitty did not launch ${name}; it is now stopped. Start it again with its launcher."
        return 1
    fi

    local marker
    h-tmux2kitty-marker "${id}"
    marker="${REPLY}"
    command mkdir -p -- "${marker:h}" && ec "${name}" >| "${marker}"

    ecgray "$0: ${name} now runs in kitty window ${win}. See it with: tmux2kitty-text $(gq "${name}"), tmux2kitty-focus $(gq "${name}")"
}

function tmux2kitty-ls {
    : "lists the jobs tmux2kitty moved into kitty: name, kitty window id, and the pid kitty started"
    local sock
    sock="$(kitty-socket-get)" @RET

    local rows
    rows="$(h-tmux2kitty-windows "${sock}")" @RET

    local row
    local -a f
    for row in ${(f)rows} ; do
        f=( "${(@ps:\t:)row}" )
        ec "${f[4]:-${f[2]}}"$'\t'"window ${f[1]}"$'\t'"pid ${f[3]}"
    done
}

function h-tmux2kitty-match {
    : "sets REPLY to the kitty --match expression for the job named <name>"
    h-tmux2kitty-id "${1}"

    REPLY="var:tmux2kitty=^${REPLY}\$"
}

function tmux2kitty-text {
    : "usage: tmux2kitty-text <name>; prints the whole scrollback of a moved job's kitty window"
    #: Through kitty's socket, so it works from anywhere the socket is
    #: reachable, including a mosh session on the phone.
    ##
    local name="${1}"
    assert-args name @RET

    local sock
    sock="$(kitty-socket-get)" @RET
    h-tmux2kitty-match "${name}"

    kitty @ --to "${sock}" get-text --match "${REPLY}" --extent all
}

function tmux2kitty-focus {
    : "usage: tmux2kitty-focus <name>; switches kitty to a moved job's window"
    local name="${1}"
    assert-args name @RET

    local sock
    sock="$(kitty-socket-get)" @RET
    h-tmux2kitty-match "${name}"

    kitty @ --to "${sock}" focus-window --match "${REPLY}"
}

function tmux2kitty-stop {
    : "usage: tmux2kitty-stop <name>; stops a job tmux2kitty moved into kitty and closes its window"
    local name="${1}"
    assert-args name @RET

    h-tmux2kitty-id "${name}"
    local id="${REPLY}"
    h-tmux2kitty-marker "${id}"
    local marker="${REPLY}"

    local sock
    if ! sock="$(kitty-socket-get 2>/dev/null)" ; then
        #: The job was kitty's child, so a kitty that is gone took it along.
        command rm -f -- "${marker}"
        return 0
    fi

    local rows
    rows="$(h-tmux2kitty-windows "${sock}")" @RET

    local row
    local -a f
    for row in ${(f)rows} ; do
        f=( "${(@ps:\t:)row}" )
        [[ "${f[2]}" == "${id}" ]] || continue

        ecgray "$0: stopping ${name} (kitty window ${f[1]}, pid ${f[3]})"
        h-tmux2kitty-kill-tree "${f[3]}" || continue
        kitty @ --to "${sock}" close-window --match "id:${f[1]}" &>/dev/null || true
    done

    command rm -f -- "${marker}"
}

function h-tmux2kitty-reclaim {
    : "usage: h-tmux2kitty-reclaim <session>; for tmuxnew: stops the kitty copy of <session>, if tmux2kitty made one"
    #: A `test -e' when there is nothing to reclaim, which is every call but
    #: the first after a move.
    ##
    h-tmux2kitty-id "${1}"
    h-tmux2kitty-marker "${REPLY}"
    test -e "${REPLY}" || return 0

    tmux2kitty-stop "${1}"
}
##
