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

#: [agfi:tmux-session-id], [agfi:tmux-session-goto], [agfi:tmux-alive-p],
#: [agfi:tmux-ensure-attach], [agfi:tmuxnewsh], [agfi:tmuxnewsh2] and the
#: `z'-aware launchers now live in =zshlang/plugins/tmux-z/=, so minimal
#: remote setups get them too instead of keeping a second, drifting copy.
##

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
    tmux list-panes  -s -F '#{pane_pid}' -t "$(tmux-session-id "$1")" | inargsf serr kill
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

function tmux-pane-send-text {
    : "types <2> into the tmux pane <1> and submits it with Enter"
    #: A pane that is gone, or whose command has already exited, is refused
    #: rather than typed into. `~/.tmux.conf' sets `remain-on-exit' globally,
    #: so a finished pane is still a valid `-t' target and `send-keys' would
    #: report success while the keystrokes went nowhere. The caller is told
    #: instead, and can say so.
    #:
    #: Two `send-keys': the text goes with `-l', which sends it literally, so a
    #: word like `Enter', `C-c' or `Space' inside it is typed rather than read
    #: as a key name. The submit is therefore a call of its own, since `-l'
    #: would type the five letters `Enter'.
    #:
    #: An id (`%12'), not a name: a pane index moves when panes are split or
    #: closed, and the id never does. See [agfi:tmux-session-id].
    ##
    local pane="${1}" text="${2}"
    assert-args pane text @RET

    ensure-cmd tmux @RET

    #: An empty answer, not a failure, is how tmux reports a pane id that
    #: resolves to nothing: `display-message' exits 0 and prints nothing rather
    #: than complaining, so the emptiness is the check.
    local dead
    dead="$(command tmux display-message -p -t "${pane}" '#{pane_dead}' 2>/dev/null)" || dead=''
    if test -z "${dead}" ; then
        ecerr "$0: no such tmux pane: ${pane}"
        return 1
    fi
    if [[ "${dead}" != 0 ]] ; then
        ecerr "$0: tmux pane has exited, not typing into it: ${pane}"
        return 1
    fi

    command tmux send-keys -t "${pane}" -l -- "${text}" @RET
    command tmux send-keys -t "${pane}" Enter @RET
}
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

    local target
    target="$(tmux-session-id "${session}")" @RET

    tty-title "$(tmux-session-name-of "${target}")"
    tmux-session-goto "${target}"
}
##
#: Naming the session you are in. Aimed at shells spawned by an AI agent
#: inside tmux, so the tmux session can carry the agent session's name. Every
#: target goes through =$TMUX_PANE=: an agent's shell has no attached client,
#: so "the current session" has to be derived from the pane.
##
function tmux-session-current-get {
    : "prints the name of the tmux session this shell runs in"
    if ! h-tmux-here-p ; then
        ecerr "$0: not inside tmux"
        return 1
    fi

    command tmux display-message -p -t "${TMUX_PANE}" '#S'
}

function h-tmux-session-name-claim {
    : "frees <name> for a rename when a dead session is holding it; 0 free, 1 held by a live session"
    #: tmux refuses a duplicate session name, and `has-session' is not a
    #: liveness test: with remain-on-exit, a session whose process died stays
    #: listed forever and keeps its name against everything that comes after.
    #: [agfi:tmux-job-running-p] already treats a corpse as absent rather than
    #: as a conflict; this is the same stance for renames.
    #:
    #: It is routine rather than exotic. A resumed agent computes the very name
    #: its own abandoned session is still wearing, so `tnameme' after a resume
    #: met `duplicate session' against a corpse two days dead, and the pane
    #: kept the name of whoever had used it before.
    #:
    #: The corpse is moved aside, not killed: its scrollback is the only record
    #: of how the thing ended, and a rename is no place for a destructive
    #: default. [agfi:tmux-alive-p] decides, so a session with a live pane in
    #: any window is never touched.
    #:
    #: Usage: h-tmux-session-name-claim <name> [own-session-id]
    ##
    local name="${1}" own="${2}"
    assert-args name @RET

    local id
    #: Resolving the id doubles as the existence check: no session, name free.
    id="$(tmux-session-id "${name}" 2>/dev/null)" || return 0

    #: Already ours. Renaming a session to the name it has is a no-op, not a
    #: collision with a live session.
    if test -n "${own}" && [[ "${id}" == "${own}" ]] ; then
        return 0
    fi

    if tmux-alive-p "${id}" ; then
        return 1
    fi

    local aside="${name} ~dead"
    local i
    #: Corpses pile up under one name over time, so find a free suffix.
    for i in {2..99} ; do
        tmux-session-id "${aside}" &>/dev/null || break
        aside="${name} ~dead${i}"
    done

    command tmux rename-session -t "${id}" "${aside}" @RET
    ecgray "$0: '${name}' was held by a dead session; moved it to '${aside}'"
}

function tmux-session-rename-current {
    : "renames the tmux session this shell runs in"
    local name="${1}"
    assert-args name @RET

    if ! h-tmux-here-p ; then
        ecerr "$0: not inside tmux"
        return 1
    fi

    name="$(h-tmux-session-name-sanitize "${name}")"

    #: A session that died but still holds this name must not stop us; see
    #: [agfi:h-tmux-session-name-claim].
    local own
    own="$(command tmux display-message -p -t "${TMUX_PANE}" '#{session_id}')" @RET
    if ! h-tmux-session-name-claim "${name}" "${own}" ; then
        ecerr "$0: '${name}' is taken by a live session"
        return 1
    fi

    command tmux rename-session -t "${TMUX_PANE}" "${name}" @RET
    ecgray "$0: ${name}"
}

function h-tmux-session-agent-prefix {
    : "prints +Claude/<profile>, +Codex or +Agy for the agent that spawned this shell"
    local agent
    if ! agent="$(ai-agent-name)" ; then
        ecerr "$0: no AI agent detected in this shell's environment"
        return 1
    fi

    local marker="${agent_tmux_name_marker}"
    case "${agent}" in
        claude) ec "${marker}Claude/$(claude-code-profile-current)" ;;
        codex) ec "${marker}Codex" ;;
        agy) ec "${marker}Agy" ;;
        *) ec "${marker}${agent}" ;;
    esac
}

function tmux-session-rename-current-with-agent {
    : "like [agfi:tmux-session-rename-current], prefixed by the agent running this shell, e.g. '+Claude/work NAME'"
    local name="${1}"
    assert-args name @RET

    local prefix
    prefix="$(h-tmux-session-agent-prefix)" @RET

    tmux-session-rename-current "${prefix} ${name}"
}

function tmux-session-rename-current-auto {
    : "renames to '@<Agent> <the agent session's own name>'; needs no argument"
    #: The same name the hooks would give it ([agfi:h-agent-session-tmux-name]),
    #: so doing it by hand and letting the hook do it agree.
    #:
    #: Finding *which* session we are in differs per agent. Claude Code
    #: exports its session id ([agfi:claude-code-session-current-file]);
    #: Codex exports CODEX_THREAD_ID into its shell tool; agy exports
    #: ANTIGRAVITY_CONVERSATION_ID into its run_command shells (and to hooks).
    #: When the environment has nothing, e.g. a terminal opened next to the
    #: agent, this falls back to the identity the agent's hook left on the
    #: tmux session ([agfi:agent-tmux-identity-get]); until that hook has
    #: fired there is nothing to read.
    ##
    local agent
    agent="$(ai-agent-name)" || {
        #: A plain shell has no agent session to be named after. Doing nothing
        #: and succeeding keeps this safe to put in launchers and hooks that
        #: run in both kinds of shell.
        ecgray "$0: not inside an AI agent; leaving the session name alone"
        return 0
    }

    local id='' transcript=''
    case "${agent}" in
        claude)
            transcript="$(claude-code-session-current-file)" @RET
            ;;
        codex)
            id="${CODEX_THREAD_ID:-${CODEX_SESSION_ID}}"
            ;;
        agy)
            id="${ANTIGRAVITY_CONVERSATION_ID}"
            ;;
    esac

    if test -z "${id}${transcript}" ; then
        local recorded
        recorded="$(agent-tmux-identity-get 2>/dev/null)" || {
            ecerr "$0: cannot tell which ${agent} session this is: nothing in the environment, and its hook has not recorded one on this tmux session yet"
            return 1
        }
        local -a f
        f=("${(@ps:\t:)recorded}")
        if [[ "${f[1]}" != "${agent}" ]] ; then
            ecerr "$0: this tmux session was last claimed by ${f[1]}, not ${agent}"
            return 1
        fi
        id="${f[2]}" transcript="${f[3]}"
    fi

    local name
    name="$(h-agent-session-tmux-name "${agent}" "${id}" "${transcript}")" @RET

    tmux-session-rename-current "${name}"
}
aliasfn tsrc tmux-session-rename-current
aliasfn tsrcag tmux-session-rename-current-with-agent
aliasfn tsrca tmux-session-rename-current-auto
aliasfn tnameme tmux-session-rename-current-auto
##
function tmux-session-autoname {
    : "on|off|unset|status: may the agents' hooks rename the tmux session this shell runs in?"
    #: Sets the session-level =@agent_autoname=, which beats the global default
    #: from =~/.tmux.conf=. `unset' returns to that default. `on' also renames
    #: right away when run from inside Claude Code, so the effect is visible.
    ##
    local mode="${1:-status}"
    local opt="${agent_tmux_autoname_option}"

    if ! h-tmux-here-p ; then
        ecerr "$0: not inside tmux"
        return 1
    fi

    case "${mode}" in
        on|off)
            command tmux set-option -t "${TMUX_PANE}" "${opt}" "${mode}" @RET
            ecgray "$0: $(tmux-session-current-get): ${mode}"
            if [[ "${mode}" == on ]] ; then
                #: A silent no-op outside an agent, so no guard is needed.
                tmux-session-rename-current-auto
            fi
            ;;
        unset)
            command tmux set-option -u -t "${TMUX_PANE}" "${opt}" @RET
            ecgray "$0: $(tmux-session-current-get): back to the global default"
            ;;
        status)
            local own effective global
            own="$(command tmux show-option -qv -t "${TMUX_PANE}" "${opt}")"
            effective="$(command tmux show-option -qvA -t "${TMUX_PANE}" "${opt}")"
            global="$(command tmux show-option -gqv "${opt}")"
            ec "effective: ${effective:-unset}"
            ec "this session: ${own:-unset}"
            ec "global default: ${global:-unset}"
            ;;
        *)
            ecerr "$0: usage: $0 on|off|unset|status"
            return 1
            ;;
    esac
}

function tmux-session-autoname-global {
    : "on|off|unset: the default for every tmux session without its own @agent_autoname"
    #: For the running server only; the persistent default lives in =~/.tmux.conf=.
    local mode="${1}"
    assert-args mode @RET
    local opt="${agent_tmux_autoname_option}"

    case "${mode}" in
        on|off) command tmux set-option -g "${opt}" "${mode}" @RET ;;
        unset) command tmux set-option -gu "${opt}" @RET ;;
        *)
            ecerr "$0: usage: $0 on|off|unset"
            return 1
            ;;
    esac
    local global
    global="$(command tmux show-option -gqv "${opt}")"
    ecgray "$0: global default: ${global:-unset}"
}
aliasfn tnameme-on tmux-session-autoname on
aliasfn tnameme-off tmux-session-autoname off
aliasfn tnameme-status tmux-session-autoname status
##
#: A tmux session used as a long-running job: start it once, stop it, ask after it.
#:
#: [agfi:tmuxnewsh2] alone will not do, because [agfi:tmuxnew] KILLS an existing session
#: of that name before creating the new one. For a scratch shell that is the right
#: default; for a job it means re-running the start command silently takes down work that
#: was in flight, and you find out later, from output that never arrived.
#:
#: `has-session` is not a liveness test either: with remain-on-exit set, a session whose
#: process has died stays listed with a dead pane and has-session still says yes. So the
#: running test asks the panes, and start treats a corpse as absent rather than as a
#: conflict.
#:
#: Targets are written `=name`, tmux's exact match: without it, `foo` also selects a
#: session named `foobar`.
function tmux-job-running-p {
    local name="$1"
    assert-args name @RET

    command tmux has-session -t "=${name}" &>/dev/null || return 1
    local dead
    dead="$(command tmux list-panes -t "=${name}" -F '#{pane_dead}' 2>/dev/null)" || return 1
    #: one line per pane; a single live pane is enough to call the job running
    [[ "$dead" == *0* ]]
}

function tmux-job-start {
    local name="$1" ; shift
    assert-args name @RET
    (( $# )) || { ecerr "$0: ${name}: no command given" ; return 1 }

    if tmux-job-running-p "$name" ; then
        ecerr "$0: ${name} is already running; tmux-job-status ${name}, or tmux-job-stop ${name} first"
        return 1
    fi
    command tmux kill-session -t "=${name}" &>/dev/null || true #: a corpse is not a job

    tmuxnewsh2 "$name" "$@" @RET
    ecgray "$0: started ${name}"
}

function tmux-job-stop {
    local name="$1"
    assert-args name @RET

    if ! command tmux has-session -t "=${name}" &>/dev/null ; then
        ecgray "$0: ${name}: nothing to stop"
        return 0
    fi
    #: With their children: killing only the pane's own process leaves a grandchild rsync
    #: or ssh running against the files the next start will expect to own.
    tmux-session-processes-kill "$name"
    command tmux kill-session -t "=${name}" &>/dev/null || true
    ecgray "$0: stopped ${name}"
}

function tmux-job-status {
    local name="$1"
    assert-args name @RET

    if tmux-job-running-p "$name" ; then
        local pids
        pids="$(command tmux list-panes -t "=${name}" -F '#{pane_pid}' 2>/dev/null | command tr '\n' ' ')"
        ec "${name}: running (pane pid ${pids% })"
    elif command tmux has-session -t "=${name}" &>/dev/null ; then
        ec "${name}: DEAD, session kept by remain-on-exit; a start will clear it"
        return 1
    else
        ec "${name}: not running"
        return 1
    fi
}
##
