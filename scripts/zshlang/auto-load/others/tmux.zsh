##
tmuxnew() {
    silent tmux kill-session -t "$1"
    tmux new -d -s "$@"
}
tmuxnewsh() {
    doc "Use tsh instead."
    
    revaldbg tmuxnew "$1" "$(gq zsh -c "FORCE_INTERACTIVE=y ${tmuxnewshenv[*]} $(gq "${@[2,-1]}")")"
}
function tmuxnewsh2() {
    doc "Supports simple env vars automatically"

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
##
function tmuxzombie-ls() {
    tmux list-panes -a -F "#{pane_dead} #{pane_id}" | awk '/^1/ { print $2 }'
}
aliasfn tzls tmuxzombie-ls
function tmuxzombie-kill() {
    local fd1
    {
        exec {fd1}>&1 # to output to the original stdout
        tmuxzombie-ls >&$fd1 | inargsf re 'tmux kill-pane -t'
    } always { exec {fd1}>&- }
}
aliasfn tzkill tmuxzombie-kill
tmuxzombie() {
    # kills the pane of a session, thus turning it to a "dead pane"
    tmux list-panes  -s -F '#{pane_pid}' -t "$1" | inargsf serr kill
}
##
function str2tmuxname() {
    # this might be too restrictive
    gtr -cd ' [a-zA-Z0-9]_-'
}
function str2filename() {
    # this might be too restrictive
    gtr -d ':?/\\~!@#$%^&*+|<>\000'$'\n\t'
}
##
function tmux-capture() {
    local target="${1:?}" limit="${2}" # empty limit seems to mean return everything
    tmux capture-pane -p -S -"$limit" -t "$target"
}
##
alias t.hv='tmux new-session \; split-window -h \; split-window -v \; attach'
function ivy() {
    various-darwin.zsh

    export DISABLE_DEFER=y
    tmux new-session -d 'zsh'
    tmux send-keys "muc " # 'mu' could also download, but it needs to be updated
    tmux split-window -h  'zsh'
    tmux send-keys "lunas
"
    tmux split-window -v 'zsh'
    # tmux split-window -v 'salice.py'
    tmux select-pane -t 0
    comment order matters. Select a pane before attaching.
    comment '-2            Force tmux to assume the terminal supports 256 colours.'
    tmux -2 attach-session -d
}
##
