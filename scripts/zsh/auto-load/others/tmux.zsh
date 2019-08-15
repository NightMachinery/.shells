alias t.hv='tmux new-session \; split-window -h \; split-window -v \; attach'
ivy() {
    tmux new-session -d 'zsh'
    tmux split-window -h  'lo_s=$((60*30)) lo_noinit=y loop pmset displaysleepnow'
    tmux split-window -v 'ipython; zsh'
    comment '-2            Force tmux to assume the terminal supports 256 colours.'
    tmux select-pane -t 0
    comment order matters. Select a pane before attaching.
    tmux -2 attach-session -d
}
