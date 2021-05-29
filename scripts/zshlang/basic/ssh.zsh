##
function mosh {
  command mosh --server="TERM=$TERM KITTY_WINDOW_ID=$KITTY_WINDOW_ID ITERM_SESSION_ID=$ITERM_SESSION_ID BICON_MODE=$BICON_MODE mosh-server" "$@" # -- zsh
}
##
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ] || [[ -n "$SSH_CONNECTION" ]] ; then
    amSSH=remote/ssh
else
    case $(ps -o comm= -p $PPID) in
        sshd|*/sshd) amSSH=remote/ssh;;
    esac
fi
isSSH() test -n "$amSSH"
