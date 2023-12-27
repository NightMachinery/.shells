###
alias nohistory='unset HISTFILE' #disables history for current session

function history-enable {
    if test -z "$history_enable_nowarn" ; then
        ecerr "$0: Beware that this will still persist the history items before this was called"
    fi

    if [ -z "$HISTFILE" ]; then
        HISTFILE=$HOME/.zsh_history
    fi
}
history_enable_nowarn=y history-enable
##
export HISTSIZE=1000000
export SAVEHIST=1000000
## BASH only
export HISTFILESIZE=20000000
export HISTTIMEFORMAT="%m/%d/%Y %T "
###
unalias run-help &> /dev/null
autoload run-help
##
TIMEFMT="%J  %U user %S system %P cpu %*E total; max RSS %M"
# See TIMEFMT in zshall
# %M     The  maximum memory the process had in use at any time in kilobytes.
# See REPORTMEMORY REPORTTIME for automatically reporting big time/memory usage
##
