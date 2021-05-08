###
alias nohistory='unset HISTFILE' #disables history for current session
function history-enable() {
    ecerr "$0: Beware that this will still persist the history items before this was called"

    if [ -z "$HISTFILE" ]; then
        HISTFILE=$HOME/.zsh_history
    fi
}
history-enable
export HISTSIZE=1000000
export SAVEHIST=1000000
## BASH only
export HISTFILESIZE=20000000
export HISTTIMEFORMAT="%m/%d/%Y %T " #I always tend to configure my machines with an large HISTSIZE value so it keeps a longer history list, as well as HISTTIMEFORMAT with the time stamp value so I can see when was the command ran.
###
unalias run-help &> /dev/null
autoload run-help
##
TIMEFMT="%J  %U user %S system %P cpu %*E total; max RSS %M"
# See TIMEFMT in zshall
# %M     The  maximum memory the process had in use at any time in kilobytes.
# See REPORTMEMORY REPORTTIME for automatically reporting big time/memory usage
##
