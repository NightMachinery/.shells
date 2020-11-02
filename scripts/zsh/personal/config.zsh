###
if [ -z "$HISTFILE" ]; then
    HISTFILE=$HOME/.zsh_history
fi
export HISTSIZE=1000000
## BASH only
export HISTFILESIZE=20000000
export HISTTIMEFORMAT="%m/%d/%Y %T " #I always tend to configure my machines with an large HISTSIZE value so it keeps a longer history list, as well as HISTTIMEFORMAT with the time stamp value so I can see when was the command ran.
###
unalias run-help &> /dev/null
autoload run-help
# typeset -g ZSH_AUTOSUGGEST_STRATEGY
# ZSH_AUTOSUGGEST_STRATEGY=(history completion)
#export ZSH_AUTOSUGGEST_USE_ASYNC=y
##
TIMEFMT="%J  %U user %S system %P cpu %*E total; max RSS %M"
# See TIMEFMT in zshall
# %M     The  maximum memory the process had in use at any time in kilobytes.
# See REPORTMEMORY REPORTTIME for automatically reporting big time/memory usage
##
