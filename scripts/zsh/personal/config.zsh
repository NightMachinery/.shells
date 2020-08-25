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
