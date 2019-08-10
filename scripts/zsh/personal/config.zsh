export HISTSIZE=100000
export HISTTIMEFORMAT="%m/%d/%Y %T " #I always tend to configure my machines with an large HISTSIZE value so it keeps a longer history list, as well as HISTTIMEFORMAT with the time stamp value so I can see when was the command ran.
isD && export NEDITOR='emacsclient' || export NEDITOR='nvim'
export ALTERNATE_EDITOR="" #Causes Emacs to start a daemon if one is not found.
export SUDO_EDITOR="$NEDITOR"
export VISUAL="$NEDITOR"
export EDITOR="$VISUAL"
unalias run-help &> /dev/null
autoload run-help
