export HISTSIZE=1000000
## BASH only
export HISTFILESIZE=20000000
export HISTTIMEFORMAT="%m/%d/%Y %T " #I always tend to configure my machines with an large HISTSIZE value so it keeps a longer history list, as well as HISTTIMEFORMAT with the time stamp value so I can see when was the command ran.
##
export NODE_OPTIONS='--max-old-space-size=3096 --unhandled-rejections=strict' # 1. Increasing default heap size 2. this is good for my own scripts but might break others ... Note that linux doesn't support args in shebang.
