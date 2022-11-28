ialias zshrc='$=EDITOR ~/.zshrc'
ialias zshenv='$=EDITOR ~/.zshenv'
##
alias sb=". ~/.zshenv ; conda-reactivate ; bell-zsh-start"
alias ss='emc-sourceme'
alias sbp='eval "$(pbpaste)"'
alias sbi="sb ; source-interactive-all"
alias sia='source-interactive-all ; bell-zsh-start'
alias sbash='source "$NIGHTDIR"/bash/load-others.bash'
alias szsh='source "$NIGHTDIR"/zshlang/load-others.zsh'
##
alias hrep="fc -El 0 | grep"
alias grep='grep --color=auto'

ialias plc=playlistc
##
alias b='builtin'
alias n='noglob'
##
alias pym='python -m'
alias pyc='python -c'
alias tsm='tsend $me'
alias kipy="pbcopy 'import os; os.kill(os.getpid(), 9)' #kill from within ipython embed"
# alias ta='tmux a -t' # fftmux might have made this irrelevant
alias agsf='ags -F'
alias ebk='ebook-viewer'
##
alias ltl='lt -l'
##
isDarwin && alias ncdu="ncdu --color off --exclude 'Volumes' -x" # -x: Do not cross filesystem boundaries. using exclude patterns to avoid the infinite loop has not worked for me. beware that the loop can hog up all ram and then use swap space and fill up the disk completely.  --exclude-firmlinks also makes everything show up as zero.
alias mail='sudo less /var/mail/$(whoami)'
alias mcomix='awaysh python3 ~/bin/mcomixstarter.py'
##
alias pz='printz-quoted' # @useme for using zsh history suggestions effectively; E.g., `pz in doom sees`
##
alias o="@opts"
