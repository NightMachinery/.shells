alias l='exa -al'
alias ash='autossh -M 0 -o "ServerAliveInterval 30" -o "ServerAliveCountMax 3"' #IC
alias px='ruu proxychains4'
alias imdb='imdbpy search movie --first'
alias set-timezone='sudo dpkg-reconfigure tzdata'
alias timer='noglob timer-raw'
alias zsh-to-shells='command -v zsh | sudo tee -a /etc/shells'
alias mpv='mpv --fs'
alias trs='trash -Fv'
alias pat='play-and-trash'
alias vi='nvim -u NONE'
alias setuid='sudo chmod 4755' #set the SetUID bit, make it executable for all and writable only by root. You still need to chown the file to root:root (root:wheel on macOS).
eval-linux 'alias gnc=nc'
alias dlga="deluge-console add"
alias pk="pkill -9 -i"
alias laa="ls-by-added"
alias ys="y-stream"
alias sb="sbash; szsh"
alias cdrose="cd /var/snap/nextcloud/common/nextcloud/data/FriedRose/files"
alias ai="sudo apt install -y"
alias au="sudo apt upgrade"
alias bre="brew remove"
alias auu="sudo apt update"
alias ncs="sudo nextcloud.occ files:scan --all"
alias his="history|grep"
alias re='run-on-each'
alias sbash='source ~/scripts/bash/load-others.bash'
alias szsh='source ~/scripts/zsh/load-others.zsh'
alias yic='youtube-dl --ignore-config  --external-downloader aria2c' #--external-downloader-args "-s 4"'
alias reeb='run-on-each rename-ebook'
alias aa='aria2c --seed-time=0 --max-tries=0 --retry-wait=1' #-Z has some unsavory sideeffects so I have not included in this.
alias set-volume='setv'
alias anki='/Applications/Anki.app/Contents/MacOS/Anki -b /Base/_GDrive/Anki'
alias pc='pbcopy'
alias pop='pbpaste'
alias ch='cht.sh'
alias powerdecay="sudo powermetrics -i 1000 --poweravg 1 | grep 'Average cumulatively decayed power score' -A 20"
alias bu='brew upgrade'
alias bcu='brew cask upgrade'
alias ynow='y -f best'
alias ddg='ddgr --unsafe -n 6'
alias dg='ddg --noprompt'
alias ggg='googler -n 6'
alias gg='ggg --noprompt'
alias t.hv='tmux new-session \; split-window -h \; split-window -v \; attach'
alias pdc='sdc "$(pbpaste)"'
alias lynx="lynx -cfg=~/.lynx.cfg  --accept_all_cookies"
alias rsp-safe='rsync --human-readable --xattrs --times --partial-dir=.rsync-partial  --info=progress2 -r'
alias rsp='rsp-safe --delete-after --force-delete' #--ignore-errors will delete even if there are IO errors on sender's side.
alias rspm='rsp --crtimes'
alias rspb='rsp --backup --backup-dir=.rsync-backup'
alias rspbm='rspb --crtimes'
alias fsayd='fsay Darkness is Paramount'
case "$(uname)" in
    Darwin)
        alias ggrep='ggrep  --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn}'
        ;;
    Linux)
        alias ggrep='grep  --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn}'
        ;;
esac

alias mac-mail-log="sudo log stream --predicate  '(process == "smtpd") || (process == "smtp")' --info" #this command starts filtering, so after that you get log messages when you start accessing smtp. 
alias erase-nonprintables='tr -cd "[:print:]\n"'
alias tmnte=increment-last\ \''(E)(\d+)'\'
alias tmnt=increment-last\ \''()(\d+)(?=\D*\z)'\'
