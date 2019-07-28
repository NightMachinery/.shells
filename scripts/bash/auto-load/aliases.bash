re "silence unalias" map p fd ff pip
alias 2k='2kindle'
alias zzz='sleepnow'
alias lad=onla
alias xlad=onxla
alias pksay='pkill speechsynthesisd say'
alias fz="fzf --bind 'tab:toggle,shift-tab:toggle+beginning-of-line+kill-line,ctrl-j:toggle+beginning-of-line+kill-line,ctrl-t:top' --color=light -1 -m"
alias pkmu="pkill -f -- 'mpv.*--no-video'"
alias c='command'
alias l='exa -al'
alias lt='l -T'
alias ash='autossh -M 0 -o "ServerAliveInterval 30" -o "ServerAliveCountMax 3"' #IC
alias px='ruu proxychains4'
alias set-timezone='sudo dpkg-reconfigure tzdata'
alias timer='noglob timer-raw'
alias zsh-to-shells='command -v zsh | sudo tee -a /etc/shells'
alias mpv='mpv --fs'
alias pat='play-and-trash'
alias vi='nvim -u NONE'
alias setuid='sudo chmod 4755' #set the SetUID bit, make it executable for all and writable only by root. You still need to chown the file to root:root (root:wheel on macOS).
eval-linux alias gnc=nc
alias dlga="deluge-console add"
alias pk="pkill -9 -i"
alias ys="y-stream"
alias sb=". ~/.zshenv"
alias cdrose="cd /var/snap/nextcloud/common/nextcloud/data/FriedRose/files"
alias ai="sudo apt install -y"
alias aug="sudo apt upgrade"
alias brm="brew remove"
alias bri="brew reinstall"
alias aup="sudo apt update"
alias ncs="sudo nextcloud.occ files:scan --all"
alias his="history|grep"
alias sbash='source ~/scripts/bash/load-others.bash'
alias szsh='source ~/scripts/zsh/load-others.zsh'
alias yic='youtube-dl --ignore-config  --external-downloader aria2c' #--external-downloader-args "-s 4"'
alias reeb='run-on-each rename-ebook'
alias aa='noglob aria2c --seed-time=0 --max-tries=0 --retry-wait=1' #-Z has some unsavory sideeffects so I have not included in this.
alias set-volume='setv'
alias anki='/Applications/Anki.app/Contents/MacOS/Anki -b /Base/_GDrive/Anki'
alias pc='pbcopy'
alias pop='pbpaste'
alias ch='cht.sh'
alias powerdecay="sudo powermetrics -i 1000 --poweravg 1 | grep 'Average cumulatively decayed power score' -A 20"
alias bu='brew upgrade'
alias bcu='brew cask upgrade'
alias bcrm='brew cask remove'
alias ynow='y -f best' #No conversion
alias ddg='ddgr --unsafe -n 6'
alias dg='ddg --noprompt'
alias ggg='googler -n 6'
alias gg='ggg --noprompt'
alias t.hv='tmux new-session \; split-window -h \; split-window -v \; attach'
alias pdc='p sdc'
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

alias mac-mail-log="sudo log stream --predicate  '(process == \"smtpd\") || (process == \"smtp\")' --info" #this command starts filtering, so after that you get log messages when you start accessing smtp. 
alias erase-nonprintables='tr -cd "[:print:]\n"'
alias tmnte=increment-last\ \''(E)(\d+)'\'
alias tmnt=increment-last\ \''()(\d+)(?=\D*\z)'\'
alias apx='ALL_PROXY=socks5://127.0.0.1:1080'
alias aac='aa --ca-certificate=/etc/ssl/certs/ca-certificates.crt'
# alias cxc='noglob __calc_plugin'
alias retry='retry-limited 0'
alias s=silence
alias table2ebook='\wget -r -k -c --no-check-certificate -l1' #recursive convert_links continue recursive_depth
alias coursera='coursera-dl -n -pl --aria2 --video-resolution 720p --download-quizzes --download-notebooks -sl "en,fa" --resume'
alias ox='zdict -dt oxford'
alias wh='which'
alias rqup='wg-quick up ~/Downloads/rq.conf'
alias rqdown='wg-quick down ~/Downloads/rq.conf'
alias wifi='osx-wifi-cli'
alias pi='noglob pip install -U'
alias last-created='\ls -AtU|head -n1' #macOS only
alias last-accessed='\ls -Atu|head -n1' #macOS only
alias last-added='ls-by-added |head -n1' #macOS only
alias last-modified='\ls -At|head -n1'
alias l-c=last-created
alias l-ac=last-accessed
alias l-a=last-added
alias l-m=last-modified
alias milli="mill mill.scalalib.GenIdeaModule/idea"
alias eta="etlas exec eta"
alias eta7="~/.etlas/binaries/cdnverify.eta-lang.org/eta-0.7.0.2/binaries/x86_64-osx/eta"
alias pe="pkill -SIGUSR2 Emacs"
alias ls="ls -aG"
alias ocr="pngpaste - | tesseract stdin stdout | pbcopy; pbpaste"
alias cask="brew cask"
alias bi="brew install"
alias bci="brew cask install"
alias hear='mpv --keep-open=no --no-video' #--no-config  #'ffplay -autoexit -nodisp -loglevel panic'
alias weather="wego | less -r"
alias j8='export JAVA_HOME=$JAVA_HOME8; export PATH=$JAVA_HOME/bin:$PATH'
alias j9='export JAVA_HOME=$JAVA_HOME9; export PATH=$JAVA_HOME/bin:$PATH'
alias emacsi="brew install emacs-plus --HEAD --with-24bit-color --with-mailutils --with-x11 --without-spacemacs-icon"
alias emc="emacsclient -t"
alias emcg="emacsclient -c"
alias y="noglob youtube-dl --no-playlist"
alias enhance='function ne() { sudo docker run --rm -v "$(pwd)/`dirname ${@:$#}`":/ne/input -it alexjc/neural-enhance ${@:1:$#-1} "input/`basename ${@:$#}`"; }; ne'
alias ymp4="y -f 'bestvideo[ext=mp4]+bestaudio[ext=m4a]/mp4'"
alias tl='noglob tlrl'
alias w2e='noglob w2e-raw'
alias w2e-lw='noglob w2e-lw-raw'
alias dcali="h2ed='html2epub-calibre' "
alias dpan="h2ed='html2epub-pandoc' "
alias dpans="h2ed=html2epub-pandoc-simple"
