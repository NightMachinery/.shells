# Perhaps you should use aliasfn
###
re unalias a l la ll map p fd ff pip sp rd pu gss gd zi &>/dev/null # mv cp
alias 'brishzr-repeat'='isLocal && { ec ; assert reval-ec brishzr "$0" ; ec } || true'
alias rgjl="rgm -t julia" #"rgm --glob '*.jl'"
alias rgpy="rgm --glob '*.py'"
alias cleanbuffer='printf "\ec\e[3J"' # there is a similar tool in my anaconda's bin named clear.
alias emcsave="emc-gateway -e '(save-some-buffers t)'"
alias itunesprogress='exa -a -T -l ~/Library/iTunes/'
alias tll="tl -e w2e-curl"
alias visudo="VISUAL=vim sudo visudo"
##
alias FIY='FORCE_INTERACTIVE=y FORCE_NONINTERACTIVE="" '
alias FIN='FORCE_INTERACTIVE="" FORCE_NONINTERACTIVE=y '
##
ialias re='run-on-each' #duplicate, to make it ialias
ialias ec='print -r --' #duplicate, to make it ialias
alias norg="gron --ungron"
alias ungron="gron --ungron"
alias displaysleep='pmset displaysleepnow'
alias gis='gist --copy  --shorten'
alias bat='bat --theme OneHalfLight --pager="less -FRXn"'
alias bt='bat --style=plain'
alias btz='bt --language zsh'
alias dbg='DEBUGME=d'
##
# alias l='exa -a' # --oneline
# alias ll='exa -a -l --blocks'
# alias lt='exa -a -T'
alias lc='l -s created'
alias lm='l -s modified'
alias la='ls-by-added|tac'
alias lac='l -s accessed'
function last-exa() {
    # Usage:
    # last-exa created
    # last-exa created SOME_DIR
    # Illegal: last-exa created SOME_File ...
    #   Because exa doesn't sort them. exa is stupid. Use `array=(*.sh(Nom))` instead: o -> sort, m -> mod date
    #   https://unix.stackexchange.com/a/214280/282382
    ##
    local dir="$(bottomdir "${2:-.}")"
    # Ignoring hidden files, add --all to show them
    local rs="$(exa --sort "$@"|tail -n1)"
    if ! [[ "$rs" =~ '^/' ]] ; then
        rs="$dir/$rs"
    fi
    ec "$rs"
}
function last-created() {
    if isDarwin ; then
        last-exa created
    else
        ecerr "$0: creation time is not supported on Linux. Using mod date instead."
        last-modified
    fi
}
alias last-accessed='last-exa accessed'
alias last-modified='last-exa modified'
alias last-added='ls-by-added |head -n1' #macOS only
alias l-c=last-created
alias l-ac=last-accessed
alias l-a=last-added
alias l-m=last-modified
##
alias em="emc-gateway -e '(counsel-recentf)'" # helm-recentf
alias dmy='DEBUGME=y'
alias a=aget
alias jee='ensure-empty || return 1'
alias jej='isI && { ensure-ju || { ask "jufile is not set correctly; Are you sure you want to proceed?" N || return 1 } }'
alias 2k='2kindle'
alias zzz='sleepnow'
alias lad=onla
alias xlad=onxla
alias pksay='pkill speechsynthesisd say'
alias pkmu="pkill -f -- 'mpv.*--no-video'"
alias c='command'
alias px='ruu proxychains4'
alias set-timezone='sudo dpkg-reconfigure tzdata'
alias zsh-to-shells='command -v zsh | sudo tee -a /etc/shells'
alias pat='play-and-trash'
alias vi='nvim -u NONE'
alias setuid='sudo chmod 4755' #set the SetUID bit, make it executable for all and writable only by root. You still need to chown the file to root:root (root:wheel on macOS).
alias api="sudo apt install -y"
alias apug="sudo apt upgrade"
alias brm="reval-ec brew remove"
alias bri="brew reinstall"
alias apup="sudo apt update"
alias reeb='run-on-each rename-ebook'
alias aa='\noglob aacookies'
alias anki='/Applications/Anki.app/Contents/MacOS/Anki -b /Base/_GDrive/Anki'
alias ch='cht.sh'
alias powerdecay="sudo powermetrics -i 1000 --poweravg 1 | grep 'Average cumulatively decayed power score' -A 20"
alias bu='brew upgrade'
alias bcu='brew upgrade --cask'
alias bcrm='brew cask remove'
alias ynow='y -f best' #No conversion
alias ddg='ddgr --unsafe -n 6'
alias dg='ddg --noprompt'
alias ggg='googler -n 6'
alias gg='ggg --noprompt'
alias lynx="lynx -cfg=~/.lynx.cfg  --accept_all_cookies"
alias fsayd='fsay Darkness is Paramount'
case "$(uname)" in
    Darwin)
        alias ggrep='ggrep  --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn}'
        ;;
    Linux)
        alias ggrep='grep  --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn}'
        ;;
esac

alias erase-nonprintables='tr -cd "[:print:]\n"'
# alias cxc='\noglob __calc_plugin'
alias s=silent
alias rh=rehash
alias table2ebook='\wget -r -k -c --no-check-certificate -l1' #recursive convert_links continue recursive_depth
alias ox='zdict -dt oxford'
alias rqup='wg-quick up ~/Downloads/rq.conf'
alias rqdown='wg-quick down ~/Downloads/rq.conf'
alias wifi='osx-wifi-cli'
##
function pip-install {
    pip install -U "$@"
}
alias pi='\noglob pip-install'
##
alias milli="mill mill.scalalib.GenIdeaModule/idea"
alias eta="etlas exec eta"
alias eta7="~/.etlas/binaries/cdnverify.eta-lang.org/eta-0.7.0.2/binaries/x86_64-osx/eta"
alias ls="gls --hyperlink=auto --color=auto --all"
alias ocr="pngpaste - | tesseract stdin stdout | pbcopy; pbpaste"
alias cask="brew cask"
alias bi="brew install" # `--force-bottle` now causes problems when no bottle is available
alias bci="brew install --cask --no-quarantine"
alias weather="wego | less-min -r"
# alias j8='export JAVA_HOME=$JAVA_HOME8; export PATH=$JAVA_HOME/bin:$PATH'
# alias j9='export JAVA_HOME=$JAVA_HOME9; export PATH=$JAVA_HOME/bin:$PATH'
##
# https://github.com/ytdl-org/youtube-dl#format-selection-examples
alias ybase="noglob youtube-dl --no-playlist --write-sub --sub-lang en --prefer-ffmpeg"
alias y="ybase --embed-subs --add-metadata --external-downloader aria2c --external-downloader-args '-c -j 3 -x 3 -s 3 -k 1M'" #  --embed-thumbnail errs: Only mp3 and m4a/mp4 are supported for thumbnail embedding for now. Causes only the first URL to be downloaded (possibly because of the error.)
alias 'y@pl'="ytdl_opts=(--yes-playlist  -o '%(playlist_index)03d. %(title)s.%(ext)s') "
alias ysmall="y -f '(bestvideo[height<=800]+bestaudio/best[height<=800]/best)[protocol^=http]'"
alias ys="ysmall"
# ysmp4 still can output an mkv. Probably because of merging?
alias ysmp4="y -f '(bestvideo[ext=mp4][height<=800]+bestaudio/best[ext=mp4][height<=800]/best[ext=mp4]/best)'"
# youtube-dl sometimes exits on error instead of retrying (possibly always) # aria2 will not get used for DASH
alias yarc="noglob retry ysmall --download-archive ~/.yarchive"
alias yic='y --ignore-config' #--external-downloader-args "-s 4"'
alias ymp4="y -f 'bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best'"
alias yaudio="noglob youtube-dl --no-playlist -f 'bestaudio[ext=m4a]/bestaudio'"
alias yaudio-playlist='yaudio --yes-playlist'
alias ymp3='noglob youtube-dl --no-playlist --prefer-ffmpeg --extract-audio --audio-format mp3'
alias ymp3-playlist='ymp3 --yes-playlist'
# `-f best` to download single file
##
alias tl='\noglob tlrl-ng'
alias w2e='\noglob w2e-raw'
alias w2e-lw='\noglob w2e-lw-raw'
alias dcali="h2ed='html2epub-calibre' "
alias dpan="h2ed='html2epub-pandoc' "
alias dpans="h2ed=html2epub-pandoc-simple"
alias carbon='carbon-now --headless --copy'
alias jglob='test -z "$*" && { test -n "$jufile" && set -- "$jufile" || { ecerr No args and no jufile supplied to $0 ; return 1 } }'
alias sii=nig
alias pkno='pk_no=y '
alias rmswp='command rm -i $NIGHTDIR/**/*.swp(D)'
alias rmpyc='command rm -ri $NIGHTDIR/**/__pycache__'
alias svi='nvim -u ~/.SpaceVim/vimrc'
