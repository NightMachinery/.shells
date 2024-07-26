set -exo pipefail
##

## WebI
#: @proxyNeeded

curl -sS https://webi.sh/webi | sh

curl -sS https://webi.sh/golang | sh
curl -sS https://webi.sh/go-essentials | sh

# curl -sS https://webi.sh/ffmpeg | sh
#: ffmpeg via webi has broken
#: - [jalali:1403/04/11/16:29]

curl -sS https://webi.sh/gh | sh
curl -sS https://webi.sh/rg | sh
curl -sS https://webi.sh/fd | sh
curl -sS https://webi.sh/fzf | sh
curl -sS https://webi.sh/jq | sh


#: @NA :
# curl -sS https://webi.sh/eza | sh
# curl -sS https://webi.sh/nnn | sh
# curl -sS https://webi.sh/ugrep | sh
# curl -sS https://webi.sh/ncdu | sh
# curl -sS https://webi.sh/docker | sh
##

function junest-install {
(
    export PS4='> '
    setopt LOCAL_OPTIONS PIPE_FAIL PRINT_EXIT_VALUE ERR_RETURN SOURCE_TRACE XTRACE 
    setopt TYPESET_SILENT NO_CASE_GLOB multios re_match_pcre extendedglob pipefail interactivecomments hash_executables_only
    ##
    git clone https://github.com/fsquillace/junest.git ~/.local/share/junest
    ##
    export PATH="${HOME}/.local/share/junest/bin:$PATH"
    export PATH="$PATH:${HOME}/.junest/usr/bin_wrappers"
    export JUNEST_HOME="${HOME}/.junest"

    if test -n "${JUNEST_ENV}" ; then
        #: We are already inside junest, so just use the normal sudo.
        alias sudo-junest=sudo
    else
        alias sudo-junest="${HOME}/.junest/usr/bin_wrappers/sudo"
    fi
    ##
    junest setup
    sudo-junest pacman -Syy
)
}

if ! test -e ~/.local/share/junest ; then
    junest-install
fi

sudo-junest pacman -Syy --noconfirm gcc zsh ugrep tealdeer progress bandwhich ncdu eza ffmpeg

sudo-junest pacman -Syy --noconfirm rust
cargo install rm-improved
# sudo-junest yay -S --noconfirm rm-improved

sudo-junest pacman -Syy --noconfirm socat nodejs npm

sudo-junest pacman -Syy --noconfirm tealdeer
tldr --update

sudo-junest pacman -Syy --noconfirm emacs

# sudo-junest pacman -Syy --noconfirm redis
#: doesn't seem to work

#: Add to cron:
# tmux new -d -s redis redis-server

###
# brew install gcc zsh ugrep tealdeer progress bandwhich ncdu rm-improved eza

# brew install redis
# brew services start redis
###
