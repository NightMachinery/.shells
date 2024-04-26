set -exo pipefail
##

## WebI
#: @proxyNeeded

curl -sS https://webi.sh/webi | sh

curl -sS https://webi.sh/golang | sh
curl -sS https://webi.sh/go-essentials | sh

curl -sS https://webi.sh/ffmpeg | sh

curl -sS https://webi.sh/gh | sh
curl -sS https://webi.sh/rg | sh
curl -sS https://webi.sh/fd | sh
curl -sS https://webi.sh/fzf | sh
curl -sS https://webi.sh/jq | sh

#: @NA :
# curl -sS https://webi.sh/nnn | sh
# curl -sS https://webi.sh/ugrep | sh
# curl -sS https://webi.sh/ncdu | sh
# curl -sS https://webi.sh/docker | sh
##

sudo-junest pacman -Syy --noconfirm socat nodejs npm

sudo-junest pacman -Syy --noconfirm tealdeer
tldr --update

sudo-junest pacman -Syy --noconfirm emacs

brew install gcc zsh ugrep tealdeer progress bandwhich ncdu rm-improved eza

brew install redis
brew services start redis
