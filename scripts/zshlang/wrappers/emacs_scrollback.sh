#!/usr/bin/env -S zsh -f

source ~/.bashrc
source ~/.shared.sh

tmp="$(gmktemp --suffix '.scrollback')" || return 1

cat > "$tmp" || return 1

echo "tmp: $tmp"
emacs.dash "$tmp"
# sleep 5
