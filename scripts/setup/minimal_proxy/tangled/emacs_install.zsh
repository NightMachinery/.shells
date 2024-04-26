(
export PS4='> '
setopt PIPE_FAIL PRINT_EXIT_VALUE ERR_RETURN SOURCE_TRACE XTRACE
##

# brew tap d12frosted/emacs-plus
# brew install emacs-plus@29 --without-cocoa

alias gcl='git clone --recursive'
cd ~/

rm -fr ~/.emacs.d ~/doom.d ~/.doom.d || true
gcl https://github.com/hlissner/doom-emacs ~/.emacs.d
gcl https://github.com/NightMachinary/doom.d

ln -s ~/doom.d ~/.doom.d

rehash

#: With `yes` added, hopefully non-interactive
command yes | doom install
doom sync
)
