##
re noglobfn jlib jfic
# Note that this won't affect the use of these functions from before this call is made. (i.e., globexists shouldn't come here.)
##
psource "$HOME/.privateShell"
[ -e ~/.localScripts ] && re psource ~/.localScripts/**/*.zsh || :
