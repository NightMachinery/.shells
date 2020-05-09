typeset -Ug path #Makes the path array's elements unique. It needs to be run again to fix a bad PATH, or some other array operation needs to be performed.
test -e ~/.SpaceVim && veditor=svi || veditor=vim
