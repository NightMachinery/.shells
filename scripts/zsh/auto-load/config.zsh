typeset -Ug path #Makes the path array's elements unique. It needs to be run again to fix a bad PATH, or some other array operation needs to be performed.

if isDarwin ; then
    veditor=(code-insiders -r)
else
    test -e ~/.SpaceVim && veditor=(svi -p) || veditor=(vim -p) # doc '-o opens in split view, -p in tabs. Use gt, gT, <num>gt to navigate tabs.'
fi
