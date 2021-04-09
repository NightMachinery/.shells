is-at-least 4.2.0 || { return 0 }
if [[ -n "$BROWSER" ]]; then
    _browser_fts=(htm html de org net com at cx nl se dk)
    for ft in $_browser_fts; do alias -s $ft=$BROWSER; done
fi

# _editor_fts=(cpp cxx cc c hh h inl asc txt TXT tex)
_editor_fts=(txt TXT tex)
for ft in $_editor_fts; do alias -s $ft=$EDITOR; done

_image_fts=(jpg jpeg png gif mng tiff tif xpm)
for ft in $_image_fts; do alias -s $ft=icat; done # sth is buggy and sometimes zsh says `zsh: permission denied: /Users/evar/Base/_Art/ddg/The Kindness..blue../09ec3883406d840ceb13a911359f709fa94cf24a.jpg`. This seems to only happen with full paths.

for ft in $video_formats; do alias -s $ft='awaysh mpv'; done
for ft in $audio_formats; do alias -s $ft=hear; done

#list whats inside packed file
alias -s zip="unzip -l"
alias -s rar="unrar l"
alias -s tar="tar tf"
alias -s tar.gz="echo "
alias -s ace="unace l"
