is-at-least 4.2.0 || return 0

if [[ -n "$BROWSER" ]]; then
    _browser_fts=(htm html de org net com at cx nl se dk)
    for ft in $_browser_fts; do alias -s $ft=$BROWSER; done
fi

_editor_fts=(cpp cxx cc c hh h inl asc txt TXT tex)
for ft in $_editor_fts; do alias -s $ft=$EDITOR; done

if [[ -n "$XIVIEWER" ]]; then
    _image_fts=(jpg jpeg png gif mng tiff tif xpm)
    for ft in $_image_fts; do alias -s $ft=$XIVIEWER; done
fi

_media_fts=(ape avi flv mkv mov  mpeg mpg rm webm)
for ft in $_media_fts; do alias -s $ft=mpv; done
_audio_fts=(mp3 m4a ogg flac ogm opus wav)
for ft in $_audio_fts; do alias -s $ft=hear; done

#list whats inside packed file
alias -s zip="unzip -l"
alias -s rar="unrar l"
alias -s tar="tar tf"
alias -s tar.gz="echo "
alias -s ace="unace l"
