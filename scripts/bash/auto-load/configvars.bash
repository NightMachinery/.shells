fd_default=( --hidden --no-ignore )
h2ed='html2epub-pandoc'
cellar=~/cellar
music_dir="$HOME/my-music"
to_dirtouch=y
logdir="$HOME/logs"
PRUNE_SONGD_DAYS="+120"
deleteus=~/.deleteus
audio_formats=(mp3 m4a m4b ogg flac ogm opus wav)
media_formats=(ape avi flv mp4 mkv mov mpeg mpg rm webm)
codedir="$HOME/code"
if isDarwin ; then
    veditor=(code-insiders -r)
else
    test -e ~/.SpaceVim && veditor=(svi -p) || veditor=(vim -p) # doc '-o opens in split view, -p in tabs. Use gt, gT, <num>gt to navigate tabs.'
fi

