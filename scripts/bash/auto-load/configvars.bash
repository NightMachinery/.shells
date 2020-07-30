dl_base_url='https://files.lilf.ir'
fd_default=( --hidden --no-ignore )
h2ed='html2epub-pandoc'
export PURGATORY="$HOME/purgatory"
mkdir -p "$PURGATORY"
export cellar=~/cellar
export music_dir="$HOME/my-music"
to_dirtouch=y
export logdir="$HOME/logs"
PRUNE_SONGD_DAYS="+120"
export deleteus=~/.deleteus
export codedir="$HOME/code"
##
export nightNotes="$cellar/notes/"
export ZETTLE_DIR="$nightNotes/zettle"
export ZETTLE_DIR="${ZETTLE_DIR:a}"
export nightJournal="$nightNotes/journal/j0/"
note_formats=( txt md org )
createglob note_formats noteglob
##
audio_formats=(mp3 m4a m4b ogg flac ogm opus wav)
createglob audio_formats audioglob
video_formats=(ape avi flv mp4 mkv mov mpeg mpg rm webm)
createglob video_formats videoglob
media_formats=( ${audio_formats[@]} ${video_formats[@]} )
createglob media_formats mediaglob
code_formats=( el py jl scala sc kt kotlin java clj cljs rkt js rs toml zsh dash bash sh ml php lua glsl frag )
createglob code_formats codeglob
config_formats=( ini json cson toml conf )
createglob config_formats configglob
text_formats=( $note_formats[@] $code_formats[@] $config_formats[@] )
createglob text_formats textglob
##
if isDarwin ; then
    veditor=(code-insiders -r)
    cookiesFiles="${HOME}/Library/Application Support/Google/Chrome/Default/Cookies"
else
    # test -e ~/.SpaceVim && veditor=(svi -p) ||
    veditor=(vim -p) # doc '-o opens in split view, -p in tabs. Use gt, gT, <num>gt to navigate tabs.'
fi

