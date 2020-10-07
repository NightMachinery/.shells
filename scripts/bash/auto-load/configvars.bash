export LESS='-RiNF --mouse --wheel-lines=3' #F: --quit-if-one-screen ; R: maintain the ANSI colour sequences; i: smartcase searches (all lower=ignore case);  -N or --LINE-NUMBERS Causes a line number to be displayed at the beginning of each line in the display.
isSSH && LESS="-RiNF"
##
export FZF_DEFAULT_OPTS="--bind 'shift-up:toggle+up,shift-down:toggle+down,tab:toggle,shift-tab:toggle+beginning-of-line+kill-line,alt-/:toggle-preview,ctrl-j:toggle+beginning-of-line+kill-line,ctrl-t:top,ctrl-a:select-all' --color=light --multi --hscroll-off 99999"
export FZF_DEFAULT_COMMAND="fd --type f" # fzf runs this when input is a tty
export SHELL=/bin/dash #"${commands[dash]}" # fzf uses this shell to run the default command
# It might be problematic to set SHELL, but who knows ...
export FZF_SHELL="$SHELL" # night.sh's variable
##
ITERMMAGIC=ITERM_MAGIC
##
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
export BASE_DIR="$HOME/base"
export GREENCASE_DIR="$BASE_DIR/music/greencase"
##
export nightNotes="$cellar/notes/"
export orgdir="$nightNotes/org"
export memorydir="$nightNotes/private/memories"
export peopledir="$nightNotes/private/memories/people"
##
export remindayDir="$nightNotes/reminders"
export remindayBakDir="$cellar/reminders_bak"
##
export ZETTLE_DIR="$nightNotes/zettle"
export ZETTLE_DIR="${ZETTLE_DIR:a}"
export nightJournal="$nightNotes/journal/j0/"
note_formats=( txt md org )
createglob note_formats noteglob
##
audio_formats=(mp3 m4a m4b ogg flac ogm opus wav)
createglob audio_formats audioglob
image_formats=(png jpg jpeg gif psd)
createglob image_formats imageglob
video_formats=(ape avi flv mp4 mkv mov mpeg mpg rm webm)
createglob video_formats videoglob
media_formats=( ${audio_formats[@]} ${video_formats[@]} )
createglob media_formats mediaglob
code_formats=( el py jl scala sc kt kotlin java clj cljs rkt js rs zsh dash bash sh ml php lua glsl frag go )
createglob code_formats codeglob
config_formats=( ini json cson toml conf plist xml )
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

