export LESS='-RiNF --mouse --wheel-lines=3' #F: --quit-if-one-screen ; R: maintain the ANSI colour sequences; i: smartcase searches (all lower=ignore case);  -N or --LINE-NUMBERS Causes a line number to be displayed at the beginning of each line in the display.
isSSH && LESS="-RiNF"
export PAGER="$commands[less]"
##
export Font_Symbola_CourierNew="$NIGHTDIR/resources/fonts/Symbola_CourierNew.ttf"
export Font_CourierNew_Symbola="$NIGHTDIR/resources/fonts/CourierNew_Symbola.ttf" # monospace
##
# I also use in my own functions (e.g., `reval-onhold`), so let them be.
# https://github.com/alexdelorenzo/onhold
# https://github.com/alexdelorenzo/ding
export ONHOLD=$GREENCASE_DIR/music/Sleep\ Party\ People\ -\ Heaven\ Is\ Above\ Us.m4a
export DING="$GREENCASE_DIR/LittleMisfortune/flac/26.1_16_MI_thetrainishere..blue..flac"
###
# `preview-half-page-down` is also an option
# unsupported key: ctrl-enter
export FZF_DEFAULT_OPTS="--bind 'shift-up:toggle+up,shift-down:toggle+down,alt-up:preview-up,alt-down:preview-down,tab:toggle,shift-tab:toggle+beginning-of-line+kill-line,alt-/:toggle-preview,ctrl-j:toggle+beginning-of-line+kill-line,ctrl-t:top,ctrl-s:select-all,alt-enter:print-query' --color=light --multi --hscroll-off 99999"
##
export FZF_DEFAULT_COMMAND="fd --type f" # fzf runs this when input is a tty
export SHELL=/bin/dash #"${commands[dash]}" # fzf uses this shell to run the default command
# It might be problematic to set SHELL, but who knows ...
export FZF_SHELL="$SHELL" # night.sh's variable
###
export ITERMMAGIC=ITERM_MAGIC
export iterm_socket="$HOME/tmp/.iterm_socket"
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
export deleteusdir=~/tmp/deleteus
export codedir="$HOME/code"
export BASE_DIR="$HOME/base"
export GREENCASE_DIR="$BASE_DIR/music/greencase"
##
export nightNotes="$cellar/notes/" # keep the trailing '/', it is important when removing prefixes
export orgdir="$nightNotes/org"
export memorydir="$nightNotes/private/memories"
export peopledir="$nightNotes/private/memories/people"
##
test -z "$attic_dir" && export attic_dir="$cellar/attic/"
export attic_private_dir="$cellar/attic_private/"
test -z "$attic" && attic="$attic_dir/.darkattic"
test -z "$attic_todo" && attic_todo="$attic_private_dir/.attic_todo"
test -z "$attic_temoji" && attic_temoji="$attic_dir/.temojis"
test -z "$attic_quotes" && attic_quotes="$attic_dir/.quotes"
test -z "$attic_emails" && attic_emails="$attic_private_dir/.emails"
##
export borgEndpoint="http://127.0.0.1:5922"
export timetracker_db="${attic_private_dir:?}/timetracker.db"
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
office_formats=(pdf ppt pptx doc docx xlsl)
createglob office_formats officeglob
media_formats=( ${audio_formats[@]} ${video_formats[@]} ${(@)office_formats} )
createglob media_formats mediaglob
code_formats=( m cpp h c applescript as osa nu nush el py jl scala sc kt kotlin java clj cljs rkt js rs zsh dash bash sh ml php lua glsl frag go )
createglob code_formats codeglob
config_formats=( ini json cson toml conf plist xml )
createglob config_formats configglob
text_formats=( $note_formats[@] $code_formats[@] $config_formats[@] )
createglob text_formats textglob
##
if isDarwin ; then
    # veditor=(code-insiders -r)
    veditor=(emc)
    cookiesFile="${HOME}/Library/Application Support/Google/Chrome/Default/Cookies"
else
    # test -e ~/.SpaceVim && veditor=(svi -p) ||
    veditor=(vim -p) # doc '-o opens in split view, -p in tabs. Use gt, gT, <num>gt to navigate tabs.'
fi

