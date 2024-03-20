##
org_props_folded=$'\n:PROPERTIES:\n:visibility: folded\n:END:\n'
##
typeset -g irc_networks=('Libera')
typeset -xgT IRC_USERNAMES irc_usernames=('greyrat' 'lucerne') '|'
##
typeset -g lilf_ip="82.102.10.244"
# typeset -g lilf_ip="82.102.11.148"
typeset -g lilf_user="eva"
##
WORDLIST0='/usr/share/dict/words'
##
# BROTHER_IP=192.168.1.230
BROTHER_IP=192.168.1.238
##
export Font_Symbola_CourierNew="$NIGHTDIR/resources/fonts/Symbola_CourierNew.ttf"
export Font_CourierNew_Symbola="$NIGHTDIR/resources/fonts/CourierNew_Symbola.ttf" # monospace
##
# I also use in my own functions (e.g., `reval-onhold`), so let them be.
# https://github.com/alexdelorenzo/onhold
# https://github.com/alexdelorenzo/ding
export ONHOLD=$GREENCASE_DIR/music/Sleep\ Party\ People\ -\ Heaven\ Is\ Above\ Us.mp3
export DING="$GREENCASE_DIR/LittleMisfortune/flac/26.1_16_MI_thetrainishere..blue..flac"
###
# `preview-half-page-down` is also an option
# unsupported key: ctrl-enter
export FZF_DEFAULT_OPTS="--exact --bind 'shift-up:toggle+up,shift-down:toggle+down,alt-up:preview-up,alt-down:preview-down,alt-n:next-history,alt-p:previous-history,tab:toggle,shift-tab:toggle+beginning-of-line+kill-line,alt-/:toggle-preview,ctrl-j:toggle+beginning-of-line+kill-line,ctrl-t:top,ctrl-s:select-all,alt-enter:print-query,shift-right:replace-query' --color=light --multi --hscroll-off 99999"
# ctrl-up and ctrl-down do the same thing as shift-up and shift-down
# pageup and pagedown keys work as expected
# `shift-up:preview-up,shift-down:preview-down,alt-up:page-up,alt-down:page-down`
# `shift-up:toggle+up,shift-down:toggle+down,alt-up:preview-up,alt-down:preview-down`
##
export FZF_DEFAULT_COMMAND="fd --hidden --follow" # fzf runs this when input is a tty
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

export BASE_DIR="$HOME/base"

export PURGATORY="$HOME/purgatory"
mkdir -p "$PURGATORY"

export cellar=~/cellar

export music_dir="$HOME/my-music"

export playlist_dir="${music_dir}/playlists"
export playlist_auto_dir="${playlist_dir}/autopl/"
# export songs_dir='/Volumes/hyper-diva/Songs' # @hardCoded @DarwinOnly
PRUNE_SONGD_DAYS="+120"
export GREENCASE_DIR="$BASE_DIR/music/greencase"

export logdir="$HOME/logs"

export deleteus=~/.deleteus
export deleteusdir=~/tmp/deleteus

export codedir="$HOME/code"

export chat_logs_dir="$BASE_DIR/documents/chat_logs"
##
export nightNotesN="${HOME}/notes"
export nightNotes="${nightNotesN}/"
# export nightNotes="$cellar/notes/"
#: Keep the trailing '/', it is important when removing prefixes. =nightNotesN= is useful for =aliasdir=.

export nightNotesPrivate="${nightNotes}/private"
export nightNotesPublic="${nightNotes}/public"

# export resources_dir="${nightNotes}/resources"
export nightResourcesPrivate="${nightNotes}/resources"
export nightResourcesPublic="${nightNotesPublic}/resources"
export nightGlobalBib="${nightResourcesPublic}/latex/global_refs.bib"

export orgdir="${nightNotesPublic}/org"
export org_img_dir="${orgdir}/images"
export memorydir="${nightNotesPrivate}/memories"
export peopledir="${nightNotesPrivate}/memories/people"

typeset -g paper_dir="$HOME/base/books/papers"
# paper_dir="$(ffz-get 'paper')" @TRET

typeset -g paper_source_dir="${paper_dir}/latex"
# paper_source_dir="$(ffz-get paper latex)" @TRET

# export mpv_bookmarks="${music_dir}/bookmarks/default"
export mpv_bookmarks="${nightNotesPrivate}/configs/mpv/bookmarks/default"

typeset -g UHIST_FILE="${nightNotesPublic}/bookmarks/useme/zsh/universal_history.zsh"
typeset -g UHIST_FILE_FC="${nightNotesPublic}/bookmarks/useme/zsh/universal_history_fc.zsh"

typeset -g kindle_clippings_dir="${nightNotesPrivate}/backups/Kindle/clippings"
typeset -g kindle_clippings_org_dir="${kindle_clippings_dir}/orgified"
##
test -z "$attic_dir" && {
    export attic_dir="$nightNotesPublic/attic/"
    # export attic_dir="$cellar/attic/"
}
export attic_private_dir="$nightNotesPrivate/attic/"
test -z "$attic" && attic="$attic_dir/.darkattic"
test -z "$attic_todo" && attic_todo="$attic_private_dir/.attic_todo"
test -z "$attic_temoji" && attic_temoji="$attic_dir/.temojis"
test -z "$attic_quotes" && attic_quotes="$attic_dir/.quotes"
test -z "$attic_emails" && attic_emails="$attic_private_dir/.emails"
##
export borgEndpoint="http://127.0.0.1:5922"

export timetracker_dir="${HOME}/.timetracker"
export timetracker_db="${timetracker_dir}/timetracker.db"
export cmdlog="${timetracker_dir}/cmdlogs/cmdlog.txt"
##
export remindayRootDir="${nightNotes}/rem"
export remindayDir="${remindayRootDir}/reminders"
export remindayBakDir="${remindayRootDir}/reminders_bak"

export remindayCDir="${remindayRootDir}/remindersC"
export remindayBakCDir="${remindayRootDir}/remindersC_bak" # moving these to a local location will cause one server to lose out on it.
# export remindayBakCDir="$HOME/tmp/remindersC_bak"
##
export ZETTLE_DIR="$nightNotesPublic/zettle"
export ZETTLE_DIR="${ZETTLE_DIR:a}"
export nightJournal="$nightNotesPrivate/journal/j0/"
##
note_formats=( txt md org )
createglob note_formats noteglob
##
typeset -ag ebook_formats=(pdf epub mobi azw azw3 azw4 txt cbz cbr djvu fb2 prc lit lrf odt pdb pml rb snb tcr)
#: * @GPT4T
#: - `djvu`: A format often used for scanned documents and ebooks, particularly those containing a combination of text, line drawings, and photographs.
#: - `fb2`: FictionBook 2.0 format, an XML-based ebook format that is popular in countries where the EPUB format is not widely adopted.
#: - `azw3`: Also known as KF8 (Kindle Format 8), it's Amazon's newer version of the AZW format that supports HTML5 and CSS3.
#: - `azw4`: A format that is essentially a PDF wrapped in a Kindle-compatible format, often used for textbooks.
#: - `prc`: An older ebook format that is also used on some mobile devices and e-readers.
#: - `html` and `htmlz`: HTML is the standard markup language for documents designed to be displayed in a web browser, and it can be used for ebooks. HTMLZ is a zipped HTML ebook format.
#: - `lit`: Microsoft's discontinued ebook format for its Microsoft Reader software.
#: - `lrf`: Sony's proprietary format for its line of ebook readers.
#: - `odt`: OpenDocument Text, the word processing file format of OpenOffice and LibreOffice, which can also be used for ebooks.
#: - `pdb`: Palm Digital Media format, used on PalmOS and other devices.
#: - `pml`: Palm Markup Language, an older ebook format used on Palm devices.
#: - `rb`: A format used by the Rocket eBook device.
#: - `snb`: A format used by the Shanda Bambook.
#: - `tcr`: A text compression format for ebooks.
#:#
audio_formats=(mp3 m4a m4b ogg flac ogm opus wav wma aac aiff ape mka mpa ra tta wv)
createglob audio_formats audioglob

image_formats=(png jpg jpeg gif psd tif tiff avif webp)
createglob image_formats imageglob

video_formats=(ape avi flv mp4 mkv mov mpeg mpg rm webm)
createglob video_formats videoglob

office_formats=(pdf ppt pptx doc docx xlsl)
createglob office_formats officeglob

media_formats=( ${audio_formats[@]} ${video_formats[@]} ${(@)office_formats} )
createglob media_formats mediaglob

code_formats=( m cpp h c swift applescript as osa nu nush el ss scm lisp rkt py jl scala sc kt kotlin java clj cljs rkt js jxa dart rs rb cr crystal zsh dash bash sh ml php lua glsl frag go )
createglob code_formats codeglob

config_formats=( ini json cson toml conf plist xml )
createglob config_formats configglob

text_formats=( $note_formats[@] $code_formats[@] $config_formats[@] )
createglob text_formats textglob

archive_formats=( zip rar tar gz 7z )
createglob archive_formats archiveglob
##
if isDarwin ; then
    # veditor=(code-insiders -r)
    veditor=(emc)
    cookiesFile="${HOME}/Library/Application Support/Google/Chrome/Default/Cookies"
else
    # test -e ~/.SpaceVim && veditor=(svi -p) ||
    veditor=(vim -p) # doc '-o opens in split view, -p in tabs. Use gt, gT, <num>gt to navigate tabs.'
fi
##
