## Vars
export mpv_audio_ipc=~/tmp/.mpv_audio_ipc
## Functions
function hear-noipc() {
    local vol="${hear_noipc_volume:-${hear_noipc_v:-40}}"
    command mpv --volume="$vol" --keep-open=no --no-video $MPV_AUDIO_NORM "$@"
}
aliasfn hearinvisible silent hear-noipc --no-terminal --load-scripts=no
@opts-setprefix hearinvisible hear-noipc
@opts-setprefix hear hear-noipc
function hear() {
    # arger "${(0@)$(rpargs "$@")}"
    comment '(0@) inserts empty elements with quoting'
    hear-noipc --input-ipc-server="$mpv_audio_ipc" ${(0@)"$(rpargs "$@")"} #--no-config  #'ffplay -autoexit -nodisp -loglevel panic'
}
##
function songc() {
    # Please note that I am relying on the auto-load plugin of mpv to load all files in a folder. If you don't have that, remove the `-e EXT` filters of fd in this function.
    local f
    f=()
    # re 'ecdbg arg:' 'start:' "all args:" "$@" '${@:1:-1}' "${@:1:-1}" "f begins" "${(@f)f}"
    local p="${@: -1}"
    test -z "${p##-*}" && set -- $@ '.' #Don't quote or you'll get ''s.
    test -z "${p##-*}" && p='.'
    local f2="$(playlister "$p")"
    f+=( ${(@f)f2} )
    local autopl="${playlist_dir:-$HOME/playlists}/autopl/"
    mkdir -p "$autopl"
    gfind "$autopl" -mindepth 1 -type f -mtime +3 -delete
    test $#f -gt 1 && ec "$f2" > "$autopl/$(date)"
    # re 'ecdbg arg:' 'end:' "all args:" "$@" '${@:1:-1}' "${@:1:-1}" "f begins" "${(@f)f}" 
    ! test -z "$f" && { touch-tracks  "${(@f)f}" ; hear "${@:1:-1}" "${(@f)f}" }
}
touch-tracks() {
    comment "songd dir-touches using touch-tracks, but songc does not. We can of course add an env var and dir-touch here ... Using 'mus' (so songd) might also work ..."
    test -z "$to_dirtouch" && touch-tracks_ "$@" || {
            local f tt i
            tt=()
            typeset -U tt
            f=( "$@" )
            for i in "$f[@]"
            do
                tt+=( "$(bottomdir "$i")"/*(DN) )
            done
            touch-tracks_ "$tt[@]"
        }
}
touch-tracks_() {
    local track
    for track in "$@"
    do
        # debugcol=(200 10 255) ecdbg "Touching $track"
        test -e "$track" && serr touch "$track" #"$(bottomdir "$track")"
    done
}
playlistc() {
    local pl="$(fd --follow -t f '.' "${playlist_dir:-$HOME/playlists/}" | fz -q "$*")"
    test -z "$pl" || { ec "Playing playlist(s) $pl" && hearp +s "${(@f)pl}" }
}
playlister() {
    find-music "$@" | fz -q "'.mp3 | '.m4a | '.flac " #--history "$music_dir/.fzfhist" # -q "$1"
    comment By adding the extensions to the query, we force it to show paths from the end.
}
find-music() {
    memoi_expire="${fm_expire:-$memoi_expire}" memoi_skiperr=y memoi-eval fd -c never --follow -e m4a -e mp3 -e flac --full-path "$*" "${music_dir:-$HOME/my-music}"
}
songd() {
    # music_dir=~/my-music/ ; musiccache='' # Update: Bug solved in newer versions (fine on 5.8). See https://www.zsh.org/mla/workers/2019/msg00700.html # To hardcode-circumvent zsh's unset bug.
    doc 'Use songc to play already downloaded files.
    Set PRUNE_SONGD_DAYS to, e.g., +120 to remove files older (measured by access time) than 120 days from the cache.'
    ecdbg "$@"
    comment songd expects existent query
    [[ "${@: -1}" =~ '--?.*' ]] && set -- "$@" ''
    local music_dir="${music_dir}/${musiccache:-cache}"
    local musiccache='/' #To avoid recursion. Can't be empty or it'll auto replace.
    mkdir -p "$music_dir"
    test -z "$PRUNE_SONGD_DAYS" || {
        gfind "$music_dir" -mindepth 1 -type f -mtime "$PRUNE_SONGD_DAYS" -print -delete >> "$logdir"/prune_songd 2>&1 | tee
        # Access time itself is hard to use
        # https://unix.stackexchange.com/questions/530896/removing-directories-not-accessed-in-x-days
        # So we TOUCH :')
    }
    silence eval '\rm -r -- "$music_dir/"*(-@D)' #The characters in parentheses are glob qualifiers: - to dereference symlinks, @ to match only symlinks (the combination -@ means broken symlinks only), and D to match dot files. To recurse into subdirectories, make that rm -- **/*(-@D).
    local bp
    { test "${1}" = "-d" || test "$1" = "-b" || test "$1" = "-p" } && {
        bp="$1"
        shift
    }
    ecdbg "$@"
    local q="${@: -1}"
    local spath="$music_dir/${q:gs#/# }/"
    ecdbg "spath: $spath"
    ecdbg "real spath:"  "$(realpath "$spath")"
    test "$bp" = "-d" && {
        trs "$(realpath "$spath")"
        trs "$spath"
        (exit 0)
    } || {
        { test -n "$q" && test -e "$spath" } && {
            ecdbg Cache found
            touch "$(bottomdir "$spath")"
            eval 'touch-tracks "$spath"/*' || {
                ecdbg Could not TOUCH tracks, probably no tracks are available.
                trs "$spath"
                (exit 1)
            } && {
                hear "${@:1:-1}" "$spath"
                (exit 0) }
        } || {
            local usedCache=''
            [[ "$q" =~ "^http" ]] || {
                # colorbg 0 0 255
                # colorfg 0 255 0
                # find-music "$q" #Printing available music
                comment 'the -1 autoselect feature of fzf can cause false positives but you can then just interrupt mpv which will make it exit non-zero.'
                comment "songc currently feeds the query into fd, so it's not fuzzy."
                # ecdbg "Calling songc with: "
                # re 'ecdbg "arg: "' "$@"
                # ecdbg "music_dir: $music_dir"
                music_dir="${music_dir:h}" songc "$@" && { usedCache='y' && return 0 } || ecdbg "songc exited $?"
                # I just learned about `return`, so the usedCache logic is now useless. It's also kind of buggy and returns 1 in case of success.
                # resetcolor
                test -z "$q" && return 0
            }
            ecdbg "usedCache: $usedCache"
            test -z "$usedCache" && {
                #nonexistent path
                fsaydbg Cache NOT found
                local fm_expire=-1
                mkdir -p "$spath"
                test -z "$bp" && {
                    spotdl -f "$spath" -s "$q" && { sleep 1 && songd "$@" } || {
                        songd -d "$@"
                    }
                } || {
                    local bp_name
                    spotdl_dir="$spath" aget sdlg "$bp" "$q:q" '&& bp_name=(./**/*.txt) ' && {
                        local bp_path="$music_dir/${bp_name:t:r:gs/[-_]/ }"
                        mkdir -p "$bp_path"
                        mv "$spath"/*(D) "$bp_path"
                        \rm -r "$spath"
                        ln -s "$bp_path" "$(removeTrailingSlashes "$spath")"
                        sleep 1 && songd "$bp" "$@"
                    } || {
                        songd -d "$@"
                    }
                }
            }
        }
    }
}
hearp() {
    local shuf='--shuffle'
    test "${1}" = '+s' && {
        shuf=''
        shift
    }
    local tracks="$(mapln '$1
' "$(cat "${@}")")"
    test -z "$shuf" || tracks="$(ec "$tracks"|shuf)"
    test -z "$NO_HEARP_TOUCH" && {
        touch-tracks "${(@f)tracks}"
    }
    # Don't use mpv's native --shuffle since it MIGHT use autoloaded tracks, also empty string causes a harmless error
    # k shuffles live in mpv (with MY config :D)
    hear --loop-playlist --playlist <(ec "$tracks")
}
mut() {
    music_dir=$HOME'/Downloads/Telegram Desktop' songc --loop ${*:+"$*"}
}
muf() songc --loop ${*:+"$*"}
mub() {
    songc --loop-playlist ${*:+"$*"} #alBum
}
mup() playlistc "$@"
mus() mu -b "$(@s "$@" album)"
mu() {
    local bp=()
    { test "${1}" = "-d" || test "$1" = "-b" || test "$1" = "-p" } && {
        bp+="$1"
        shift
    }
    songd "$bp[@]" --loop-playlist ${*:+"$*"} #Download
}
muc() { fz_opts=(--no-sort) songc --loop-playlist "$@" }
svpl() {
    # Save Playlist save-playlist save-pl
    mv "$(last-created "${playlist_dir:-$HOME/playlists}/autopl")" "${playlist_dir:-$HOME/playlists}/$1"
}
function sdl() {
    local bp
    { test "${1[1]}" = "-" } && { # || test "$1" = "-b" || test "$1" = "-p" } && {
        bp="$1"
        shift
    }
    test -z "$bp" && {
        nisout spotdl -f "${spotdl_dir:-.}" -s "$*" } || {
        nisout sdlg "$bp" "$@"
    }
}
