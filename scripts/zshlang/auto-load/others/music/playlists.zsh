##
function playlist-auto-cleanup {
    local autopl="${playlist_auto_dir}"

    gfind "$autopl" -mindepth 1 -type f -mtime +3 -delete @TRET
}

function playlist-auto-create {
    local files=($@)

    local autopl="${playlist_auto_dir}"
    mkdir -p "$autopl" @TRET

    playlist-auto-cleanup @TRET

    local dest
    dest="$autopl/$(date).m3u" @TRET
    if (( ${#files} >= 1 )) ; then
        ec "${(pj.\n.)files}" > "$dest" @TRET
        playlist-relative-make "$dest" autopl/ @TRET
    fi
}
##
function playlist-auto-save-last() {
    # Save Playlist save-playlist save-pl
    ##
    local name="$1"
    assert-args name @RET

    local autopl="${playlist_auto_dir}"

    mv -v "$(last-created "$autopl")" "${playlist_dir}/${name}"
}
aliasfn playlist-save playlist-auto-save-last
aliasfn pls playlist-auto-save-last
##
function playlist-relative-make() {
    # @supersedes/`nnp'
    ##
    local input="$1"
    local out_name="${2}"
    local out_dir
    out_dir="$(bottomdir_empty='' bottomdir "$out_name")" @TRET
    out_name="$(bottomfile "$out_name")" @TRET
    if test -z "$out_name" ; then
        out_name+="/${input:t}"
    fi

    assert-args music_dir playlist_dir out_name @RET

    out_dir="${playlist_dir}/relative/${out_dir}"
    mkdir -p "$out_dir" @TRET

    local out
    out="${out_dir}/${out_name}"
    out="${out:a}"
    if [[ "$out" != *.m3u ]] ; then
        out+=".m3u"
    fi

    ecgray "$0: $(gquote-sq "$input") -(not deleted)-> $(gquote-sq "$out")"

    local music_dir_rel
    music_dir_rel="$(grealpath --relative-to "$out_dir" "$music_dir")" @TRET
    local shared_dir_rel="${music_dir_rel}/.."

    cat "$input" | \
    sd "$music_dir" "${music_dir_rel}"  \
    > "$out"

    ## @legacyWorkaround
    # local songs_dir_rel="${shared_dir_rel}/hyperdiva/Songs" # kind of @hardCoded
    # sd "$music_dir/Songs" "${songs_dir_rel}" | \
    ##
}
##
