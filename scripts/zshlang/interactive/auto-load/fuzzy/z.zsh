function list-dirs() {
    local d="${1:?}" multi_fs="${list_dirs_m}" # multi = disable one-file-system

    if ! test -d "$d" ; then
        return 1
    fi
    local opts=()
    if test -z "$multi_fs" ; then
        opts+="--one-file-system"
    fi
    fd "$opts[@]" --follow --absolute-path --type d . $d
}
reify list-dirs
##
ffz_last_query=''
function ffz() {
    ## Performance:
    # Note that piping a lot of stuff into fzp itself seems to be slow, even when the result is cached. To be sure, control the amount of feeded dirs, and run 'deus z' to refresh the caches. Note that older clients might poison the caches again ...
    ##
    # @retiredtodo2 We can bypass the interactive selection if the score of the top match is high enough compared to the second-best match, and have a `zi` that disables this auto-bypass.
    # DONE: We can also just cache the result for each query!
    ##
    setopt localoptions pipefail
    local query="${*:-$ffz_last_query}" sel
    ffz_last_query="$query"
    ##
    local fz_opts=( $fz_opts[@] --prompt "Z> ")
    ##
    # memoi-eval doesn't read from pipe
    sel="$( {
    serr zoxide query --list
    arrN /Volumes/*(/N)
    arrN /Volumes/*/*(/N)
    # list-dirs ~/base/cache ~/base/Lectures ~/base/series ~/base/anime ~/"base/_Local TMP" ~/base/docu ~/base/movies ~/base/V ~/base/dls ~/Downloads # takes ~0.2s
    memoi_expire=$((3600*24*7)) memoi_skiperr=y serr memoi-eval list-dirs $NIGHTDIR $codedir $cellar $DOOMDIR ~/base ~/.julia $music_dir ~/Downloads
    true
 } | { sponge || true } | deusvult="${ffz_nocache:-$deusvult}" memoi_key=fuzzy_z memoi_skiperr=y memoi_inheriterr=y memoi_od=0 memoi_expire=0 memoi-eval fzp "$query " | head -1)" ||  {
        local r=$? msg="$0: returned $? ${pipestatus[@]}"
        ecerr $msg
        return $r
    }
    if test -z "$sel" ; then
        ecerr "$0: selection is empty even though no error was returned. This is likely to be a bug."
        return 1
    fi
    sel="$(ntag-recoverpath "$sel")"
    if test -z "$ffz_nocache" && ! test -e "$sel" ; then
        ffz_nocache=y reval "$0" "$@"
        return $?
    fi
    cd "$sel"
}
aliasfn z ffz
aliasfn zi ffz_nocache=y ffz
