if isExpensive && isIReally ; then
    ##
    # function zimportzlua() {
    #     zoxide import --merge ~/.zlua
    # }
    ##
    export _ZO_DATA_DIR="$HOME/.z.dir"
    mkdir -p "$_ZO_DATA_DIR"
    if ((${+commands[zoxide]})) ; then
           eval "$(zoxide init zsh --no-aliases)" # --no-aliases: don't define extra aliases like zi, zq, za, and zr
           # `z -i` is fzf z.
    fi
    ##
    # export _ZL_ADD_ONCE=1
    # export _ZL_MATCH_MODE=1
    # antibody bundle skywind3000/z.lua
    ##
    ialiases[z]=y
    ## these are for zlua
    # ialias zz='z -c'      # restrict matches to subdirs of $PWD
    # alias zi='z -i'      # cd with interactive selection
    # ialias zf='z -I'      # use fzf to select in multiple matches
    # ialias zb='z -b'      # quickly cd to the parent directory
    ##
fi
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
    arrN ~/*(/N)
    arrN ~/base/*(/N)
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
        if test -z "$ffz_nocache" ; then
            ffz_nocache=y reval "$0" "$@"
            return $?
        else
            ecerr "$0: selection is empty even though no error was returned. This is likely to be a bug."
            return 1
        fi
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
