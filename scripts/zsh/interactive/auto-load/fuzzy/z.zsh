function list-dirs() {
    fd --follow --absolute-path --type d . "$1"
}
reify list-dirs
##
ffz_last_query=''
function ffz() {
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
    memoi_expire=$((3600*24*7)) memoi_skiperr=y serr memoi-eval list-dirs $NIGHTDIR $codedir $cellar $DOOMDIR ~/base ~/.julia ~/Downloads $music_dir
    true
 } | deusvult="${ffz_nocache:-$deusvult}" memoi_skiperr=y memoi_inheriterr=y memoi_od=0 memoi_expire=0 memoi-eval fzp "$query " | head -1)" ||  {
        return 1
    }
    if test -z "$ffz_nocache" && ! test -e "$sel" ; then
        ffz_nocache=y reval "$0" "$@"
        return $?
    fi
    cd "$sel"
}
aliasfn z ffz
aliasfn zi ffz_nocache=y ffz
