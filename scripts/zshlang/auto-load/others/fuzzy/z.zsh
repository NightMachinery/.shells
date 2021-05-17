typeset -g ZDIRS_ENABLED=y
typeset -g ZDIRS_NAME=zdirs
function z-add() {
    if test -z "$HISTFILE" ; then
        return 0
    fi

    local dir="${1}"
    if test -n "$dir" ; then
        redism SADD "$ZDIRS_NAME" "$dir"
    fi
}
function z-add-pwd() {
    inbg silence z-add "$PWD"
    # the 'silent' alias is weirdly not defined in the completion context (test with `in x ll y<TAB>`)
}
function z-list() {
    redism SMEMBERS "$ZDIRS_NAME"
}
##
export _ZO_DATA_DIR="$HOME/.z.dir"
if ! bool "$ZDIRS_ENABLED" && isExpensive && isIReally ; then
    ##
    # function zimportzlua() {
    #     zoxide import --merge ~/.zlua
    # }
    ##
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
function ffz-get() {
    ## Performance:
    # Note that piping a lot of stuff into fzp itself seems to be slow, even when the result is cached. To be sure, control the amount of feeded dirs, and run 'deus z' to refresh the caches. Note that older clients might poison the caches again ...
    ##
    # @retiredtodo2 We can bypass the interactive selection if the score of the top match is high enough compared to the second-best match, and have a `zi` that disables this auto-bypass.
    # DONE: We can also just cache the result for each query!
    ##
    setopt localoptions pipefail interactivecomments

    local query="${*}" cd_mode="${ffz_cd}" redis_dict='zdirs_choices' nocache="$ffz_nocache"
    if test -z "$query" ; then
        query="$ffz_last_query"
        nocache=y
    else
        ffz_last_query="$query"
    fi
    local redis_key="$query"

    local sel
    if test -z "$nocache" && test -n "$redis_key" && silence redism hexists "$redis_dict" "$redis_key" ; then
        sel="$(redism hget "$redis_dict" "$redis_key")" @TRET
    else
        local fz_opts=( $fz_opts[@] --prompt "Z> ")

        sel="$( {
            if bool "$ZDIRS_ENABLED" ; then
            tty-title zdirs
            z-list
            else
                tty-title zoxide
                serr zoxide query --list
            fi
            tty-title ffz

            arrN ~/*(/N)
            arrN ~/base/*(/N)
            arrN /Volumes/*(/N)
            arrN /Volumes/*/*(/N)

            list_dirs_d=3 list-dirs "$PWD"
            # 'list_dirs_d=3 time2 silence list-dirs ~/' takes 0.12s, using depth=4 takes 0.28s

            list-dirs ~/base/cache ~/base/Lectures ~/base/series ~/base/anime ~/"base/_Local TMP" ~/base/docu ~/base/movies ~/base/V ~/base/dls ~/Downloads # takes ~0.2s
            memoi_expire=$((3600*24*7)) memoi_skiperr=y serr memoi-eval list-dirs $NIGHTDIR $codedir $cellar $DOOMDIR ~/base ~/.julia $music_dir ~/Downloads
            true
            } | fzp "$query " | ghead -n 1 || retcode)" ||  {
            # local r=$? msg="$0: $(retcode 2>&1)"
            # ecerr $msg
            return $r
        }

        assert silence redism hset "$redis_dict" "$redis_key" "$sel" @RET
    fi

    if test -z "$sel" ; then
        if test -z "$nocache" ; then
            ffz_nocache=y reval "$0" "$@"
            return $?
        else
            ecerr "$0: selection is empty even though no error was returned. This is likely to be a bug."
            return 1
        fi
    fi
    sel="$(ntag-recoverpath "$sel")"
    if test -z "$nocache" && ! test -e "$sel" ; then
        ffz_nocache=y reval "$0" "$@"
        return $?
    fi

    if test -n "$cd_mode" ; then
        cd "$sel"
    else
        ec "$sel"
    fi

}
@opts-setprefix ffz-get ffz

function ffz() {
    ffz_cd=y ffz-get "$@"
    # avoid forking here, ffz-get needs to save 'ffz_last_query'
}
aliasfn z ffz
aliasfn zi ffz_nocache=y ffz
##
function ffz-b() {
    # z-back
    local q="$*"

    local o
    o="$(list-dirs-parents "$PWD" | fzp "$q" | ghead -n 1)" @RET
    cd "$o"
}
alias zb='ffz-b'

function ffz-r() {
    # z-recursive
    local q="$*"

    local o
    o="$(@opts fd -uu @ list-dirs "$PWD" | fzp "$q" | ghead -n 1)" @RET
    cd "$o"
}
alias zx='ffz-r'
##
