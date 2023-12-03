### Vars
FZF_SIMPLE_PREVIEW='printf -- "%s " {}'
# fzf supports wrapping itself. # | command fold -s -w $FZF_PREVIEW_COLUMNS'
# << was bad for dash, no <<< in dash
##
if isRtl ; then
    FZF_RTL_PREVIEW="$FZF_SIMPLE_PREVIEW"
else
    # FZF_RTL_PREVIEW='printf -- "%s " {} | rtl_reshaper.py'
    FZF_RTL_PREVIEW='printf -- "%s " {} | rtl_reshaper.dash'
fi
##
FZF_SIMPLE_PREVIEW="$FZF_RTL_PREVIEW"
##
FZF_CAT_PREVIEW='cat {f}'
if isRtl ; then
    FZF_CAT_RTL_PREVIEW="$FZF_CAT_PREVIEW"
else
    FZF_CAT_RTL_PREVIEW='cat {f} | rtl_reshaper.dash'
fi
##
# FZF_PREVIEW_NTAG="brishz_in={} brishzq.zsh ntag-color"
FZF_PREVIEW_NTAG="$FZF_SIMPLE_PREVIEW | ntagcolor"
##
### Functions
aliasfn fz-grep fz --no-sort --filter #Filter mode. Do not start interactive finder. When used with  --no-sort,  fzf becomes  a fuzzy-version of grep. # Just fz -f if you don't want the nosort.

function fz-empty() {
    fz_empty='y' fz "$@"
}
function fz {
    # "Use `fnswap fzf-gateway gq fz` to get the final command for use in other envs
    local opts emptyMode="${fz_empty}"
    opts=(${(@)fz_opts} --exit-0) #Don't quote it or it'll insert empty args
    # --exit-0 : By using this we'll lose the automatic streaming feature of fzf as we need to wait for the whole input. (Update: It doesn't seem that the streaming feature is useful at all, as it doesn't show anything until completion in my tests ...)
    # --select-1 : auto-selects if only one match
    test -n "$fz_no_preview" || opts+=(--preview "$FZF_SIMPLE_PREVIEW" --preview-window up:7:wrap:nohidden:nocycle)

    local cmdbody=( "${(@)opts}" "$@" )
    if test -z "$emptyMode" ; then
        fzf-noempty "$cmdbody[@]" # moved options to FZF_DEFAULT_OPTS
    else
        fzf-gateway "$cmdbody[@]"
    fi
}

function fzf-noempty {
    local in="$(</dev/stdin)" # So we need to wait for the whole input to finish first.
    test -z "$in" && { return 130 } || { ecn "$in" | fzf-gateway "$@" }
}

function fzf-gateway() {
    local -x SHELL="${FZF_SHELL:-${commands[dash]}}"

    bella_zsh_disable1

    if true ; then # we might want to check tmux's version here, as fzf-tmux needs the current HEAD
        if test -z "$fzf_mru_context" ; then
            if ! isTmux || isKitty || ! isI ; then
                # @kittyBug?
                command fzf "$@" | sponge
            else
                # fzf-tmux is generally a bit buggy and weird, take care not to use it in non-interactive cases (i.e., with --filter)
                command fzf-tmux -p90% "$@" | sponge
                # sponge is necessary: https://github.com/junegunn/fzf/pull/1946#issuecomment-687714849
            fi
        else
            if isI ; then
                fzf_mru.sh "$@"
            else
                FORCE_NONINTERACTIVE=y fzf_mru.sh "$@"
            fi
        fi
    else
        fzf "$@"
    fi
}

function fzp() {
    # fzp: fz potentially

    # 'fzp_ug' is used in pther places. Do NOT rename it.
    local opts=("${@[1,-2]}") query="${@[-1]}" disallowNI="${fzp_dni}" ugrepMode="${fzp_ug}"
    local fzp_ug='' # redundant, but this makes sure we won't have an infinite loop in the future

    local ugfz_cmd='ugfz'
    if [[ "$ugrepMode" == (i0|o0|i0o0) ]] ; then
        ugfz_cmd="ugfz-$ugrepMode"
    fi


    # FNSWAP: isI
    if isI ; then
        if test -n "$ugrepMode" ; then
            "$ugfz_cmd" "$opts[@]" "$query" @RET
        else
            fz "$opts[@]" --query "$query" @RET
        fi
    else
        ## tests:
        # `fd | fzp_ug=y fzp_dni=truncate fnswap isI false fzp`
        ##
        local truncateMode=''
        if test -n "$disallowNI" ; then
            if [[ "$disallowNI" == truncate ]] ; then
                truncateMode=100
            else
                ecerr "$0: Non-interactive usage has been explicitly forbidden."
                return 1
            fi
        fi
        {
            if test -n "$ugrepMode" ; then
                ensure-dbg "$ugfz_cmd" "$opts[@]" "$query" @MRET
                # revaldbg "$ugfz_cmd" "$opts[@]" "$query" @RET
            else
                fz --no-sort "$opts[@]" --filter "$query" @RET
            fi
        } | {
            if test -n "$truncateMode" ; then
                ghead -n "$truncateMode"
            else
                cat
            fi
        } || {
            ecdbg "fzp  failed: ${(j.|.)pipestatus[@]}"
            return 1
        }
    fi
}
##
function fz-masked {
    : "GLOBAL sel_i"
    unset sel_i
    local masks="$(cat)" input="${@[-1]}" opts=("${@[1,-2]}")

    sel_i="$(ecn "${masks}" | cat -n | FZF_SIMPLE_PREVIEW="${FZF_CAT_PREVIEW}" fz --with-nth 2.. "$opts[@]" | gawk '{print $1}')" @RET
    gawk 'NR == FNR {nums[$1]; next} FNR in nums' <(ec "$sel_i") <(ec "$input")
    sel_i=(${(@f)sel_i})
}
##
function fzf-exact-p {
    [[ "${FZF_DEFAULT_OPTS}" =~ '--exact\b' ]]
}
##
function fzp-q {
    local query="$(fz-createquery "$@")"

    fzp "$query"
}

function fz-q {
    local query="$(fz-createquery "$@")"

    fz --query "$query"
}

function rg-createquery() {
    local i res=''
    for i in ${@} ; do
        res+=".*$i"
    done
    ec $res
}

function fz-createquery {
    local ugrepMode="$fzp_ug"
    if [[ "$fzp_ug" == ni ]] ; then
        if isI ; then
            ugrepMode=''
        else
            ugrepMode='y'
        fi
    fi
    
    local i res=''
    for i in ${=@} ; do
        # ! is exact-match by default.
        if [[ "$i" =~ "^(\^|\!|'|\|)" || "$i" =~ '\$$' ]] ; then
            if test -z "$ugrepMode" ; then
                res+="$i "
            else
                local s="$i[1]" r="${i[2,-1]}"
                if [[ "$s" == '!' ]] ; then
                    res+="-$r "
                elif [[ "$s" == "'" ]] ; then
                    res+="$r "
                else
                    # ^, $, | are valid regex
                    res+="$i "
                fi
            fi
        else
            local exact_prefix=''
            if ! fzf-exact-p ; then
                exact_prefix="'"
            fi

            if test -z "$ugrepMode" ; then
                res+="${exact_prefix}$i "
            else
                res+="$i "
            fi
        fi
    done
    ec "$res" |
        perl -lpe 's/\\ /\\\\ /g'
        #: Whitespace needs to be quoted separately for fzf, so to match =\ =, we =\\ =.
}
##
function cat-fdz-if-tty {
    if (( $# ))
    then
        arrN "$@"
    else
        if isInTty ; then
            fd | fz
        else
            cat
        fi
    fi
}
##
