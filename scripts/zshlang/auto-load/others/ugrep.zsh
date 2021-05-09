typeset -ag ugrep_opts=(--bool --smart-case --sort=best --no-confirm --perl-regexp --hidden --binary-files=without-match)
##
function ugbase() {
    local sel ret opts=()
    if isOutTty  ; then
        opts+='--color=always'
    fi
    sel="$(command ugrep "$ugrep_opts[@]" "$opts[@]" "$@")"
    # don't add pretty as it adds line numbers to the output
    ret=$?

    if test -n "$sel" ; then # exit status of ugrep is 1 when we use ESC to exit
        ec "$sel"
        return 0
    else
        return $ret
    fi
}
##
function ugbool() {
    ugbase --bool "$*"
}
alias ugb='ugbool'
##
function ug-i() {
    ugbase --query=1 "$@"
}
function ugf() {
    ug-i --regexp="$*"
}
function ugm() {
    # https://github.com/Genivia/ugrep/issues/31
    ug-i --pretty --context=3 --regexp="$*" --recursive
    # --fuzzy=3 (not compatible with PCRE)
    #  --break  Adds a line break between results from different files.
    #  --heading, -+ Group matches per file.  Adds a heading and a line break between results from different files.
}
##
function ugnt() {
    local i args=()
    for i in "$note_formats[@]" ; do
        args+=( -O "$i" )
    done
    ugm "$args[@]" "$@" $nightNotes/
}
###
##
# @warn ugrep does not support read0: https://github.com/Genivia/ugrep/issues/113
# So we just do a hack:
function ugfz-i0() {
    prefixer -i '\x00' -o $'\n' | ugfz "$@"
}
function ugfz-i0o0() {
    prefixer -i '\x00' -o $'\n' | ugfz "$@" | prefixer -i $'\n' -o '\x00'
}
function ugfz-o0() {
    ugfz "$@" | prefixer -i $'\n' -o '\x00'
}
##
function ugfz() {
    local opts=("${@[1,-2]}") INITIAL_QUERY="${@[-1]}"
    local input
    input="$(cat)" @RET

    local tmp
    tmp="$(gmktemp)" @RET
    ecn "$input" > "$tmp"

    local ugrep_opts=("$ugrep_opts[@]")
    ## Add line numbers to output:
    # opts+=( --delimiter ':' --with-nth 2..)
    # ugrep_opts+="--format='%n:%O%~'"
    ##
    if isI ; then
        ugrep_opts+='--color=always' # gets stripped by fzf if interactive
    else
        ugrep_opts+='--color=never'
    fi
    if true || ! isDbg ; then
        # @warn @untested Not adding this might break an otherwise fine execution
        ugrep_opts+='--no-messages'
    fi

    RG_PREFIX="cat $(gq "$tmp") | ugrep $ugrep_opts[*] -e"

    local FZF_DEFAULT_COMMAND="$RG_PREFIX $(gq "${INITIAL_QUERY}")" # Not exported as we are just feeding fzf on stdin ourselves
    if isI ; then
        if test -n "$INITIAL_QUERY" ; then
            opts+=( --query "$INITIAL_QUERY " )
        fi

        eval "$FZF_DEFAULT_COMMAND" |
            FZF_SIMPLE_PREVIEW='cat {f}' \
                fz --bind "change:reload:$RG_PREFIX {q} || true" ${opts[@]}  --ansi --disabled
    else
        eval "$FZF_DEFAULT_COMMAND" | {
            sponge || true # no idea why sometimes this can fail 141
        } || {
            ecerr "$0: failed ${(@j.|.)pipestatus}"
            return 1
        }
    fi
}
###
