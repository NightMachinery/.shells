typeset -ag ugrep_opts=(--bool --smart-case --sort=best --no-confirm --perl-regexp --hidden --binary-files=without-match)
# --dereference-recursive : recursive dirs, follows symlinks

function ugrep-c-get {
    ec "${ugrep_c:-3}"
}
##
function ugbase() {
    local follow_sym="${ugbase_follow}" rtl="${ugbase_rtl:-y}"

    local sel ret opts=()

    if isColorTty ; then
        opts+='--color=always'
    fi

    if bool "$follow_sym" ; then
        opts+='--dereference-recursive'
        # @warn this causes ugrep to search files given on stdin!
    fi

    sel="$(command ugrep "$ugrep_opts[@]" "$opts[@]" "$@")"
    # don't add pretty as it adds line numbers to the output
    ret=$?

    if test -n "$sel" ; then # exit status of ugrep is 1 when we use ESC to exit
        if bool "$rtl" && isColorTty ; then
            ec "$sel" | rtl-reshaper
        else
            ec "$sel"
        fi
        return 0
    else
        return $ret
    fi
}
##
function ugbool() {
    local q="${*:-.}"

    ugbase --bool -- "$q"
}
noglobfn ugbool
alias ugb='\noglob ugbool'

function ugbool-context {
    ugrep_opts=("$ugrep_opts[@]" --context="$(ugrep-c-get)") ugbool "$@"
}
alias ugbc='ugbool-context'
##
function ug-i() {
    : "oneliner: ugrep --bool --smart-case '--sort=best' --no-confirm --perl-regexp --hidden '--binary-files=without-match' --query=1"
    ##
    ugbase --query=1 "$@"
}
function ugf() {
    ug-i --regexp="$*"
}
function ug-ic() {
    # ug-i context

    local r="${@[-1]}" opts=("${@[1,-2]}")
    
    # https://github.com/Genivia/ugrep/issues/31
    revaldbg ug-i --pretty --context="$(ugrep-c-get)" --regexp="$r" --recursive "$opts[@]"
    # --fuzzy=3 (not compatible with PCRE)
    #  --break  Adds a line break between results from different files.
    #  --heading, -+ Group matches per file.  Adds a heading and a line break between results from different files.
}
function ugc {
    # non-interactive ugrep with context

    local r="${@[-1]}" opts=("${@[1,-2]}")

    ugrep --heading --color=always --pretty --context="$(ugrep-c-get)" --recursive --bool --smart-case '--sort=best' --no-confirm --perl-regexp --hidden '--binary-files=without-match' "$opts[@]" --regexp="$r" | less -n
}

function ugm() {
    ug-ic "$*"
}
##
function ugnt() {
    local i args=()
    
    # for i in "$note_formats[@]" ; do
    #     args+=( -O "$i" )
    # done
    
    ug-ic "$args[@]" $nightNotes/ "$*"
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
function ugrep_get {
    local format="${ugrep_get_format:-%[1]#%~}"
    # format='%f%s%n%s%[1]#' # path+linenumber+match

    ugrep "$ugrep_opts[@]" --perl-regexp --format="$format" "$@"
    # `--colors='hl'` doesn't work with `--format`.
}
##
