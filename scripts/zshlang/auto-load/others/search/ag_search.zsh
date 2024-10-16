##
# if (( $+commands[tag-ag] )); then
#     export TAG_SEARCH_PROG=ag  # replace with rg for ripgrep
#     export TAG_CMD_FMT_STRING='nvim -c "call cursor({{.LineNumber}}, {{.ColumnNumber}})" "{{.Filename}}"'
#     agg() { command tag-ag "$@"; source ${TAG_ALIAS_FILE:-/tmp/tag_aliases} 2>/dev/null }
# fi
###
function ffaliases {
    # also see aga
    for k in "${(@k)aliases}"; do
        ec "$k=${aliases[$k]}"
    done | fz --prompt='Aliases> '
}
# alias ffa=ffaliases

alias agcommands='ec "${(F)commands}" | agC=0 rgm  --color=never'
function ffcommands {
    # cmf (previous name)
    # command finder
    printz "$(agcommands "${@:-.}" | fz --prompt 'Commands> ')"
}
# alias ffc=ffcommands

alias agfunc='ec "${(Fk)functions}"| agC=0 rgm  --color=never'
fffunctions() {
    printz "$(agfunc "${@:-.}" | fz --prompt 'Functions> ')"
}
# alias ff=fffunctions
##
function ffall {
    # @alt agfi
    local query="$(fz-createquery "$@")"

    {
        ec "${(Fk)functions}"
        ec "${(Fk)aliases}"
        ec "${(Fk)commands}"
        ec "${(Fk)builtins}"
    } | fzp "$query"
}
alias ffa=ffall

function function-rg {
    {
        print -r -- "${(Fk)functions}"
        print -r -- "${(Fk)aliases}"
    } | rg "$@"
}
alias frg='function-rg'
##
function rg-literal-or {
    ensure-array rg_literal_or_opts
    local patterns=("$@") opts=("${rg_literal_or_opts[@]}")
    local engine=("${rg_literal_or[@]:-rg}")
    # ugrep also supported, slower though. (Especially as rg seems to go instant with cache, while ugrep doesn't benefit as much.)
    #   You might want to use '--dereference-recursive' with ugrep.

    local opts_pats=() i
    for i in "$patterns[@]" ; do
        opts_pats+=(-e "$i")
    done
    revaldbg "$engine[@]" --fixed-strings "$opts_pats[@]" "$opts[@]"
}
##
aliasfn fda fd --hidden --no-ignore # ag --unrestricted -g # search in the pathnames
function fdrp {
    fda "$@" | inargsf re 'grealpath --'
}
##
function emc-search {
    local d="${emc_search_d:-$PWD}" q="${*}"
    assert-args d @RET

    local cmd
    if false ; then
        cmd=(emc-gateway -e)
    else
        cmd=(awaysh-fast emc-eval)
        emc-focus
    fi

    reval-ec "${cmd[@]}" "(night/search-dir :dir $(lisp-quote "$d") :query $(lisp-quote "$q"))"
}
aliasfn sees-emc emc-search
# aliasfn rd emc-search

function rgbase {
    local opts=()

    if isColorTty ; then
        opts+=( --color always )
    fi

    command rg --no-ignore-global --smart-case --colors "match:none" --colors "match:fg:255,120,0" --colors "match:bg:255,255,255" --colors "match:style:nobold" --engine auto --hidden "$opts[@]" "$@"
    #: --engine auto: use PCRE2 only if needed
    #: --colors "match:bg:255,228,181"
}
alias rr='rgbase'
alias rrn='rr --line-number'
# alias rr=rgm
# alias rrn='rgm --line-number'

function rgcontext {
    rgbase -C ${agC:-1} "$@"
}

function rgm {
    local color=false
    if isColorTty ; then
        color=true
    fi

    fnswap isColorTty "$color" rgcontext --heading "$@" | less-min
}
# function rg-less {
#     rgm "$@" | less-min
# }
# aliasfn rgl rg-less

function agm {
    rgm "$@" #alias agm='rg' #'ag -C "${agC:-1}" --nonumbers'
}

function aga {
    # agm "$@" "$NIGHTDIR"/**/*alias*(.)
    builtin alias|agm "$@"
}
function ags {
    agm "$@" ~/.zshenv ~/.zshrc "$NIGHTDIR"/**/*(.) ~/.bashrc ~/.profile ~/.bashrc ~/.bash_profile
}
function agf {
    ags "$@"'\s*\(\)'
}
function agi {
    doc ag internals of zsh
    agm "$@" ~/.oh-my-zsh/ $plugin_dir
}
function agcell {
    agm -uuu --iglob '!.git' "$@" $cellar # --binary --hidden don't work with -C somehow, so we use -uuu :D
}
##
function agrdry {
    agm -F  -- "$from" "${@}"
}

function agr {
    doc 'Use https://github.com/facebookincubator/fastmod instead?'
    doc 'usage: from=sth to=another agr [ag-args]'
    comment -l --files-with-matches

    local opts=()
    if ! bool "$agr_regex" ; then
        opts+='--literal'
    fi
    command ag -0 -l --ignore-case "$opts[@]" -- "$from" "${@}" | pre-files "$from" "$to"
}
##
function pathtree-search {
    ##
    if isColor && isOutTty
    then
        ugrep_opts+='--color=always'
    fi

    ugbase_follow=y revaldbg indir pathtree ugbase --bool -- "$*" ./ <&- | prefixer -o $'\n\n' # closing the stdin forces ugrep to search in the file tree
}
noglobfn pathtree-search
alias pts='\noglob pathtree-search'

function pathtree-video {
    pathtree-search "\.${(j/|\./)video_formats} $*"
}
noglobfn pathtree-video

function ptv {
    jcolor pathtree-video "$@"
}
noglobfn ptv
##
