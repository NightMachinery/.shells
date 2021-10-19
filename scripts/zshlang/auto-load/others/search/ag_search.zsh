##
# if (( $+commands[tag-ag] )); then
#     export TAG_SEARCH_PROG=ag  # replace with rg for ripgrep
#     export TAG_CMD_FMT_STRING='nvim -c "call cursor({{.LineNumber}}, {{.ColumnNumber}})" "{{.Filename}}"'
#     agg() { command tag-ag "$@"; source ${TAG_ALIAS_FILE:-/tmp/tag_aliases} 2>/dev/null }
# fi
###
ffaliases() {
    # also see aga
    for k in "${(@k)aliases}"; do
        ec "$k=${aliases[$k]}"
    done | fz --prompt='Aliases> '
}
# alias ffa=ffaliases

alias agcommands='ec "${(F)commands}"|agC=0 rgm  --color=never'
ffcommands() {
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
function ffall() {
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
##
alias rr=rgm
alias rrn='rgm --line-number'
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
aliasfn fda fd --hidden --no-ignore #ag --unrestricted -g # search in the pathnames
function fdrp() {
    fda "$@" | inargsf re realpath
}
##
function emcrg() {
    emc-gateway -e "(night/search-dir \"$(pwd)\")"
}
# aliasfn rd emcrg
function rgbase() {
    local opts=()

    if isColorTty ; then
        opts+=( --color always )
    fi

    command rg --smart-case --colors "match:none" --colors "match:fg:255,120,0" --colors "match:bg:255,255,255" --colors "match:style:nobold" --engine auto --hidden "$opts[@]" "$@" # (use PCRE2 only if needed). --colors "match:bg:255,228,181" # This should've been on the personal list, but then it would not be loaded when needed by functions
}

function rgcontext() {
    rgbase -C ${agC:-1} "$@"
}

function rgm() {
    fnswap isColorTty true rgcontext --heading "$@" | less-min
}

agm() rgm "$@" #alias agm='rg' #'ag -C "${agC:-1}" --nonumbers'

aga() {
    # agm "$@" "$NIGHTDIR"/**/*alias*(.)
    builtin alias|agm "$@"
}
ags() {
    agm "$@" ~/.zshenv ~/.zshrc "$NIGHTDIR"/**/*(.) ~/.bashrc ~/.profile ~/.bashrc ~/.bash_profile
}
agf() {
    ags "$@"'\s*\(\)'
}
agi() {
    doc ag internals of zsh
    agm "$@" ~/.oh-my-zsh/ $plugin_dir
}
agcell() {
    agm -uuu --iglob '!.git' "$@" $cellar # --binary --hidden don't work with -C somehow, so we use -uuu :D
}
##
agrdry() {
    agm -F  -- "$from" "${@}"
}
function agr {
    doc 'Use https://github.com/facebookincubator/fastmod instead?'
    doc 'usage: from=sth to=another agr [ag-args]'
    comment -l --files-with-matches

    local opts=()
    if test -z "$agr_regex" ; then
        opts+='--literal'
    fi
    ag -0 -l "$opts[@]" -- "$from" "${@}" | pre-files "$from" "$to"
}
##
function pathtree-search {
    ##
    if isColor && isOutTty
    then
        ugrep_opts+='--color=always'
    fi

    ugbase_follow=y revaldbg indir pathtree ugbool "$@" < /dev/null | prefixer -o $'\n\n' # closing the stdin forces ugrep to search in the file tree
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
