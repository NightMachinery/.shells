# DEBUGME=y
export NIGHTDIR="${0:h:h}/" # echo "_: $_ 0: $0 bs: $BASH_SOURCE"

autoload -U zargs #Necessary for scripts
autoload -U regexp-replace


comment() {

}
doc() {
    #Used for documentation
}
function ec() {
    if [[ -n $ZSH_VERSION ]]; then
        print -r -- "$@"
    else  # bash
        echo -E -- "$@"
    fi
}
function gquote() {
    doc Use this to control quoting centrally.
    ec "${(q+@)@}"
}
alias gq=gquote
function run-on-each() {
    local i
    for i in "${@:2}"
    do
        eval "$1 $(gq "$i")"
    done
}
alias re='run-on-each'
run-on-each setopt re_match_pcre extendedglob
## SSH Module
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    amSSH=remote/ssh
    # many other tests omitted
else
    case $(ps -o comm= -p $PPID) in
        sshd|*/sshd) amSSH=remote/ssh;;
    esac
fi
isSSH() test -n "$amSSH"
## Alias Module
expand-aliases() {
    doc 'See https://unix.stackexchange.com/questions/372811/better-understand-a-function-expanding-aliases-in-zsh'
    unset 'functions[_expand-aliases]'
    functions[_expand-aliases]="$@"
    (($+functions[_expand-aliases])) && ec "${functions[_expand-aliases]#$'\t'}"
}
# Forked from https://blog.sebastian-daschner.com/entries/zsh-aliases
alias-special() {
    local args=("$@[2,-1]")
    # re 'ec arg:' "$args[@]"
    builtin alias "$args[@]"
    isSSH || [[ "$args[1]" == -* ]] || [[ "$args[*]" =~ '\s*\b(.*)\b=.*' ]] &&
        {
            # ec "Setting $1 [$match[1]] to y"
            eval "$1[\$match[1]]=y"
            # naliases[$match[1]]=y
        }
}
typeset -Ag baliases
typeset -Ag ialiases
# typeset -Ag naliases #normal aliases

balias() alias-special baliases "$@"
ialias() alias-special ialiases "$@"
# alias() alias-special naliases "$@"

expand-alias-space() {
    (( $+baliases[$LBUFFER] )) ; insertBlank=$?
    (( $+ialiases[$LBUFFER] )) || { (( $+aliases[$LBUFFER] )) && zle expand-aliases-widget } #_expand_alias
    [[ "$insertBlank" = "0" ]] || zle self-insert
}
isSSH || {
    zle -N expand-alias-space
    bindkey " " expand-alias-space
    bindkey -M isearch " " magic-space
}
## Aliases
alias seval='ge_ecdbg=y geval'
## Functions
function isDarwin() { [[ "$(uname)" == "Darwin" ]] }
alias isD=isDarwin
function isLinux() { [[ "$(uname)" == "Linux" ]] }
alias isL=isLinux
function eval-dl()
{
    case "$(uname)" in
        Darwin)
            eval "$1"
            ;;
        Linux)
            eval "$2"
            ;;esac
}
function eval-darwinq()
{
    #input should be quoted.
    case "$(uname)" in
        Darwin)
            eval "${@}"
            ;;
        Linux)

            ;;esac
}
function eval-darwin()
{
    case "$(uname)" in
        Darwin)
            eval "${@:q}"
            ;;
        Linux)

            ;;esac
}
function eval-linux()
{
    case "$(uname)" in
        Darwin)

        ;;
        Linux)
            eval "${@:q}"
        ;;esac
}
function psource()
{
    if [[ -r "$1" ]]; then
        source "$1"
    fi
}


alias silent=silence
function silence() {
    { eval "$@:q"  } &> /dev/null
}
function nig() {
    #silence not interactive
    isI && eval "${@:2:q}" || "$1" "${@:2}"
}
sout() {
    { eval "$@:q" } > /dev/null
}
serr() {
    { eval "$@:q" } 2> /dev/null
}
alias nisout='nig sout'
alias niserr='nig serr'
alias nis='nig silence'

function combine-funcs() {
    # Combine multiple functions into one named by $1; The result will run all functions with $@.
    local tmp321_string="function $1() { "
    for i in "${@:2}"
    do
        tmp321_string="$tmp321_string""$i "'"$@"; '
    done
    tmp321_string="$tmp321_string""}"
    # echo "$tmp321_string"
    eval "$tmp321_string"
}
function rexx(){
    xargs -d " " -n 1 -I _ "$=1" <<< "${@:2}"
}
function rex(){
    zargs --verbose -i _ -- "${@:2}" -- "$=1"
    #Using -n 1 fails somehow. Probably a zargs bug.
}
function rexa(){
    local i
    for i in "${@:2}"
    do
        eval "$(sed -e "s/_/${i:q:q}/g" <<< "$1")" #sed itself needs escaping, hence the double :q; I don't know if this works well.
    done
}
function expand-alias {
    doc This only expands once. To expand all aliases, use 'expand-aliases'.
    if [[ -n $ZSH_VERSION ]]; then
        # shellcheck disable=2154  # aliases referenced but not assigned
        printf '%s\n' "${aliases[$1]}"
    else  # bash
        printf '%s\n' "${BASH_ALIASES[$1]}"
    fi
}
function force-expand {
    local e="$(expand-aliases "$1")"
    test -z "$e" && e="$1"
    echo "$e"
}
function ruu() {
    local f=()
    test -z "$1" || f+="$1"
    local a="$(force-expand "$2")"
    comment @lilbug strip-left
    a="$(strip "$a" 'noglob ')"
    a="$(strip "$a" 'nocorrect ')"
    seval "$f[@]" "$=a" "${@:3:q}"
}
alias noglob='noglob ruu ""'
colorfg() printf "\x1b[38;2;${1:-0};${2:-0};${3:-0}m"
colorbg() printf "\x1b[48;2;${1:-0};${2:-0};${3:-0}m"
colorb() {
    [[ "$1" =~ '^\d+$' ]] &&
        {
            colorbg "$@"
            shift 2
        } || printf %s "$bg[$1]"
    if (( $# == 1 ))
    then
        cat
    else
        ec "${@:2}"
    fi
    resetcolor
}

color() {
    [[ "$1" =~ '^\d+$' ]] &&
        {
            colorfg "$@"
            shift 2
        } || printf %s "$fg[$1]"
    if (( $# == 1 ))
    then
        cat
    else
        ec "${@:2}"
    fi
    # printf "\x1b[0m\n"
    resetcolor
}
resetcolor() printf %s "$reset_color"
helloworld() {
    colorbg 0 0 255;colorfg 0 255; ec HELLO "$(colorfg 255 100)"BRAVE"$(colorfg 0 255)" $(colorbg 100 0 255)NEW$(colorbg 0 0 255) WORLD\!;resetcolor
}
printcolors() {
    printf "\x1b[${bg};2;${red};${green};${blue}m\n"
    helloworld
    comment awk 'BEGIN{
    s="/\\/\\/\\/\\/\\"; s=s s s s s s s s;
    for (colnum = 0; colnum<77; colnum++) {
        r = 255-(colnum*255/76);
        g = (colnum*510/76);
        b = (colnum*255/76);
        if (g>255) g = 510-g;
        printf "\033[48;2;%d;%d;%dm", r,g,b;
        printf "\033[38;2;%d;%d;%dm", 255-r,255-g,255-b;
        printf "%s\033[0m", substr(s,colnum+1,1);
    }
    printf "\n";
}'
    ec 'https://github.com/johan/zsh/blob/master/Functions/Misc/colors
# Text color codes:
  30 black                  40 bg-black
  31 red                    41 bg-red
  32 green                  42 bg-green
  33 yellow                 43 bg-yellow
  34 blue                   44 bg-blue
  35 magenta                45 bg-magenta
  36 cyan                   46 bg-cyan
  37 white                  47 bg-white
# 38 iso-8316-6           # 48 bg-iso-8316-6
  39 default                49 bg-default'
}
ecdbg() {
    test -z "$DEBUGME" || {
        errcol=("${debugcol[@]:-cyan}") rederr ecerr "$@"
    }
}
fsaydbg() {
    test -z "$DEBUGME" || {
        ecdbg "$@"
        fsay "$@"
    }
}
silence eval 'export jufile=(*)'
#-------------------------------
alias zre='regexp-replace' #Change to function and add bash fallback
function strip() {
    local x="$1"
    zre x "^$2" ''
    zre x "$2"'$' ''
    ec "$x"
    # local STRING="${1##"$2"}"
    # ec "${STRING%%"$2"}"
}
#-------------------------------
isI() {
    ! test -z "$FORCE_INTERACTIVE" || [[ $- == *i* ]]
}
rgx() {
    local a
    (( $# == 2 )) && a="$(</dev/stdin)" || { a="$1" ; shift 1 }
    zre a "$1" "$2"
    ec "$a"
}

typeset -Ug path
function addToPATH {
    # case ":$PATH:" in
    #     *":$1:"*) :;; # already there
    #     *) PATH="$1:$PATH";; # org/r PATH="$PATH:$1"
    # esac
    #path[1,0]="$1"
    #path=("$1" "$path[@]")
    PATH="$1:$PATH"
    typeset -Ug path
}
function add-path {
    doc add-path NODE_PATH /some/path
    local p="$(eval 'ec $'"$1")"
    test -z "$p" && {
        eval "$1=$2:q"
    } ||
        { eval 'case ":$p:" in
        *":$2:"*) :;; # already there
        *) '"$1"'="$2:$p";; # org/r PATH="$PATH:$1"
    esac' }
    eval "export $1"
}
cdm() {
    mkdir -p -- "$1" &&
        cd -P -- "$1"
}
function bottomdir() {
    { { [ -e "$1" ]  && ! [ -d "$1" ] } || [[ "$1" != */ ]] } && { ec "${1:h}"; } || { ec "$1"; } ;}
function cdd() {
    cd "$(bottomdir "$1")" }
redo() {
    local i
    for i in {1.."${@: -1}"}
    do
        eval "$(gquote "${@: 1:-1}")"
    done
}
function printz() {
    test -n "$*" && print -rz -- "$@"
}
## END
run-on-each source "$NIGHTDIR"/zsh/basic/**/*(.)
