# DEBUGME=y
autoload -U regexp-replace
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
    if [[ -r $1 ]]; then 
        source $1 
    fi 
} 


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

function run-on-each() {
    local i
    for i in "${@:2}"
    do
        eval "$1 $i:q"
    done
}
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
    if [[ -n $ZSH_VERSION ]]; then
        # shellcheck disable=2154  # aliases referenced but not assigned
        printf '%s\n' "${aliases[$1]}"
    else  # bash
        printf '%s\n' "${BASH_ALIASES[$1]}"
    fi
}
function force-expand {
    local e="$(expand-alias "$1")"
    test -z "$e" && e="$1"
    echo "$e"
}
function ruu() {
    local a="$(force-expand "$2")"
    a="$(strip "$a" 'noglob ')"
    "$1" "$=a" "${@:3}"
}
function geval() {
    local cmd="$@"
    ec "$cmd"
    print -r -S -- "$cmd" #Add to history
    eval -- "$cmd"
}
function ec() {
    if [[ -n $ZSH_VERSION ]]; then
        print -r -- "$@"
    else  # bash
        echo -E -- "$@"
    fi
}
ecerr() ec "$@" 1>&2
function rederr() {
	  (setopt nomultios 2>/dev/null; set -o pipefail;
     # eval "$@:q" 2>&1 1>&3|sed $'s,.*,\e[31m&\e[m,'1>&2
     eval "$@:q" 2>&1 1>&3|color "${errcol:-red}" 1>&2
    )3>&1
}
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
comment() {
    
}
doc() {
    #Used for documentation
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
        errcol="${debugcol:-cyan}" rederr ecerr "$@"
    }
}
fsaydbg() {
    test -z "$DEBUGME" || {
        ecdbg "$@"
        fsay "$@"
    }
}
alias re='run-on-each'
silence eval 'export jufile=(*)'
#-------------------------------
setopt re_match_pcre
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
	local a="$1"
	zre a "$2" "$3"
	ec "$a"
}


