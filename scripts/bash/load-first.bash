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
isI() [[ $- == *i* ]] 
