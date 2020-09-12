zmodload zsh/terminfo zsh/system zsh/datetime
autoload -U zargs
autoload -U regexp-replace
## Aliases
alias ec='print -r --'
alias ecn='print -rn --'
## Global Aliases
## Vars
zshword='[a-zA-Z0-9!_-]' #unused, I opted for simpler solutions
##

alias doc='\noglob :'
alias comment='\noglob :'
# comment() {

# }
# doc() {
#     #Used for documentation
# }
function uuidpy() {
    python3 -c 'import uuid ; print(uuid.uuid4().hex)'
}
function uuidm() {
    doc "This is the official interface to create new UUIDs"
    uuidgen | gtr -d '-' # '-' causes problems with some usages
}
function md5m() {
    print -nr -- "$1" | md5sum | awk '{print $1}' || {
        echo "Could not get md5 of '$1'" >&2
        return 1
    }
}
function ec_bash() {
    doc deprecated. Use the alias ec.
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
    # doc "Note that run-on-each won't run anything at all if no arguments are supplied"
    # doc Use unusual name not to shadow actual vars
    local i98765
    for i98765 in "${@:2}"
    do
        eval "$1 $(gquote "$i98765")"
    done
}
alias re='run-on-each'
function re-async() {
    # doc "Note that run-on-each won't run anything at all if no arguments are supplied"
    # doc Use unusual name not to shadow actual vars
    local i98765
    for i98765 in "${@:2}"
    do
        eval "$1 $(gquote "$i98765")" &
    done
}
run-on-each setopt re_match_pcre extendedglob pipefail
run-on-each unsetopt autopushd
