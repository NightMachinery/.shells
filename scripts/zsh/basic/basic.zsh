autoload -U zargs #Necessary for scripts
autoload -U regexp-replace
## Aliases
alias ec='print -r --'
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
    doc Use unusual name not to shadow actual vars
    local i98765
    for i98765 in "${@:2}"
    do
        eval "$1 $(gq "$i98765")"
    done
}
alias re='run-on-each'
run-on-each setopt re_match_pcre extendedglob
