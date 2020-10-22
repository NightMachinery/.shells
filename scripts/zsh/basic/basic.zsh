zmodload zsh/terminfo zsh/system zsh/datetime
autoload -Uz zargs regexp-replace is-at-least colors # U: do not use aliases, z: always use zsh mode
##
alias ec='print -r --'
alias ecn='print -rn --'
function ec-file() {
    local target="${ec_file_target:/dev/tty}"
    if test -w "$target" ; then
        ec "$@" >> "$target"
    else
        return 1
    fi
}
function ec-tty() { ec_file_target=/dev/tty ec-file "$@" } # echoes directly to the terminal. Survives $() or silent.
ectty() ec-tty "$@"
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

    ##
    # You need to `gtr -d '\n'` on bigger outputs
    xxd -l 16 -p /dev/urandom
    ## Alt:
    # uuidgen | gtr -d '-' # '-' causes problems with some usages
    ##
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
setopt re_match_pcre extendedglob pipefail hash_executables_only # hash_executables_only will not hash dirs instead of executables, but it can be slow.
unsetopt autopushd
