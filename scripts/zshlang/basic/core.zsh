## Core public-safe basics shared by the minimal plugin and the full local stack.
zmodload zsh/terminfo zsh/system zsh/datetime zsh/mathfunc
autoload -Uz zargs regexp-replace is-at-least colors # U: do not use aliases, z: always use zsh mode
##
alias ec='print -r --'
function ec {
    print -r -- "$@"
}
alias ecn='print -rn --'
function ecn {
    print -rn -- "$@"
}

function ec-file {
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
alias comment='\noglob :'
function comment {
    #: used as a nop for documentation
}

alias doc='\noglob :'
function doc {
    #: used as a nop for documentation
}
##
function ec_bash() {
    # deprecated. Use the alias ec.
    if [[ -n $ZSH_VERSION ]]; then
        print -r -- "$@"
    else  # bash
        echo -E -- "$@"
    fi
}
##
function return-code {
    #: @duplicateCode/62ee5b8e72bcd36de9d0e1bb405edfe5
    ##
    return "${1:-$?}"
}
##
function gquote-simple() {
    ec "${(q+@)@}"
    # @warn This doesn't quote global aliases:
    # `ec '@RET'`
    # `reval ec '@RET'`
}
function gquote() {
    # Use this to control quoting centrally.
    ##
    if (( $# == 0 )) ; then
       return 0
    fi

    # the first term can be an alias and so we do not quote it using double-quotes. The rest of the terms are not allowed to be global aliases and are all quoted using double quotes.
    ec "${(q+@)@[1]}" "${(qq@)@[2,-1]}" # @did_I_break_sth? Wed May 26 16:34:57 2021
}
function gquote-sq() {
    # uses single-quotes
    ec "${(qq@)@}"
}
function gquote-dq() {
    # uses double-quotes
    ec "${(qqq@)@}"
    ## @broken
    # `eval "$(gquote-dq ec 'hi!')" `
    # -> hi\!
    ##
}
alias gq=gquote
alias gqs=gquote-simple
alias gqd=gquote-dq
function gq() { gquote "$@" }
##
function run-on-each() {
    # Note that run-on-each won't run anything at all if no arguments are supplied
    ##

    # Use unusual name not to shadow actual vars
    local i98765 ret98765=0
    for i98765 in "${@:2}"
    do
        eval "$1 $(gquote "$i98765")" || ret98765=$?
    done
    return $ret98765
}
alias re='run-on-each'
function re() { run-on-each "$@" }

function re-sleep() {
    local secs="${1}" ; shift

    # Use unusual name not to shadow actual vars
    local i98765 ret98765=0
    for i98765 in "${@:2}"
    do
        eval "$1 $(gquote "$i98765")" || ret98765=$?
        sleep "${secs}"
    done
    return $ret98765
}

function re-any() {
    # Use unusual name not to shadow actual vars
    local i98765 ret98765=1
    for i98765 in "${@:2}"
    do
        if eval "$1 $(gquote "$i98765")" ; then
            return 0
        fi
    done
    return $ret98765
}

function run-on-each2() {
    zargs --max-lines=1 --no-run-if-empty -- "${@:2}" -- "$=1" || ecerr "ERR: $0 $(gq "$@")"
}

function re-async {
    # @alt para
    #
    # Note that run-on-each won't run anything at all if no arguments are supplied
    ##

    # Use unusual name not to shadow actual vars
    local i98765
    for i98765 in "${@:2}"
    do
        eval "$1 $(gquote "$i98765")" &
    done
}
##
function bool {
    local i="${1:l}"

    if [[ "${i}" == (n|no|0|false) ]] ; then
        return 1
    else
        test -n "${i}"
        return $?
    fi
}
##
function ecgray {
    ec "$@" >&2
}

function ecbold {
    ec "$@" >&2
}
