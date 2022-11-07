function rexx(){
    xargs -d " " -n 1 -I _ "$=1" <<< "${@:2}"
}
function rex(){
    zargs -i _ -- "${@:2}" -- "$=1"
    #Using -n 1 fails somehow. Probably a zargs bug.
}
function rexa(){
    local i
    for i in "${@:2}"
    do
        eval "$(sed -e "s/_/${i:q:q}/g" <<< "$1")" #sed itself needs escaping, hence the double :q; I don't know if this works well.
    done
}
redo-eval() {
    local i
    for i in {1.."${@: -1}"}
    do
        eval "${@: 1:-1}"
    done
}
redo() redo-eval "$(gquote "${@: 1:-1}")" "${@: -1}"
redo2() { redo "$@[2,-1]" "$1" }
function redo-async() {
    local cmd=( "$@[2,-1]" ) n="$1" i
    for i in {1..$n}; do
        reval "$cmd[@]" &
    done
}
##
function skipglob() {
    if test -n "${*[2,-1]}" ; then
        # ecdbg "$0: ${*[2,-1]}"
        eval "$1 $(gq "${@:2}")"
    fi
}
aliasfn skipemptyargs skipglob
aliasfn skipargs skipglob
function skipemptyin() {
    # whitespace doesn't matter in this application, and in fact needs to be ignored
    # local in="${$(</dev/stdin ; print -n .)[1,-2]}"
    local in="$(</dev/stdin)"
    test -z "$in" || { print -nr -- "$in" | reval "$@" }
}
aliasfn skipin skipemptyin
##
function indir {
    # we are not handling the autocompletion system at all
    local origfile="$1" dir="$(bottomdir "$1")" cmd=("${@:2}") cd_engine="${indir_cd_engine:-cdz}" origPWD=$PWD
    { test -e "$origfile" && test -d "$dir" } || {
        ##
        # ecerr "$0: '$origfile' is invalid (probably doesn't exist)"
        # return 1
        ##
        dir="$origfile" # using ffz
    }

    if test -z "$cmd[*]" ; then
        "${cd_engine[@]}" "$dir" || return $?
        return 0
    # elif [[ "$cmd[1]" == 'cd' ]] ; then
    #     "${cd_engine[@]}" "$dir" || return $?
    #     reval "${cmd[@]}"
    #     return $?
    fi

    "${cd_engine[@]}" "$dir" || return $?
    local dir_final=$PWD
    {
        reval-env "${cmd[@]}"
    } always {
        if [[ "$PWD" == "$dir_final" ]] ; then
            # if we are still in the directory that indir started in, go back to the original directory of the caller:
            cd "$origPWD"
        fi
    }
}
alias in=indir # best reserved for interactive use

function indir-mkdir {
    mkdir -p "$1" @TRET
    indir-exists "$@"
}
aliasfn indirm indir-mkdir

function indir-exists {
    @opts cd_engine cd @ indir "$@" # @todoing
}
##
typeset -ag exit_traps=( 0 INT TERM HUP EXIT ) # '0' alone seems enough though https://stackoverflow.com/questions/8122779/is-it-necessary-to-specify-traps-other-than-exit
alias trapexits='setopt localtraps ; trap "" $exit_traps[@]'
alias trapexits-release='trap - $exit_traps[@]'
function reval-notifexit() {
    # always alone is not sufficient. Test with `zsh -c 'reval-notifexit iterm_focus.py'`.
    # But now always is most probably redundant.
    trapexits
    {
        ( reval "$@" ) # the subshell allows us to terminate this command via signals normally
    } always {
        brishz bell-sc2-eradicator_destroyed
        trapexits-release
        notif "$0: $@"
    }
}
##
function reval-env() {
    test -z "$*" && return 0

    local clean_mode="$reval_env_clean" eval_engine=("${reval_env_e[@]:-eval}")
    local reval_env_clean='' reval_env_e=''
    # unsetting the input vars locally, so as to not change the defaults for inner calls to ourselves
    # example: `@opts e geval @ reval-env reval-env ec a`

    local env=()
    local i
    for i in "$@" ; do
        if [[ "$i" =~ '^([^=]*)=(.*)$' ]] ; then
            if bool $clean_mode ; then
                env+="$i"
            else
                env+="$(gq "$match[1]")=$(gq "$match[2]")"
            fi
            shift
        else
            break
        fi
    done

    local cmdhead="$1"
    local cmdbody=( "$@[2,-1]" )
    if bool $clean_mode ; then
        env -i "$env[@]" "$(realpath2 "$cmdhead")" "$cmdbody[@]"
    else
        local cmd="$(gq "$cmdhead" "$cmdbody[@]")"
        if test -n "$env[*]" ; then
            cmd="$env[*] ${cmd}"
        fi

        "$eval_engine[@]" "$cmd"
    fi
    ## tests:
    # `reval-env sth=\"67 fin='"98" !!' echo-fin`
    ##
}

function reval-env-ec {
    reval_env_e=(eval-ec) reval-env "$@"
}
function reval-ec-env {
    reval-env-ec "$@"
}

function env-clean() {
    @opts clean y @ reval-env "$@"
}
##
# function sudo() {
#     unset -f sudo
#     if [[ "$(uname)" == 'Darwin' ]] ; then
#         # @warn these corrupted /etc/pam.d/sudo on Aeirya's computer. Add the lines manually and keep the editor open. It's not trivial to fix a broken PAM file. (https://superuser.com/questions/1368246/accidentally-added-a-character-outside-of-a-comment-in-sudo-file-on-mac-sudo-no)
#         if ! command grep 'pam_tid.so' /etc/pam.d/sudo --silent; then
#             # Enables touch ID for sudo:
#             command sudo sed -i -e '1s;^;auth       sufficient     pam_tid.so\n;' /etc/pam.d/sudo
#         fi
#         if ! command grep 'pam_reattach.so' /etc/pam.d/sudo --silent; then
#             # needs https://github.com/fabianishere/pam_reattach
#             # Reattach to the user's GUI session on macOS during authentication (for Touch ID support in tmux)
#             command sudo sed -i -e '1s;^;auth     optional     pam_reattach.so\n;' /etc/pam.d/sudo
#         fi
#     fi
#     command sudo "$@"
# }
##
function sud() {
    ## test
    # alias bb='bash -c'
    # sud fin='h j a' bb 'echo $fin'
    ##
    local env=()
    local i
    for i in "$@"
    do
        if [[ "$i" =~ '^([^=]*)=(.*)$' ]]
        then
            env+="$i"
            shift
        else
            break
        fi
    done
    local cmdhead="$1"
    cmdhead=($(expand-alias-strip "$cmdhead"))
    local cmdbody=("$@[2,-1]")
    local cmd_binary
    { cmd_binary="$(realpath2 "${cmdhead[1]}")" && test -n "$cmd_binary" } || {
        local ret=$?
        ecerr "$0: realpath2 could not find '${cmdhead[1]}'"
        return $ret
    }
    revaldbg sudo "$env[@]" "$cmd_binary" "${(@)cmdhead[2,-1]}" "$cmdbody[@]"
}
function sudoify() {
    local head="$1" ; shift

    fnswap "$head" "sudo $head" "$@"
    # @tip command is also a unix command, and so can be sudoified
}
##
