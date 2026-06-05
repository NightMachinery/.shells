# -*- mode: sh; sh-shell: zsh; -*-
# Minimal public helper surface for NightMachinary plugins.

if ! (( ${+aliases[@RET]} )) ; then
    alias -g '@RET'=' || return $?'
fi

if ! (( ${+aliases[@TRET]} )) ; then
    alias -g '@TRET'=' || { local retcode=$? ; ecerr "$0: exited ${retcode}" ; return $retcode }'
fi

if ! (( ${+aliases[@STRUE]} )) ; then
    alias -g '@STRUE'=' || true'
fi

function ec {
    print -r -- "$@"
}

function ecn {
    print -rn -- "$@"
}

function ecerr {
    ec "$@" >&2
}

function gquote {
    if (( $# == 0 )) ; then
       return 0
    fi

    ec "${(q+@)@[1]}" "${(qq@)@[2,-1]}"
}
alias gq=gquote

function reval {
    local cmd
    cmd="$(gquote "$@")" || return $?
    test -z "${cmd}" && return 0
    eval "${cmd}"
}

function reval-ec {
    local ec_engine="${reval_ec_ec_engine:-${reval_ec_e:-ecerr}}" eval_engine="${reval_ec_eval_engine:-eval}"
    local reval_ec_ec_engine='' reval_ec_e='' reval_ec_eval_engine=''
    test -z "$*" && return 0

    local cmd cmd_simple
    cmd="$(gquote "$@")" || return $?
    cmd_simple="${(q+@)@}"
    "${ec_engine}" "${cmd_simple}"
    "${eval_engine}" "${cmd}"
}

function reval-ecgray {
    reval_ec_ec_engine=ecerr reval-ec "$@"
}

function bool {
    local i="${1:l}"

    if [[ "${i}" == (n|no|0|false) ]] ; then
        return 1
    else
        test -n "${i}"
        return $?
    fi
}

function ensure-array {
    local i
    for i in "$@" ; do
        if (( ${#${(P)i}} == 0 )) ; then
            typeset -ag "$i"
        fi
    done
}

function isdefined-cmd {
     local sym="$1"

     test -n "$sym" && (( $+commands[$sym] ))
}

function ensure-cmd {
    local name
    for name in "$@" ; do
        if ! isdefined-cmd "${name}" ; then
            ecerr "missing command: ${name}"
            return 1
        fi
    done
}

function mkdir-m {
    local d
    for d in "$@" ; do
        command mkdir -p -- "${d}" || return $?
    done
}

function trs-rm {
    local paths=("$@")

    local p
    for p in "${paths[@]}" ; do
        if test -e "${p}" ; then
            command rm -rf -- "${p}" || return $?
        fi
    done
}

function assert {
    if (( $# == 0 )) ; then
        ecerr "$0: assert called with no arguments"
        return 1
    fi

    reval "$@" && return 0

    local ret=$?
    ecerr "$0: command failed (${ret}): $(gquote "$@")"
    return ${ret}
}

function arrN {
    print -nr -- "${(pj.\n.)@}"
}
alias arrn='arrN'

function arrNN {
    print -r -- "${(pj.\n.)@}"
}
alias arrnn='arrNN'

function tmuxnew {
    command tmux kill-session -t "$1" &> /dev/null || true
    command tmux new -d -s "$@"
}
