###
function in-or-args2 {
    in_or_args_in_p=''

    if (( $# )) ; then
        inargs=( "$@" )
    else
        ##
        in_or_args_in_p=y
        inargs="${$(in-or-args ; print -n .)[1,-2]}" @RET
        ##
        # if ! isInTty ; then

        #     inargs="${$(</dev/stdin ; print -n .)[1,-2]}"
        # else
        #     return 1
        # fi
        ##
    fi
}

function in-or-args3 {
    in-or-args2 "$@" @RET
    if bool "$in_or_args_in_p" ; then
        inargs=("${(@f)inargs}")
    fi
}


function in-or-args-pns {
    in_or_args_pns_p=y in-or-args "$@"
}

function in-or-args {
    local end_newline_p="${in_or_args_newline_p-y}"
    local pns_p="${in_or_args_pns_p}"
    local html_p="${in_or_args_html_p}"

    if (( $# )) ; then
        if test -n "$end_newline_p" ; then
            arrNN "$@"
        else
            arrN "$@"
        fi
    else
        if isInTty ; then
            if bool "$html_p" ; then
                pbpaste-html
            elif bool "$pns_p" ; then
                p-newline2space
            else
                pbpaste
            fi
        else
            ##
            #: @idk why we ever used this  ¯\_(ツ)_/¯
            # print -nr -- "${$(</dev/stdin ; print -n .)[1,-2]}"
            ##
            cat
            ##
        fi
    fi
}
##
function pcat {
    possiblycat "${@:-50}"
}
##
function arr0() {
    print -nr -- "${(pj.\0.)@}"
}

function arrN() {
    print -nr -- "${(pj.\n.)@}"
}
alias arrn='arrN'
function arrNN() { print -r -- "${(pj.\n.)@}" }
alias arrnn='arrNN'

# function in-or-args-arr0() {
#     (( $# )) && arr0 "$@" || ec "$(</dev/stdin)"
# }
# function in-or-args-arrN() {
#     (( $# )) && arrN "$@" || ec "$(</dev/stdin)"
# }
##
function head-tail {
    local from="${1}" to="${2}"
    assert-args from to @RET

    integer len=$((to - (from - 1)))
    head -n "${to}" | tail -n "${len}"
}
##
function ensure-ends-with-newline {
    #: Ensures the input ends in a newline
    ##
    local in
    in="$(cat)" || return $?
    ec "$in"
}
alias ensure-nl='ensure-ends-with-newline'
alias enl='ensure-nl'
###
function file-unix2uri {
    in-or-args "$@" | prefixer --skip-empty --add-prefix="file://"
}

function file-unix2uri-rp {
    local inargs
    inargs="$(in-or-args "$@")" @RET

    local f
    for f in ${(@f)inargs} ; do
        ec "file://$(grealpath -- "$f")" @TRET
    done
}

function file-uri2unix {
    local f="$1"
    # assert-args f @RET

    if [[ "$f" =~ '^file://[^/]*(/.*)$' ]] ; then
        ec "$match[1]" | url-decode.py @TRET
    else
        ec "$f"
    fi
}

function args-nochromefile() {
    local arg
    out=()
    for arg in "$@"
    do
        out+="$(file-uri2unix "$arg")" @TRET
    done
}

function rpargs() {
    doc 'Converts all existent paths in args to their abs path. Outputs both in NUL-delimited stdout and $out.'

    unset out

    local i args
    args=()
    for i in "$@"
    do
        test -e "$i" && args+="$(grealpath --canonicalize-existing -- "$i")" || args+="$i"
    done
    out=( "${args[@]}" )
    re "printf '%s\0'" "$args[@]"
}
function opts-urls() {
    doc "Partitions the arguments into (global variables) |urls| and |opts| (everything else)."
    
    opts=()
    urls=()
    local i
    for i in "$@" ; do
        if match-url2 "$i" ; then
            urls+="$i"
        else
            opts+="$i"
        fi
    done
}
##
function bool() {
    local i="${1:l}"

    if [[ "${i}" == (n|no|0|false) ]] ; then
        return 1
    else
        test -n "${i}"
        return $?
    fi
}

function bool-ask {
    local i="${1:l}" ask_args=("${@[2,-1]}")

    if [[ "${i}" == (a|ask) ]] ; then
        ask "${ask_args[@]}"
        return $?
    else
        bool "${i}"
        return $?
    fi
}
##
