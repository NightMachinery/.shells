## Public-safe file helpers shared by plugins and the full local stack.
function mkdir-m {
    local d
    for d in "$@" ; do
        command mkdir -p -- "$d" || return $?
    done
}

function trs-rm {
    local paths=("$@")

    local p
    for p in "${paths[@]}" ; do
        if test -e "$p" ; then
            if (( ${+functions[icat-maybe]} )) ; then
                icat-maybe "$p"
            fi

            if (( ${+functions[reval-ec]} )) ; then
                reval-ec command rm -rf -- "$p" @RET
            else
                command rm -rf -- "$p" || return $?
            fi
        fi
    done
}
