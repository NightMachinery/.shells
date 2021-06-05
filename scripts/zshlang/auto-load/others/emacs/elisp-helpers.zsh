function sly-doc-oneline() {
    local doc="$1"

    doc="$(ecn "$doc" | gsed '4,$p' | gtr $'\n' " ")"

    if [[ "$doc" =~ '\s*Documentation:\s+(.*)' ]] ; then
        doc="$match[1]"

    fi

    ecn "${doc[1,100]}"
    ## tests:
    # - equalp has docs
    ##
}
##
function wh-docstring() {
    local s="$1"
    local res
    res="$({
    if (( ${+functions[$s]} )) ; then
        ecn $'fn\n'"${functions[$s]}"
    elif (( ${+aliases[$s]} )) ; then
        ecn $'alias\n'"${aliases[$s]}"
    elif (( ${+commands[$s]} )) ; then
        ecn "${commands[$s]}"
    else
        local v
        if v="$(serr typeset -p "$s")" && [[ "$v" =~ '[^=]+=(.*)' ]] ; then
          ecn "${match[1]}"
        fi
    fi
    } | sd '^\s*(\\?noglob)?\s+' '' | prefixer -o '; ' --skip-empty)"

    if test -z "$res" ; then
        ## disabled as it was unnecessary
        # local s2
        # s2="${s%+}" # 'opts+' -> 'opts'
        # if [[ "$s2" != "$s" ]] ; then
        #     wh-docstring "$s2"
        #     return $?
        # fi
        ##
    else
        ecn "${res[1,80]}"
    fi
}
##
